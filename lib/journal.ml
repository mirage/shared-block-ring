open Lwt
open Sexplib.Std

module Alarm(Time: S.TIME)(Clock: S.CLOCK) = struct
  type t = {
    mutable wake_up_at: float;
    mutable thread: unit Lwt.t option;
    m: Lwt_mutex.t;
    c: unit Lwt_condition.t;
    mutable wake_up: bool;
  }

  let create () =
    let wake_up_at = 0. in
    let thread = None in
    let m = Lwt_mutex.create () in
    let c = Lwt_condition.create () in
    let wake_up = false in
    { wake_up_at; thread; m; c; wake_up }

  let rec next t =
    if t.wake_up then begin
      t.wake_up <- false;
      return ()
    end else begin
      Lwt_condition.wait t.c
      >>= fun () ->
      next t
    end

  let rec countdown t =
    let now = Clock.time () in
    Time.sleep (t.wake_up_at -. now)
    >>= fun () ->
    let now = Clock.time () in
    if now >= t.wake_up_at then begin
      t.thread <- None;
      t.wake_up <- true;
      Lwt_condition.signal t.c ();
      return ()
    end else countdown t

  let reset t for_how_long =
    let now = Clock.time () in
    let new_deadline = now +. for_how_long in
    let old_deadline = t.wake_up_at in
    t.wake_up_at <- new_deadline;
    match t.thread with
    | None ->
      t.thread <- Some (countdown t)
    | Some thread ->
      if new_deadline < old_deadline then begin
        Lwt.cancel thread;
        t.thread <- Some (countdown t)
      end (* otherwise it'll keep sleeping *)
end

module Make
  (Log: S.LOG)
  (Block: S.BLOCK)
  (Time: S.TIME)
  (Clock: S.CLOCK)
  (Op: S.CSTRUCTABLE) = struct

  open Log

  module R = Ring.Make(Log)(Block)(Op)
  open R

  module Alarm = Alarm(Time)(Clock)

  type error = [ `Msg of string ]
  (*BISECT-IGNORE-BEGIN*)
  let pp_error fmt = function
    | `Msg x -> Format.pp_print_string fmt x
  let error_to_msg = function
    | `Ok x -> `Ok x
    | `Error (`Msg x) -> `Error (`Msg x)
  let open_error = function
    | `Ok x -> `Ok x
    | `Error (`Msg x) -> `Error (`Msg x)
  type 'a result = ('a, error) Result.t
  (*BISECT-IGNORE-END*)

  type waiter = unit -> unit Lwt.t

  type t = {
    p: Producer.t;
    c: Consumer.t;
    filename: Block.t;
    cvar: unit Lwt_condition.t;
    mutable data_available: bool;
    mutable please_shutdown: bool;
    mutable shutdown_complete: bool;
    mutable consumed: Consumer.position option;
    perform: Op.t list -> (unit, error) Result.t Lwt.t;
    alarm: Alarm.t;
    m: Lwt_mutex.t;
    flush_interval: float;
    (* Internally handle `Error `Retry by sleeping on the cvar.
       All other errors are fatal. *)
    bind: 'a 'b 'c 'd. 
      (unit -> (('a, [< R.Consumer.error] as 'd) Result.t Lwt.t))
      -> ('a -> ('b, [> R.Consumer.error] as 'c) Result.t Lwt.t)
      -> ('b, [> R.Consumer.error] as 'c) Result.t Lwt.t
  }

  let perform t items () =
    Lwt.catch
      (fun () -> t.perform items) 
      (fun e ->
         let msg = Printexc.to_string e in
         return (`Error (`Msg msg))
      ) >>= function
    | `Ok x -> return (`Ok x)
    | `Error (`Msg x) ->
      error "Failed to process journal item: %s" x;
      return (`Error (`Msg x))

  let replay t () =
    let (>>|=) = t.bind in
    t.data_available <- false;
    Consumer.fold ~f:(fun x y -> x :: y) ~t:t.c ~init:[]
    >>|= fun (position, items) ->
    (* Note we want to apply the items in the original order *)
    let items = List.rev items in
    info "There are %d items in the journal to replay" (List.length items);
    perform t items
    >>|= fun () ->
    Consumer.advance ~t:t.c ~position
    >>|= fun () ->
    t.consumed <- Some position;
    (* wake up anyone stuck in a `Retry loop *)
    Lwt_condition.broadcast t.cvar ();
    return (`Ok ())

  let start ?(name="unknown journal") ?(client="unknown") ?(flush_interval=0.) filename perform =
    let (>>|=) fn f = fn () >>= function
    | `Error `Retry -> return (`Error (`Msg "start: received `Retry"))
    | `Error `Suspended -> return (`Error (`Msg "start: received `Suspended"))
    | `Error (`Msg m) -> return (`Error (`Msg m))
    | `Error x -> return (`Error x)
    | `Ok x -> f x in
    (* If the ring doesn't exist, create it *)
    ( fun () -> Consumer.attach ~queue:name ~client ~disk:filename ()
      >>= function
      | `Error (`Msg _) ->
        Producer.create ~disk:filename
        >>|= fun () ->
        return (`Ok ())
      | _ ->
        return (`Ok ()) ) >>|= fun () ->
    Consumer.attach ~queue:name ~client ~disk:filename
    >>|= fun c ->
    Producer.attach ~queue:name ~client ~disk:filename 
    >>|= fun p ->
    let please_shutdown = false in
    let shutdown_complete = false in
    let cvar = Lwt_condition.create () in
    let consumed = None in
    let m = Lwt_mutex.create () in
    let data_available = true in
    let alarm = Alarm.create () in
    let rec bind fn f = fn () >>= function
      | `Error `Suspended -> return (`Error (`Msg "Ring is suspended"))
      | `Error (`Msg x) -> return (`Error (`Msg x))
      | `Error `Retry ->
        (* If we're out of space then allow the journal to replay
           immediately. *)
        Alarm.reset alarm 0.;
        Lwt_condition.wait cvar
        >>= fun () ->
        bind fn f
      | `Ok x -> f x in
    let t = { p; c; filename; please_shutdown; shutdown_complete; cvar;
              consumed; perform; m; data_available; bind; alarm;
              flush_interval } in
    let (>>|=) = t.bind in
    replay t
    >>|= fun () ->
    (* Run a background thread processing items from the journal *)
    let (_: (unit, R.Consumer.error) Result.t Lwt.t) =
      let rec forever () =
        ( if t.data_available
          then return ()
          else Lwt_condition.wait t.cvar )
        >>= fun () ->
        if t.please_shutdown then begin
          t.shutdown_complete <- true;
          Lwt_condition.broadcast t.cvar ();
          return (`Ok ())
        end else begin
          (* This allows us to wait for batching *)
          Alarm.next alarm
          >>= fun () ->
          (* If we fail to process an item, we can't make progress
             but we can keep trying and keep responding to shutdown
             requests. *)
          replay t ()
          >>= fun _ ->
          forever ()
        end in
      forever () in
    return (`Ok t)
  let start ?name ?client ?flush_interval filename perform =
    start ?name ?client ?flush_interval filename perform
    >>= fun x ->
    return (R.Consumer.error_to_msg x)

  let shutdown t =
    t.please_shutdown <- true;
    Lwt_condition.broadcast t.cvar ();
    let rec loop () =
      if t.shutdown_complete
      then return ()
      else
        Lwt_condition.wait t.cvar
        >>= fun () ->
        loop () in
    loop ()
    >>= fun () ->
    Consumer.detach t.c 

  let rec push t item =
    let (>>|=) = t.bind in
    if t.please_shutdown
    then return (`Error (`Msg "journal shutdown in progress"))
    else begin
      Producer.push ~t:t.p ~item
      >>|= fun position ->
      Producer.advance ~t:t.p ~position
      >>|= fun () ->
      t.data_available <- true;
      Lwt_condition.broadcast t.cvar ();
      (* If the ring is becoming full, we want to flush.
         Otherwise we reset the alarm timer. *)
      Alarm.reset t.alarm t.flush_interval;
      (* Some clients want to know when the item has been processed
         i.e. when the consumer is > position *)
      let has_consumed () = match t.consumed with
        | None -> false
        | Some c ->
          begin match Consumer.compare c position with
          | `GreaterThan | `Equal -> true
          | `LessThan -> false
          end in
        let rec wait () =
          if has_consumed ()
          then return ()
          else
            Lwt_condition.wait t.cvar
            >>= fun () ->
            wait () in
        return (`Ok wait)
    end
  let push t op =
    Lwt_mutex.with_lock t.m (fun () ->
      push t op
      >>= fun ret ->
      return (R.Consumer.error_to_msg ret)
    )
end
