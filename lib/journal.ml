open Lwt
open Sexplib.Std

module Make
  (Log: S.LOG)
  (Block: S.BLOCK)
  (Op: S.CSTRUCTABLE) = struct

  open Log

  module R = Ring.Make(Log)(Block)(Op)
  open R

  type error = [ `Retry | `Suspended | `Msg of string ]

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
    m: Lwt_mutex.t;
    (* Internally handle `Error `Retry by sleeping on the cvar.
       All other errors are fatal. *)
    bind: 'a 'b 'c 'd. 
      (unit -> (('a, [< error] as 'd) Result.t Lwt.t))
      -> ('a -> ('b, [> error] as 'c) Result.t Lwt.t)
      -> ('b, [> error] as 'c) Result.t Lwt.t
  }

  let perform t items () =
    Lwt.catch
      (fun () -> t.perform items) 
      (fun e ->
         let msg = Printexc.to_string e in
         error "Failed to process journal item: %s" msg;
         return (`Error (`Msg msg))
      )

  let replay t () =
    let (>>|=) = t.bind in
    t.data_available <- false;
    Consumer.fold ~f:(fun x y -> x :: y) ~t:t.c ~init:[]
    >>|= fun (position, items) ->
    info "There are %d items in the journal to replay" (List.length items);
    perform t items
    >>|= fun () ->
    Consumer.advance ~t:t.c ~position
    >>|= fun () ->
    t.consumed <- Some position;
    (* wake up anyone stuck in a `Retry loop *)
    Lwt_condition.broadcast t.cvar ();
    return (`Ok ())

  let start filename perform =
    let (>>|=) fn f = fn () >>= function
    | `Error `Retry -> return (`Error (`Msg "start: received `Retry"))
    | `Error `Suspended -> return (`Error (`Msg "start: received `Suspended"))
    | `Error (`Msg m) -> return (`Error (`Msg m))
    | `Error x -> return (`Error x)
    | `Ok x -> f x in

    (* If the ring doesn't exist, create it *)
    ( fun () -> Consumer.attach ~disk:filename ()
      >>= function
      | `Error (`Msg _) ->
        Producer.create ~disk:filename
        >>|= fun () ->
        return (`Ok ())
      | _ ->
        return (`Ok ()) ) >>|= fun () ->

    Consumer.attach ~disk:filename
    >>|= fun c ->
    Producer.attach ~disk:filename 
    >>|= fun p ->
    let please_shutdown = false in
    let shutdown_complete = false in
    let cvar = Lwt_condition.create () in
    let consumed = None in
    let m = Lwt_mutex.create () in
    let data_available = true in
    let rec bind fn f = fn () >>= function
      | `Error `Suspended -> return (`Error (`Msg "Ring is suspended"))
      | `Error (`Msg x) -> return (`Error (`Msg x))
      | `Error `Retry ->
        Lwt_condition.wait cvar
        >>= fun () ->
        bind fn f
      | `Ok x -> f x in
    let t = { p; c; filename; please_shutdown; shutdown_complete; cvar;
              consumed; perform; m; data_available; bind } in
    let (>>|=) = t.bind in
    replay t
    >>|= fun () ->
    (* Run a background thread processing items from the journal *)
    let (_: (unit, error) Result.t Lwt.t) =
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
          replay t
          >>|= fun () ->
          forever ()
        end in
      forever () in
    return (`Ok t)

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
    then fail (Failure "journal shutdown in progress")
    else begin
      Producer.push ~t:t.p ~item
      >>|= fun position ->
      Producer.advance ~t:t.p ~position
      >>|= fun () ->
      t.data_available <- true;
      Lwt_condition.broadcast t.cvar ();
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
  let push t op = Lwt_mutex.with_lock t.m (fun () -> push t op)
end
