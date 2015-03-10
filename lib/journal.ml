open Lwt
open Sexplib.Std

module Make
  (Log: S.LOG)
  (Block: S.BLOCK)
  (Op: S.CSTRUCTABLE) = struct

  open Log

  module R = Ring.Make(Log)(Block)(Op)
  open R

  type t = {
    p: Producer.t;
    c: Consumer.t;
    filename: Block.t;
    cvar: unit Lwt_condition.t;
    mutable data_available: bool;
    mutable please_shutdown: bool;
    mutable shutdown_complete: bool;
    mutable consumed: Consumer.position option;
    perform: Op.t list -> unit Lwt.t;
    m: Lwt_mutex.t;
  }

  let replay t =
    t.data_available <- false;
    Consumer.fold ~f:(fun x y -> x :: y) ~t:t.c ~init:[] ()
    >>= function
    | `Error msg ->
       error "Error replaying the journal, cannot continue: %s" msg;
       Consumer.detach t.c
       >>= fun () ->
       fail (Failure msg)
    | `Ok (position, items) ->
       info "There are %d items in the journal to replay" (List.length items);
       Lwt.catch
         (fun () -> t.perform items) 
         (fun e ->
            error "Failed to process journal item: %s" (Printexc.to_string e);
            fail e
         )
       >>= fun () ->
       ( Consumer.advance ~t:t.c ~position ()
         >>= function
         | `Error msg ->
           error "In replay, failed to advance consumer: %s" msg;
           fail (Failure msg)
         | `Ok () ->
           t.consumed <- Some position;
           (* wake up anyone stuck in a `Retry loop *)
           Lwt_condition.broadcast t.cvar ();
           return () )

  let start filename perform =
    ( Consumer.attach ~disk:filename ()
      >>= function
      | `Error msg ->
        ( Producer.create ~disk:filename ()
          >>= function
          | `Error msg ->
            fail (Failure msg)
          | `Ok () ->
            ( Consumer.attach ~disk:filename ()
              >>= function
              | `Error msg ->
                fail (Failure msg)
              | `Ok c ->
                return c )
        )
      | `Ok x ->
        return x
    ) >>= fun c ->
    ( Producer.attach ~disk:filename ()
      >>= function
      | `Error msg ->
        fail (Failure msg)
      | `Ok p ->
        return p
    ) >>= fun p ->
    let please_shutdown = false in
    let shutdown_complete = false in
    let cvar = Lwt_condition.create () in
    let consumed = None in
    let m = Lwt_mutex.create () in
    let data_available = true in
    let t = { p; c; filename; please_shutdown; shutdown_complete; cvar;
              consumed; perform; m; data_available } in
    replay t
    >>= fun () ->
    (* Run a background thread processing items from the journal *)
    let (_: unit Lwt.t) =
      let rec forever () =
        ( if t.data_available
          then return ()
          else Lwt_condition.wait t.cvar )
        >>= fun () ->
        if t.please_shutdown then begin
          t.shutdown_complete <- true;
          Lwt_condition.broadcast t.cvar ();
          return ()
        end else begin
          replay t
          >>= fun () ->
          forever ()
        end in
      forever () in
    return t

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
    if t.please_shutdown
    then fail (Failure "journal shutdown in progress")
    else begin
      Producer.push ~t:t.p ~item ()
      >>= function
      | `Retry ->
         info "journal is full; waiting for a notification";
         Lwt_condition.wait t.cvar
         >>= fun () ->
         push t item
      | `TooBig ->
         error "journal is too small to receive item of size %d bytes" (Cstruct.len (Op.to_cstruct item));
         fail (Failure "journal too small")
      | `Suspend ->
         error "Failed to write item to journal: ring is suspended";
         (* Note: should never happen because we never call suspend *)
         fail (Failure "suspended")
      | `Error msg ->
         error "Failed to write item to journal: %s" msg;
         fail (Failure msg)
      | `Ok position ->
         ( Producer.advance ~t:t.p ~position ()
           >>= function
           | `Error msg ->
             error "Failed to advance producer pointer: %s" msg;
             fail (Failure msg)
           | `Ok () ->
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
             return wait )
    end
  let push t op = Lwt_mutex.with_lock t.m (fun () -> push t op)
end
