(*
 * Copyright (C) 2013-2015 Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Sexplib.Std
open Lwt

(* The format will be:
   sector 0: signature (to help us know if the data is supposed to be valid
   sector 1: producer pointer (as a byte offset)
   sector 2: consumer pointer (as a byte offset)
   Each data item shall be prefixed with a 4-byte length, followed by
   the payload and padded to the next sector boundary. *)

let sector_signature = 0L
let sector_producer  = 1L
let sector_consumer  = 2L
let sector_data      = 3L

let minimum_size_sectors = Int64.add sector_data 1L

let magic = Printf.sprintf "mirage shared-block-device 1.0"

let zero buf =
  for i = 0 to Cstruct.len buf - 1 do
    Cstruct.set_uint8 buf i 0
  done

let alloc sector_size =
  let page = Io_page.(to_cstruct (get 1)) in
  let sector = Cstruct.sub page 0 sector_size in
  sector

module Common(Log: S.LOG)(B: S.BLOCK) = struct
  type error = [ `Retry | `Suspended | `Msg of string ]

  let (>>=) m f = Lwt.bind m (function
    | `Ok x -> f x
    | `Error x -> return (`Error x)
    )

  (* Convert the block errors *)
  let ( >>*= ) m f =
    let open Lwt in
    m >>= function
    | `Ok x -> f x
    (* These fatal errors from the block layer indicate a software bug:
       code missing, device openeed read-only, or device prematurely
       disconnected. The best we can do is to propagate an unhandleable
       error upwards to where it can be logged, and the process / thread
       can exit. *)
    | `Error `Unimplemented -> return (`Error (`Msg "BLOCK function is unimplemented"))
    | `Error `Is_read_only -> return (`Error (`Msg "BLOCK device is read-only"))
    | `Error `Disconnected -> return (`Error (`Msg "BLOCK device has already disconnected"))
    (* This is a bad error which includes both permanent failures and
       I/O errors which could be recoverable. We will assume the error
       can be recovered and invite the upper layer to retry. *)
    | `Error (`Unknown x) -> return (`Error `Retry)

  let initialise device sector =
    (* Initialise the producer and consumer before writing the magic
       in case we crash in the middle *)
    zero sector;
    B.write device sector_producer [ sector ] >>*= fun () ->
    B.write device sector_consumer [ sector ] >>*= fun () ->
    Cstruct.blit_from_string magic 0 sector 0 (String.length magic);
    B.write device sector_signature [ sector ] >>*= fun () ->
    return (`Ok ())

  let is_initialised device sector =
    (* check for magic, initialise if not found *)
    B.read device sector_signature [ sector ] >>*= fun () ->
    let magic' = String.make (String.length magic) '\000' in
    Cstruct.blit_to_string sector 0 magic' 0 (String.length magic');
    return (`Ok (magic = magic'))

  let create device info sector =
    if info.B.size_sectors < minimum_size_sectors
    then return (`Error (`Msg (Printf.sprintf "The block device is too small for a ring; the minimum size is %Ld sectors" minimum_size_sectors)))
    else
      is_initialised device sector >>= function
      | true -> return (`Ok ())
      | false -> initialise device sector

  let read sector_offset device sector =
    B.read device sector_offset [ sector ] >>*= fun () ->
    return (`Ok ())

  let write sector_offset device sector =
    B.write device sector_offset [ sector ] >>*= fun () ->
    return (`Ok ())

  type producer = {
    producer: int64;
    suspend_ack: bool;
  }

  type consumer = {
    consumer: int64;
    suspend: bool;
  }

  let get_producer device sector =
    B.read device sector_producer [ sector ] >>*= fun () ->
    let producer = Cstruct.LE.get_uint64 sector 0 in
    let suspend_ack = Cstruct.get_uint8 sector 8 = 1 in
    return (`Ok { producer; suspend_ack })

  let set_producer device sector v =
    zero sector;
    Cstruct.LE.set_uint64 sector 0 v.producer;
    Cstruct.set_uint8 sector 8 (if v.suspend_ack then 1 else 0);
    B.write device sector_producer [ sector ] >>*= fun () ->
    return (`Ok ())

  let get_consumer device sector =
    B.read device sector_consumer [ sector ] >>*= fun () ->
    let consumer = Cstruct.LE.get_uint64 sector 0 in
    let suspend = Cstruct.get_uint8 sector 8 = 1 in
    return (`Ok { consumer; suspend })

  let set_consumer device sector v =
    zero sector;
    Cstruct.LE.set_uint64 sector 0 v.consumer;
    Cstruct.set_uint8 sector 8 (if v.suspend then 1 else 0);
    B.write device sector_consumer [ sector ] >>*= fun () ->
    return (`Ok ())

  let get_data_sectors info = Int64.(sub info.B.size_sectors sector_data)

  let get_sector_and_offset info byte_offset =
    let sector = Int64.(div byte_offset (of_int info.B.sector_size)) in
    let offset = Int64.(to_int (rem byte_offset (of_int info.B.sector_size))) in
    (sector,offset)

  type position = int64 with sexp_of

  let compare (a: position) (b: position) =
    if a < b then `LessThan
    else if a > b then `GreaterThan
    else `Equal

end

module Make(Log: S.LOG)(B: S.BLOCK)(Item: S.CSTRUCTABLE) = struct

module Producer = struct
  module C = Common(Log)(B)

  type position = C.position with sexp_of
  let compare = C.compare
  type item = Item.t
  type error = C.error

  type t = {
    disk: B.t;
    info: B.info;
    mutable producer: C.producer; (* cache of the last value we wrote *)
    sector: Cstruct.t; (* a scratch buffer of size 1 sector *)
    mutable attached: bool;
  }

  let create ~disk:disk () =
    B.get_info disk >>= fun info ->
    let open C in
    let sector = alloc info.B.sector_size in
    create disk info sector >>= fun () ->
    return (`Ok ())
  
  let attach ~disk:disk () =
    B.get_info disk >>= fun info ->
    let open C in
    let sector = alloc info.B.sector_size in
    is_initialised disk sector >>= function
    | false -> return (`Error (`Msg "block ring has not been initialised"))
    | true ->
      get_producer disk sector >>= fun producer ->
      return (`Ok {
        disk;
        info;
        producer;
        sector;
        attached = true;
        })

  let detach t =
    t.attached <- false;
    return ()

  let must_be_attached t f =
    if not t.attached
    then return (`Error (`Msg "Ring has been detached and cannot be used"))
    else f ()

  let ok_to_write t needed_bytes =
    let open C in
    get_consumer t.disk t.sector >>= fun c ->
    ( if c.suspend <> t.producer.suspend_ack then begin
        let producer = { t.producer with suspend_ack = c.suspend } in
        set_producer t.disk t.sector producer >>= fun () ->
        t.producer <- producer;
        return (`Ok ())
      end else return (`Ok ()) ) >>= fun () ->
    if c.suspend
    then return (`Error `Suspended)
    else
      let used = Int64.sub t.producer.producer c.consumer in
      let total = Int64.(mul (of_int t.info.B.sector_size) (C.get_data_sectors t.info)) in
      let total_sectors = get_data_sectors t.info in
      if Int64.(mul total_sectors (of_int t.info.B.sector_size)) < needed_bytes
      then return (`Error (`Msg (Printf.sprintf "The ring is too small for a message of size %Ld bytes" needed_bytes)))
      else if Int64.sub total used < needed_bytes
      then return (`Error `Retry)
      else return (`Ok ())

  let state t =
    let open Lwt in
    ok_to_write t 0L
    >>= function
    | `Error `Suspended -> return (`Ok `Suspended)
    | `Error `Retry -> return (`Error (`Msg "Internal error: it should always be ok to write 0 bytes"))
    | `Error x -> return (`Error x)
    | `Ok () -> return (`Ok `Running)

  let read_modify_write t sector fn =
    let open C in
    let total_sectors = get_data_sectors t.info in
    let realsector = Int64.(add sector_data (rem sector total_sectors)) in
    read realsector t.disk t.sector >>= fun () ->
    let result = fn () in
    write realsector t.disk t.sector >>= fun () ->
    return (`Ok result)

  let unsafe_write (t:t) item =
    let open C in
    (* add a 4 byte header of size, and round up to the next 4-byte offset *)
    let needed_bytes = Int64.(logand (lognot 3L) (add 7L (of_int (Cstruct.len item)))) in
    let first_sector = Int64.(div t.producer.producer (of_int t.info.B.sector_size)) in
    let first_offset = Int64.(to_int (rem t.producer.producer (of_int t.info.B.sector_size))) in

    (* Do first sector. We know that we'll always be able to fit in the length header into
       the first page as it's only a 4-byte integer and we're padding to 4-byte offsets. *)
    read_modify_write t first_sector (fun () ->
      (* Write the header and anything else we can *)
      Cstruct.LE.set_uint32 t.sector first_offset (Int32.of_int (Cstruct.len item));
      if first_offset + 4 = t.info.B.sector_size
      then item (* We can't write anything else, so just return the item *)
      else begin
        let this = min (t.info.B.sector_size - first_offset - 4) (Cstruct.len item) in
        Cstruct.blit item 0 t.sector (first_offset + 4) this;
        Cstruct.shift item this
      end) >>= fun remaining ->

    let rec loop sector remaining =
      if Cstruct.len remaining = 0
      then return (`Ok ())
      else begin
        read_modify_write t sector (fun () ->
          let this = min t.info.B.sector_size (Cstruct.len remaining) in
          let frag = Cstruct.sub t.sector 0 this in
          Cstruct.blit remaining 0 frag 0 (Cstruct.len frag);
          Cstruct.shift remaining this) >>= fun remaining ->
        loop (Int64.succ sector) remaining
      end in
    loop (Int64.succ first_sector) remaining >>= fun () ->
      (* Write the payload before updating the producer pointer *)
    let new_producer = Int64.add t.producer.producer needed_bytes in
    return (`Ok new_producer)

  let advance ~t ~position:new_producer () =
    must_be_attached t
      (fun () ->
        let open C in
        let producer = { t.producer with producer = new_producer } in
        set_producer t.disk t.sector producer >>= fun () ->
        t.producer <- producer;
        return (`Ok ())
      )

  let push ~t ~item () =
    must_be_attached t
      (fun () ->
        let item = Item.to_cstruct item in
        (* every item has a 4 byte header *)
        let needed_bytes = Int64.(add 4L (of_int (Cstruct.len item))) in
        let open C in
        ok_to_write t needed_bytes
        >>= fun () ->
        unsafe_write t item
      )
end

module Consumer = struct
  module C = Common(Log)(B)

  type position = C.position with sexp_of
  let compare = C.compare
  type item = Item.t
  type error = C.error

  type t = {
    disk: B.t;
    info: B.info;
    mutable consumer: C.consumer; (* cache of the last value we wrote *)
    sector: Cstruct.t; (* a scratch buffer of size 1 sector *)
    mutable attached: bool;
  }

  let detach t =
    t.attached <- false;
    return ()

  let must_be_attached t f =
    if not t.attached
    then return (`Error (`Msg "Ring has been detached and cannot be used"))
    else f ()

  let attach ~disk:disk () =
    let open Lwt in
    B.get_info disk >>= fun info ->
    let open C in
    let sector = alloc info.B.sector_size in
    is_initialised disk sector >>= function
    | false -> return (`Error (`Msg "block ring has not been initialised"))
    | true ->
      get_consumer disk sector >>= fun consumer ->
      return (`Ok {
        disk;
        info;
        consumer;
        sector;
        attached = true;
      })

  let suspend (t:t) =
    let open C in
    C.get_producer t.disk t.sector
    >>= fun producer ->
    if t.consumer.C.suspend <> producer.C.suspend_ack
    then return (`Error `Retry)
    else
      let consumer = { t.consumer with C.suspend = true } in
      C.set_consumer t.disk t.sector consumer
      >>= fun () ->
      t.consumer <- consumer;
      return (`Ok ())

  let state t =
    let open Lwt in
    C.get_producer t.disk t.sector
    >>= function
    | `Ok p -> return (`Ok (if p.C.suspend_ack then `Suspended else `Running))
    | `Error x -> return (`Error x)

  let resume (t: t) =
    let open C in
    C.get_producer t.disk t.sector
    >>= fun producer ->
    if t.consumer.C.suspend <> producer.C.suspend_ack
    then return (`Error `Retry)
    else
      let consumer = { t.consumer with C.suspend = false } in
      C.set_consumer t.disk t.sector consumer
      >>= fun () ->
      t.consumer <- consumer;
      return (`Ok ())

  let pop t =
    let open C in
    let total_sectors = get_data_sectors t.info in
    get_producer t.disk t.sector >>= fun producer ->
    let available_bytes = Int64.sub producer.producer t.consumer.consumer in
    if available_bytes <= 0L
    then return (`Error `Retry)
    else begin
      let first_sector,first_offset = get_sector_and_offset t.info t.consumer.consumer in
      read Int64.(add sector_data (rem first_sector total_sectors)) t.disk t.sector >>= fun () ->
      let len = Int32.to_int (Cstruct.LE.get_uint32 t.sector first_offset) in
      let result = Cstruct.create len in
      let this = min len (t.info.B.sector_size - first_offset - 4) in
      let frag = Cstruct.sub t.sector (4 + first_offset) this in
      Cstruct.blit frag 0 result 0 this;
      let rec loop consumer remaining =
        if Cstruct.len remaining = 0
        then return (`Ok ())
        else
          let this = min t.info.B.sector_size (Cstruct.len remaining) in
          let frag = Cstruct.sub remaining 0 this in
          read Int64.(add sector_data (rem consumer total_sectors)) t.disk t.sector >>= fun () ->
          Cstruct.blit t.sector 0 frag 0 this;
          loop (Int64.succ consumer) (Cstruct.shift remaining this) in
      loop (Int64.succ first_sector) (Cstruct.shift result this) >>= fun () ->
      (* Read the payload before updating the consumer pointer *)
      let needed_bytes = Int64.(logand (lognot 3L) (add 7L (of_int (len)))) in
      match Item.of_cstruct result with
      | None -> return (`Error (`Msg (Printf.sprintf "Failed to parse queue item: (%d)[%s]" (Cstruct.len result) (String.escaped (Cstruct.to_string result)))))
      | Some result ->
        return (`Ok (Int64.(add t.consumer.consumer needed_bytes),result))
    end

  let pop ~t ?(from = t.consumer.C.consumer) () =
    must_be_attached t
      (fun () ->
        pop { t with consumer = { t.consumer with C.consumer = from } }
      )

  let rec fold ~f ~t ?(from = t.consumer.C.consumer) ~init:acc () =
    let open Lwt in
    pop ~t ~from ()
    >>= function
    | `Error `Retry -> return (`Ok (from, acc))
    | `Error x -> return (`Error x)
    | `Ok (from, x) -> fold ~f ~t ~from ~init:(f x acc) ()

  let advance ~t ~position:consumer () =
    must_be_attached t
      (fun () ->
        let open C in
        let consumer' = { t.consumer with consumer = consumer } in
        set_consumer t.disk t.sector consumer' >>= fun () ->
        t.consumer <- consumer';
        return (`Ok ())
      )
end
end
