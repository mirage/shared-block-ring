(*
 * Copyright (C) 2013 Citrix Systems Inc
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

module Common(B: S.BLOCK_DEVICE) = struct

  (* Convert the block errors *)
  let ( >>= ) m f = m >>= function
  | `Ok x -> f x
  | `Error (B.Unknown x) -> return (`Error x)
  | `Error B.Unimplemented -> return (`Error "unimplemented")
  | `Error B.Is_read_only -> return (`Error "is_read_only")
  | `Error B.Disconnected -> return (`Error "disconnected")

  let initialise device sector =
    (* Initialise the producer and consumer before writing the magic
       in case we crash in the middle *)
    zero sector;
    B.write device sector_producer [ sector ] >>= fun () ->
    B.write device sector_consumer [ sector ] >>= fun () ->
    Cstruct.blit_from_string magic 0 sector 0 (String.length magic);
    B.write device sector_signature [ sector ] >>= fun () ->
    return (`Ok ())

  let is_initialised device sector =
    (* check for magic, initialise if not found *)
    B.read device sector_signature [ sector ] >>= fun () ->
    let magic' = String.make (String.length magic) '\000' in
    Cstruct.blit_to_string sector 0 magic' 0 (String.length magic');
    return (`Ok (magic = magic'))

  let create device info sector =
    let open Lwt in
    if info.B.size_sectors < minimum_size_sectors
    then return (`Error (Printf.sprintf "The block device is too small for a ring; the minimum size is %Ld sectors" minimum_size_sectors))
    else
      is_initialised device sector >>= function
      | `Ok true -> return (`Ok ())
      | `Ok false -> initialise device sector
      | `Error x -> return (`Error x)

  let read sector_offset device sector =
    B.read device sector_offset [ sector ] >>= fun () ->
    return (`Ok ())

  let write sector_offset device sector =
    B.write device sector_offset [ sector ] >>= fun () ->
    return (`Ok ())

  let get sector_offset device sector =
    B.read device sector_offset [ sector ] >>= fun () ->
    return (`Ok (Cstruct.LE.get_uint64 sector 0))

  let set sector_offset device sector v =
    zero sector;
    Cstruct.LE.set_uint64 sector 0 v;
    B.write device sector_offset [ sector ] >>= fun () ->
    return (`Ok ())

  let get_producer = get sector_producer
  let get_consumer = get sector_consumer

  let set_producer = set sector_producer
  let set_consumer = set sector_consumer

  let get_data_sectors info = Int64.(sub info.B.size_sectors sector_data)

  (* Expose our new error type to the Producer and Consumer below *)
  let ( >>= ) m f = Lwt.bind m (function
  | `Ok x -> f x
  | `Error x -> return (`Error x))
end

module Producer(B: S.BLOCK_DEVICE) = struct
  module C = Common(B)

  type t = {
    device: B.t;
    info: B.info;
    mutable producer: int64; (* cache of the last value we wrote *)
    mutable consumer: int64; (* cache of the last value we read *)
    sector: Cstruct.t; (* a scratch buffer of size 1 sector *)
  }

  let create device sector =
    B.get_info device >>= fun info ->
    let open C in
    create device info sector >>= fun () ->
    get_producer device sector >>= fun producer ->
    get_consumer device sector >>= fun consumer ->
    return (`Ok {
      device;
      info;
      producer;
      consumer;
      sector;
    })

  let get_free_sectors t =
    let open C in
    get_consumer t.device t.sector >>= fun consumer ->
    let used = Int64.sub t.producer consumer in
    let total = C.get_data_sectors t.info in
    return (`Ok (Int64.sub total used))

  let push t item =
    (* every item has a 4 byte header *)
    let needed_bytes = Int64.(add 4L (of_int (Cstruct.len item))) in
    let open C in
    let total_sectors = get_data_sectors t.info in
    get_free_sectors t >>= fun free_sectors ->
    if Int64.(mul total_sectors (of_int t.info.B.sector_size)) < needed_bytes
    then return (`Ok `TooBig)
    else if Int64.(mul free_sectors (of_int t.info.B.sector_size)) < needed_bytes
    then return (`Ok `Retry)
    else begin
      (* Write the header and the first block *)
      Cstruct.LE.set_uint32 t.sector 0 (Int32.of_int (Cstruct.len item));
      let this = min (t.info.B.sector_size - 4) (Cstruct.len item) in
      let frag = Cstruct.sub t.sector 4 this in
      Cstruct.blit item 0 frag 0 this;
      C.write Int64.(add sector_data (rem t.producer total_sectors)) t.device t.sector >>= fun () ->
      let remaining = Cstruct.shift item this in
      let rec loop producer remaining =
        if Cstruct.len remaining = 0
        then return (`Ok producer)
        else
          let this = min t.info.B.sector_size (Cstruct.len remaining) in
          let frag = Cstruct.sub t.sector 0 this in
          Cstruct.blit remaining 0 frag 0 (Cstruct.len frag);
          C.write Int64.(add sector_data (rem producer total_sectors)) t.device t.sector >>= fun () ->
          loop (Int64.succ producer) (Cstruct.shift remaining this) in
      loop (Int64.succ t.producer) remaining >>= fun producer ->
      (* Write the payload before updating the producer pointer *)
      set_producer t.device t.sector producer >>= fun () ->
      t.producer <- producer;
      return (`Ok `Written)
    end
end

module Consumer(B: S.BLOCK_DEVICE) = struct
  module C = Common(B)

  type t = {
    device: B.t;
    info: B.info;
    mutable producer: int64; (* cache of the last value we read *)
    mutable consumer: int64; (* cache of the last value we wrote *)
    sector: Cstruct.t; (* a scratch buffer of size 1 sector *)
  }

  let create device sector =
    B.get_info device >>= fun info ->
    let open C in
    is_initialised device sector >>= function
    | false -> return (`Error "block ring has not been initialised")
    | true ->
      get_producer device sector >>= fun producer ->
      get_consumer device sector >>= fun consumer ->
      return (`Ok {
        device;
        info;
        producer;
        consumer;
        sector;
      })

  let pop t =
    let open C in
    let total_sectors = get_data_sectors t.info in
    get_producer t.device t.sector >>= fun producer ->
    let available_sectors = Int64.sub producer t.consumer in
    if available_sectors <= 0L
    then return (`Ok `Retry)
    else begin
      read Int64.(add sector_data (rem t.consumer total_sectors)) t.device t.sector >>= fun () ->
      let len = Int32.to_int (Cstruct.LE.get_uint32 t.sector 0) in
      let result = Cstruct.create len in
      let this = min len (t.info.B.sector_size - 4) in
      let frag = Cstruct.sub t.sector 4 this in
      Cstruct.blit frag 0 result 0 this;
      let rec loop consumer remaining =
        if Cstruct.len remaining = 0
        then return (`Ok consumer)
        else
          let this = min t.info.B.sector_size (Cstruct.len remaining) in
          let frag = Cstruct.sub remaining 0 this in
          read Int64.(add sector_data (rem consumer total_sectors)) t.device t.sector >>= fun () ->
          Cstruct.blit t.sector 0 frag 0 this;
          loop (Int64.succ consumer) (Cstruct.shift remaining this) in
      loop (Int64.succ t.consumer) (Cstruct.shift result this) >>= fun consumer ->
      (* Read the payload before updating the consumer pointer *)
      set_consumer t.device t.sector consumer >>= fun () ->
      t.consumer <- consumer;
      return (`Ok (`Read result))
    end
end

