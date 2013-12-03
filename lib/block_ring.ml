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
   the payload. *)

let sector_signature = 0L
let sector_producer  = 1L
let sector_consumer  = 2L

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

  let create device sector =
    let open Lwt in
    is_initialised device sector >>= function
    | `Ok true -> return (`Ok ())
    | `Ok false -> initialise device sector
    | `Error x -> return (`Error x)

  let get sector_offset device sector =
    B.read device sector_offset [ sector ] >>= fun () ->
    return (`Ok (Cstruct.LE.get_uint64 sector 0))

  let set sector_offset device sector v =
    zero sector;
    Cstruct.LE.set_uint64 sector 0 v;
    B.write device sector_offset [ sector ]

  let get_producer = get sector_producer
  let get_consumer = get sector_consumer

  let set_producer = set sector_producer
  let set_consumer = set sector_consumer

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
    producer: int64; (* cache of the last value we wrote *)
    consumer: int64; (* cache of the last value we read *)
    sector: Cstruct.t; (* a scratch buffer of size 1 sector *)
  }

  let create device sector =
    B.get_info device >>= fun info ->
    let open C in
    create device sector >>= fun () ->
    get_producer device sector >>= fun producer ->
    get_consumer device sector >>= fun consumer ->
    return (`Ok {
      device;
      info;
      producer;
      consumer;
      sector;
    })

  let push t item =
    return (`Error "unimplemented")

  let get_free_space t =
    return (`Error "unimplemented")

end

module Consumer(B: S.BLOCK_DEVICE) = struct
  module C = Common(B)

  type t = {
    device: B.t;
    info: B.info;
    producer: int64; (* cache of the last value we read *)
    consumer: int64; (* cache of the last value we wrote *)
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
    return (`Error "unimplemented")

  let get_consumed_space t =
    return (`Error "unimplemented")
end

