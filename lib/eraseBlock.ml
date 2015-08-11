(*
 * Copyright (C) 2009-2015 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Lwt

let block_error = function
  | `Unknown x -> `Error (`Msg x)
  | `Unimplemented -> `Error (`Msg "unimplemented")
  | `Is_read_only -> `Error (`Msg "device is read-only")
  | `Disconnected -> `Error (`Msg "disconnected")

module IO = struct
  let ( >>= ) m f = m >>= function
  | `Error e -> return (block_error e)
  | `Ok x -> f x
end

let block_size_pages = 1024

module Make(B: S.BLOCK) = struct
  let erase ?(pattern = "This block has been erased by mirage-block-volume/lib/eraseBlock.ml") t =
    let open Lwt in
    B.get_info t
    >>= fun info ->
    let pages = Io_page.get block_size_pages in
    let buffer = Io_page.to_cstruct pages in
    for i = 0 to Cstruct.len buffer - 1 do
      Cstruct.set_char buffer i (pattern.[i mod (String.length pattern)])
    done;
    let open IO in
    let rec loop n =
      if n = info.B.size_sectors
      then return (`Ok ())
      else
        let buffer_in_sectors = Cstruct.len buffer / info.B.sector_size in
        let needed = Int64.to_int (min (Int64.sub info.B.size_sectors n) (Int64.of_int buffer_in_sectors)) in
        let towrite = Cstruct.sub buffer 0 (needed * info.B.sector_size) in
        B.write t n [ towrite ]
        >>= fun () ->
        loop (Int64.(add n (of_int needed))) in
    loop 0L
end
