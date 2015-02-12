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
open Lwt

let connect filename =
  Block.connect filename
  >>= function
  | `Error _ -> return (`Error (Printf.sprintf "Block.connect %s failed" filename))
  | `Ok x -> return (`Ok x)

module R = Shared_block.Ring.Make(Block)

module Producer = struct

  type disk = string

  type t = {
    bp: R.Producer.t;
    mutable disk: Block.t option;
  }
  type position = R.Producer.position with sexp_of
  let compare = R.Producer.compare

  let create ~disk:filename () =
    connect filename
    >>= function
    | `Error x -> return (`Error x)
    | `Ok disk ->
    R.Producer.create ~disk ()
    >>= function
    | `Error x ->
      Block.disconnect disk
      >>= fun () ->
      return (`Error x)
    | `Ok () ->
      Block.disconnect disk
      >>= fun () ->
      return (`Ok ())

  let attach ~disk:filename () =
    connect filename
    >>= function
    | `Error x -> return (`Error x)
    | `Ok disk ->
    R.Producer.attach ~disk ()
    >>= function
    | `Error x ->
      Block.disconnect disk
      >>= fun () ->
      return (`Error x)
    | `Ok bp ->
      return (`Ok { bp; disk = Some disk })

  let detach t = match t.disk with
    | Some disk ->
      Block.disconnect disk
      >>= fun () ->
      t.disk <- None;
      return ()
    | None ->
      return ()

  let push ~t ~item () = match t.disk with
  | None -> return (`Error "Not attached")
  | Some _ -> R.Producer.push ~t:t.bp ~item ()

  let advance ~t ~position () = match t.disk with
  | None -> return (`Error "Not attached")
  | Some _ -> R.Producer.advance ~t:t.bp ~position ()

end

module Consumer = struct

  type disk = string

  type t = {
    bc: R.Consumer.t;
    mutable disk: Block.t option;
  }
  type position = R.Consumer.position with sexp_of
  let compare = R.Consumer.compare

  let attach ~disk:filename () =
    connect filename
    >>= function
    | `Error x -> return (`Error x)
    | `Ok disk ->
    R.Consumer.attach ~disk ()
    >>= function
    | `Error x ->
      Block.disconnect disk
      >>= fun () ->
      return (`Error x)
    | `Ok bc ->
      return (`Ok { bc; disk = Some disk })

  let detach t = match t.disk with
    | Some disk ->
      Block.disconnect disk
      >>= fun () ->
      t.disk <- None;
      return ()
    | None ->
      return ()

  let pop ~t ?from () = match t.disk with
  | None -> return (`Error "Not attached")
  | Some _ -> R.Consumer.pop ~t:t.bc ?from ()
  let fold ~f ~t ?from ~init () = match t.disk with
  | None -> return (`Error "Not attached")
  | Some _ -> R.Consumer.fold ~f ~t:t.bc ?from ~init ()
  let advance ~t ~position () = match t.disk with
  | None -> return (`Error "Not attached")
  | Some _ -> R.Consumer.advance ~t:t.bc ~position ()
end
