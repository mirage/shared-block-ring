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
  | `Error _ -> fail (Failure (Printf.sprintf "Block.connect %s failed" filename))
  | `Ok x -> return x

module Producer = struct
  module BlockProducer = Block_ring.Producer(Block)

  type t = BlockProducer.t
  type position = BlockProducer.position with sexp_of

  let create filename =
    connect filename
    >>= fun block ->
    BlockProducer.create block

  let attach filename =
    connect filename
    >>= fun block ->
    BlockProducer.attach block

  let push = BlockProducer.push
  let advance = BlockProducer.advance

end

module Consumer = struct
  module BlockConsumer = Block_ring.Consumer(Block)

  type t = BlockConsumer.t
  type position = BlockConsumer.position with sexp_of

  let attach filename =
    connect filename
    >>= fun block ->
    BlockConsumer.attach block

  let pop = BlockConsumer.pop
  let peek = BlockConsumer.peek
  let advance = BlockConsumer.advance
end
