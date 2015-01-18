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



module Producer(B: S.BLOCK): sig
  type t

  val create: B.t -> [ `Ok of t | `Error of string ] Lwt.t
  (** [create blockdevice] initialises a shared ring on top of [blockdevice]
      where we will be able to [push] variable-sized items. *)

  val push: t -> Cstruct.t -> [ `Ok of unit | `TooBig | `Retry | `Error of string ] Lwt.t
  (** [push t item] pushes [item] onto the ring [t], failing with [`TooBig] if
      the item is too big to ever fit (we adopt the convention that items must
      be written to the ring in one go) and [`Retry] means that there is
      temporarily not enough space i.e. retry later when the consumer has
      consumed some. *)
  end

module Consumer(B: S.BLOCK): sig
  type t

  val create: B.t -> [ `Ok of t | `Error of string ] Lwt.t
  val pop: t -> [ `Ok of Int64.t * Cstruct.t | `Retry | `Error of string ] Lwt.t
  val set_consumer: t -> Cstruct.uint64 -> [ `Ok of unit | `Error of string ] Lwt.t
end
