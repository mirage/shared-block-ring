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

(** A producer/consumer ring on top of a shared block device. The producer may
    push variable-sized items (if there is enough space) and the consumer may
    then pop the items. Items are pushed and popped atomically. There should
    be at-most-one producer and at-most-one consumer at any point in time.
    Since block devices have no built-in signalling mechanisms, it is up to
    the client to either poll for updates or implement another out-of-band
    signalling mechanism. *)

module Producer(B: S.BLOCK): sig
  type t

  val create: B.t -> [ `Ok of t | `Error of string ] Lwt.t
  (** [create blockdevice] initialises a shared ring on top of [blockdevice]
      where we will be able to [push] variable-sized items. *)

  val push: t -> Cstruct.t -> [ `Ok of unit | `TooBig | `Retry | `Error of string ] Lwt.t
  (** [push t item] pushes [item] onto the ring [t].
      [`Ok ()] means the update has been safely written to the block device.
      [`TooBig] means the item is too big for the ring: we adopt the convention
        that items must be written to the ring in one go
      [`Retry] means that the item should fit but there is temporarily not
        enough space in the ring. The client should retry later. *)
  end

module Consumer(B: S.BLOCK): sig
  type t

  val attach: B.t -> [ `Ok of t | `Error of string ] Lwt.t
  (** [create blockdevice] attaches to a previously-created shared ring on top
      of [blockdevice]. *)

  type position with sexp_of
  (** A position within the ring *)

  val pop: t -> [ `Ok of position * Cstruct.t | `Retry | `Error of string ] Lwt.t
  (** [pop t] returns a pair [(position * item)] where [item] is the next
      item on the ring. Repeated calls to [pop t] will return the same [item].
      To indicate that the item has been processed, call [advance position].
      [`Retry] means there is no item available at the moment and the client should
      try again later. *)

  val advance: t -> position -> [ `Ok of unit | `Error of string ] Lwt.t
  (** [advance t position] marks the item associated with [position] from a
      previous call to [pop] as having been fully processed. When this thread
      completes future calls to [pop] will return the next item. *)
end
