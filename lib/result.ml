(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

type ('a, 'b) t = [
| `Ok of 'a 
| `Error of 'b
]

let ( >>= ) m f = match m with
| `Ok x -> f x
| `Error y -> `Error y

let return x = `Ok x
let ok = return
let fail x = `Error x

type msg = [ `Msg of string ]
let msg x = `Msg x

let all xs =
  let rec loop acc = function
  | [] -> return (List.rev acc)
  | `Ok x :: xs -> loop (x :: acc) xs
  | `Error x :: _ -> `Error x in
  loop [] xs

let ok_or_failwith = function
  | `Ok x -> x
  | `Error x -> failwith x
