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

type msg = [ `Msg of string ]

let get_ok = function
| `Ok x -> x
| `Error _ -> raise (Invalid_argument "get_ok encountered an `Error")

let get_error = function
| `Error x -> x
| `Ok _ -> raise (Invalid_argument "get_error encountered an `Ok")
