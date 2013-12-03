(*
 * Copyright (C) 2013 Citrix Inc
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt
open OUnit

module Producer = Block_ring.Producer(Mirage_block.Block)
module Consumer = Block_ring.Consumer(Mirage_block.Block)

let find_unused_file () =
  (* Find a filename which doesn't exist *)
  let rec does_not_exist i =
    let name = Printf.sprintf "%s/mirage-block-test.%d.%d"
      Filename.temp_dir_name (Unix.getpid ()) i in
    if Sys.file_exists name
    then does_not_exist (i + 1)
    else name in
  does_not_exist 0

exception Cstruct_differ

let cstruct_equal a b =
  let check_contents a b =
    try
      for i = 0 to Cstruct.len a - 1 do
        let a' = Cstruct.get_char a i in
        let b' = Cstruct.get_char b i in
        if a' <> b' then raise Cstruct_differ
      done;
      true
    with _ -> false in
      (Cstruct.len a = (Cstruct.len b)) && (check_contents a b)

let interesting_lengths = [
  0; (* possible base case *)
  1; (* easily fits inside a sector with the 4 byte header *)
  512 - 4 - 1;
  512 - 4;
  512 - 4 + 1;
]

(* ring too small *)
(* ring too full *)
(* wrap around works *)

let fill_with_message buffer message =
    for i = 0 to Cstruct.len buffer - 1 do
      Cstruct.set_char buffer i (message.[i mod (String.length message)])
    done

let test_push_pop length () =
  let t =
    let name = find_unused_file () in
    Lwt_unix.openfile name [ Lwt_unix.O_CREAT; Lwt_unix.O_WRONLY ] 0o0666 >>= fun fd ->
    let size = 16384L in
    (* Write the last sector to make sure the file has the intended size *)
    Lwt_unix.LargeFile.lseek fd Int64.(sub size 512L) Lwt_unix.SEEK_CUR >>= fun _ ->
    let sector = Mirage_block.Block.Memory.alloc 512 in
    fill_with_message sector "\xde\xead\xbe\xef";
    Mirage_block.Block.really_write fd sector >>= fun () ->

    let payload = Cstruct.create length in
    let message = "All work and no play makes Dave a dull boy.\n" in
    fill_with_message payload message;

    Mirage_block.Block.connect name >>= function
    | `Error _ -> failwith (Printf.sprintf "Block.connect %s failed" name)
    | `Ok device ->
    Producer.create device (Mirage_block.Block.Memory.alloc 512) >>= function
    | `Error x -> failwith (Printf.sprintf "Producer.create %s" name)
    | `Ok producer ->
    Consumer.create device (Mirage_block.Block.Memory.alloc 512) >>= function
    | `Error x -> failwith (Printf.sprintf "Consumer.create %s" name)
    | `Ok consumer ->
    let rec loop = function
      | 0 -> return ()
      | n ->
        Consumer.pop consumer >>= function
        | `Ok _ | `Error _ -> failwith "empty pop"
        | `Retry ->
        Producer.push producer payload >>= function
        | `Error _ | `TooBig | `Retry -> failwith "push"
        | `Ok () ->
        Consumer.pop consumer >>= function
        | `Error _ | `Retry -> failwith "pop"
        | `Ok buffer ->
        assert_equal ~printer:Cstruct.to_string ~cmp:cstruct_equal payload buffer;
        loop (n - 1) in
    (* push/pop 2 * the number of sectors to guarantee we experience some wraparound *)
    loop Int64.(to_int (mul 2L (div size 512L))) in
  Lwt_main.run t

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
  "Test shared block ring";

  let test_push_pops = List.map (fun length ->
    Printf.sprintf "push pop %d bytes" length >:: (test_push_pop length)
  ) interesting_lengths in

  let suite = "shared-block-ring" >::: [
  ] @ test_push_pops in
  run_test_tt ~verbose:!verbose suite
