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

module Result = struct
  let ( >>= ) m f = m >>= function
  | `Error x -> fail (Failure x)
  | `Ok x -> f x
end

let test_push () =
  let t =
    let name = find_unused_file () in
    Lwt_unix.openfile name [ Lwt_unix.O_CREAT; Lwt_unix.O_WRONLY ] 0o0444 >>= fun fd ->
    let size = Int64.(mul 1024L 1024L) in
    Lwt_unix.LargeFile.lseek fd Int64.(sub size 512L) Lwt_unix.SEEK_CUR >>= fun _ ->
    let message = "All work and no play makes Dave a dull boy.\n" in
    let sector = Mirage_block.Block.Memory.alloc 512 in
    for i = 0 to 511 do
      Cstruct.set_char sector i (message.[i mod (String.length message)])
    done;
    Mirage_block.Block.connect name >>= function
    | `Error _ -> failwith (Printf.sprintf "Block.connect %s failed" name)
    | `Ok device ->
      let open Result in
      Producer.create device (Mirage_block.Block.Memory.alloc 512) >>= fun producer ->
      Consumer.create device (Mirage_block.Block.Memory.alloc 512) >>= fun consumer ->
      Producer.push producer sector >>= fun () ->
      Consumer.pop consumer >>= fun buffer ->
      assert_equal ~printer:Cstruct.to_string ~cmp:cstruct_equal sector buffer; 
(*
    Block.really_write fd sector >>= fun () ->
    let sector' = Memory.alloc 512 in
    Block.connect name >>= function
    | `Error _ -> failwith (Printf.sprintf "Block.connect %s failed" name)
    | `Ok device ->
      Block.read device Int64.(sub (div size 512L) 1L) [ sector' ] >>= function
      | `Error _ -> failwith (Printf.sprintf "Block.read %s failed" name)
      | `Ok () -> begin
        assert_equal ~printer:Cstruct.to_string ~cmp:cstruct_equal sector sector';
        return ()
      end in
*)
    return () in
  Lwt_main.run t

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
  "Test shared block ring";

  let suite = "shared-block-ring" >::: [
    "test open read" >:: test_push;
  ] in
  run_test_tt ~verbose:!verbose suite
