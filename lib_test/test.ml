(*
 * Copyright (C) 2013-2015 Citrix Inc
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

(* Let's try to adopt the conventions of Rresult.R *)

let get_ok = function
| `Ok x -> x
| `Error _ -> raise (Invalid_argument "get_ok encountered an `Error")

let get_error = function
| `Error x -> x
| `Ok _ -> raise (Invalid_argument "get_error encountered an `Ok")

module Lwt_result = struct
  let (>>=) m f = m >>= fun x -> f (get_ok x)
end

let find_unused_file () =
  (* Find a filename which doesn't exist *)
  let rec does_not_exist i =
    let name = Printf.sprintf "%s/mirage-block-test.%d.%d"
      Filename.temp_dir_name (Unix.getpid ()) i in
    if Sys.file_exists name
    then does_not_exist (i + 1)
    else name in
  does_not_exist 0

let alloc sector_size =
  let page = Io_page.(to_cstruct (get 1)) in
  let sector = Cstruct.sub page 0 sector_size in
  sector

let fill_with_message buffer message =
  for i = 0 to Cstruct.len buffer - 1 do
    Cstruct.set_char buffer i (message.[i mod (String.length message)])
  done

let fresh_file nsectors =
  let name = find_unused_file () in
  Lwt_unix.openfile name [ Lwt_unix.O_CREAT; Lwt_unix.O_WRONLY ] 0o0666 >>= fun fd ->
  (* Write the last sector to make sure the file has the intended size *)
  Lwt_unix.LargeFile.lseek fd Int64.(sub nsectors 512L) Lwt_unix.SEEK_CUR >>= fun _ ->
  let sector = alloc 512 in
  fill_with_message sector "\xde\xead\xbe\xef";
  Block.really_write fd sector >>= fun () ->
  return name

let interesting_lengths = [
  0; (* possible base case *)
  1; (* easily fits inside a sector with the 4 byte header *)
  512 - 4 - 1;
  512 - 4;
  512 - 4 + 1;
  2000; (* More than 3 sectors *)
]

(* number of pushes before we pop *)
let interesting_batch_sizes = [
  1;
  2;
  3;
  4;
  5;
]

module Op = struct
  type t = string
  let to_cstruct x =
    let c = Cstruct.create (String.length x) in
    Cstruct.blit_from_string x 0 c 0 (String.length x);
    c
  let of_cstruct x = Some (Cstruct.to_string x)
end

module R = Shared_block.Ring.Make(Log)(Block)(Op)
open R

let size = 16384L

let test_push_pop length batch () =
  let t : unit Lwt.t =
    fresh_file size
    >>= fun name ->

    let payload = "All work and no play makes Dave a dull boy.\n" in
    let toobig = String.create Int64.(to_int size) in
    let open Lwt_result in
    Block.connect name
    >>= fun disk ->
    Producer.create ~disk () >>= fun () ->
    Producer.attach ~disk () >>= fun producer ->
    Consumer.attach ~disk () >>= fun consumer ->
    let rec loop = function
      | 0 -> return ()
      | n ->
        let open Lwt in
        Consumer.pop ~t:consumer () >>= function
        | `Ok _ | `Error _ -> failwith "empty pop"
        | `Retry ->
        let rec push = function
        | 0 -> return ()
        | m ->
          ( Producer.push ~t:producer ~item:toobig () >>= function
            | `TooBig -> return ()
            | _ -> failwith "push" ) >>= fun () ->
          Producer.push ~t:producer ~item:payload () >>= function
          | `Error _ | `TooBig | `Retry | `Suspend -> failwith "push"
          | `Ok position ->
            (Producer.advance ~t:producer ~position () >>= fun unit ->
             get_ok unit;
             return () 
            ) >>= fun () ->
            push (m - 1) in
        push batch >>= fun () ->
        let rec pop = function
        | 0 -> return ()
        | m ->
          Consumer.pop ~t:consumer () >>= function
          | `Error _ | `Retry -> failwith "pop"
          | `Ok (consumer_val,buffer) ->
            assert_equal ~printer:(fun x -> x) payload buffer;
	    Consumer.advance ~t:consumer ~position:consumer_val () >>= function
	    | `Ok () ->
              pop (m - 1)
	    | `Error _ -> failwith "pop" in
        pop batch >>= fun () ->
        loop (n - 1) in
    (* push/pop 2 * the number of sectors to guarantee we experience some wraparound *)
    let open Lwt in
    loop Int64.(to_int (mul 2L (div size 512L))) >>= fun () ->
    (* count how many items we can push in total *)
    let rec loop acc =
      let open Lwt in
      Producer.push ~t:producer ~item:payload () >>= function
      | `Retry -> return acc
      | `Error _ | `TooBig | `Suspend -> failwith "counting the number of pushes"
      | `Ok position ->
        (Producer.advance ~t:producer ~position () >>= fun unit ->
         get_ok unit;
         return ()
        ) >>= fun () ->
        loop (acc + 1) in
    loop 0 >>= fun n ->
    let expected = Int64.(to_int (div (sub size 1536L) (logand (lognot 3L) (of_int (String.length payload + 7))))) in
    assert_equal ~printer:string_of_int expected n;
    Producer.detach producer >>= fun () ->
    Consumer.detach consumer >>= fun () ->
    return () in
  Lwt_main.run t

let test_suspend () =
  let t =
    fresh_file size
    >>= fun name ->

    let open Lwt_result in
    Block.connect name >>= fun disk ->
    Producer.create ~disk () >>= fun () ->
    Producer.attach ~disk () >>= fun producer ->
    Consumer.attach ~disk () >>= fun consumer ->
    (* consumer thinks the queue is running *)
    let open Lwt in
    Consumer.state consumer >>= function
    | `Error x -> failwith (Printf.sprintf "Consumer.state %s" name)
    | `Ok `Suspended -> failwith "queue is suspended too early"
    | `Ok `Running ->
    (* so does the producer *)
    Producer.state producer >>= function
    | `Error x -> failwith (Printf.sprintf "Producer.state %s" name)
    | `Ok `Suspended -> failwith "queue is suspended too early"
    | `Ok `Running ->
    (* consumer requests a suspend *)
    Consumer.suspend consumer >>= function
    | `Error x -> failwith (Printf.sprintf "Consumer.suspend %s" name)
    | `Retry -> failwith "Consumer.suspend retry"
    | `Ok () ->
    (* it is not possible to request a resume before the ack *)
    Consumer.resume consumer >>= function
    | `Error x -> failwith (Printf.sprintf "Consumer.resume %s" name)
    | `Ok () -> failwith "it shouldn't be possible to immediately resume"
    | `Retry ->
    (* but the producer hasn't seen it so the queue is still running *)
    Consumer.state consumer >>= function
    | `Error x -> failwith (Printf.sprintf "Consumer.state %s" name)
    | `Ok `Suspended -> failwith "queue is suspended too early"
    | `Ok `Running ->
    (* when the producer looks, it sees the suspend *)
    Producer.state producer >>= function
    | `Error x -> failwith (Printf.sprintf "Producer.state %s" name)
    | `Ok `Running -> failwith "queue is still running"
    | `Ok `Suspended ->
    (* now the consumer sees the producer has suspended *)
    Consumer.state consumer >>= function
    | `Error x -> failwith (Printf.sprintf "Consumer.state %s" name)
    | `Ok `Running -> failwith "everyone should agree the queue is suspended"
    | `Ok `Suspended ->
    Consumer.resume consumer >>= function
    | `Error x -> failwith (Printf.sprintf "Consumer.resume %s" name)
    | `Retry -> failwith "Consumer.resume failed with Retry"
    | `Ok () ->
    (* The queue is still suspended until the producer acknowledges *)
    Consumer.state consumer >>= function
    | `Error x -> failwith (Printf.sprintf "Consumer.state %s" name)
    | `Ok `Running -> failwith "queue resumed too early"
    | `Ok `Suspended ->
    (* The queue cannot be re-suspended until the producer has seen the resume *)
    Consumer.suspend consumer >>= function
    | `Error x -> failwith (Printf.sprintf "Consumer.suspend %s" name)
    | `Ok () -> failwith "Consumer.suspend succeeded too early"
    | `Retry ->
    (* The producer notices it immediately too *)
    Producer.state producer >>= function
    | `Error x -> failwith (Printf.sprintf "Producer.state %s" name)
    | `Ok `Suspended -> failwith "producer should have seen the resume"
    | `Ok `Running ->
    
    return () in
  Lwt_main.run t

let test_journal () =
  let t =
    fresh_file size
    >>= fun name ->

    let open Lwt_result in
    Block.connect name >>= fun device ->
    let module J = Shared_block.Journal.Make(Log)(Block)(Op) in
    let perform xs =
      List.iter (fun x ->
        assert (x = "hello")
      ) xs;
      return () in
    let open Lwt in
    J.start device perform
    >>= fun j ->
    J.push j "hello"
    >>= fun wait ->
    wait ()
    >>= fun () ->
    Lwt.catch
      (fun () ->
        J.push j (String.create (Int64.to_int size))
        >>= fun _ ->
        failwith "pushed toobig to journal"
      ) (fun _ ->
        J.shutdown j
        >>= fun () ->
        return ()
      ) in
  Lwt_main.run t

let rec allpairs xs ys = match xs with
  | [] -> []
  | x :: xs -> List.map (fun y -> x, y) ys @ (allpairs xs ys)

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
  "Test shared block ring";

  let test_push_pops = List.map (fun (length, batch) ->
    Printf.sprintf "push pop %d bytes in batches of %d" length batch >:: (test_push_pop length batch)
  ) (allpairs interesting_lengths interesting_batch_sizes) in

  let suite = "shared-block-ring" >::: [
    "test suspend" >:: test_suspend;
    "test journal" >:: test_journal;
  ] @ test_push_pops in
  run_test_tt ~verbose:!verbose suite
