(*
 * Copyright (C) 2015 Citrix Inc
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

let project_url = "http://github.com/djs55/shared-block-ring"

let produce filename interval =
  `Error(false, "Unimplemented")

let consume filename interval =
  `Error(false, "Unimplemented")

let create filename =
  `Error(false, "Unimplemented")

let diagnostics filename =
  `Error(false, "Unimplemented")

open Cmdliner

(* Help sections common to all commands *)

let help = [
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

let filename =
  let doc = "Path to the device or file containing the ring." in
  Arg.(value & pos 0 (some file) None & info [] ~doc)

let interval =
  let doc = "Time in seconds between I/O retries." in
  Arg.(value & opt int 5 & info [ "interval" ] ~doc)

let produce_cmd =
  let doc = "Push data onto the ring" in
  let man = [
    `S "DESCRIPTION";
    `P "Read lines of text from stdin and push them as individual items onto the ring.";
  ] @ help in
  Term.(ret(pure produce $ filename $ interval)),
  Term.info "produce" ~doc ~man

let consume_cmd =
  let doc = "Pop data from the ring" in
  let man = [
    `S "DESCRIPTION";
    `P "Read lines of text from the ring and print them to stdout.";
  ] @ help in
  Term.(ret(pure consume $ filename $ interval)),
  Term.info "consume" ~doc ~man

let create_cmd =
  let doc = "Create an empty ring" in
  let man = [
    `S "DESCRIPTION";
    `P "Initialise a device or file with an empty ring."
  ] @ help in
  Term.(ret(pure create $ filename)),
  Term.info "create" ~doc ~man

let diagnostics_cmd =
  let doc = "Display the current state of a ring." in
  let man = [
    `S "DESCRIPTION";
    `P "Display the current ring state including producer and consumer pointers, together with the current ring contents for diagnostic purposes.";
    `P "Note: the ring will not be modified."
  ] @ help in
  Term.(ret(pure diagnostics $ filename)),
  Term.info "diagnostics" ~doc ~man

let default_cmd =
  let doc = "manipulate shared rings on block devices" in
  let man = help in
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info (Sys.argv.(0)) ~version:"1.0.0" ~doc ~man

let cmds = [create_cmd; produce_cmd; consume_cmd; diagnostics_cmd]

let _ =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
