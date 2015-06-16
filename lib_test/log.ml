(*BISECT-IGNORE-BEGIN*)
let debug fmt = Printf.ksprintf (fun s -> print_endline s) fmt
let info  fmt = Printf.ksprintf (fun s -> print_endline s) fmt
let error fmt = Printf.ksprintf (fun s -> print_endline s) fmt
let verbose = ref false
let trace list =
  if !verbose
  then info "%s" (Sexplib.Sexp.to_string (Shared_block.S.sexp_of_traced_operation_list list))
  else ()
(*BISECT-IGNORE-END*)
