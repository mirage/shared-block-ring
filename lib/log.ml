let debug fmt = Printf.ksprintf (fun s -> print_endline s) fmt
let info  fmt = Printf.ksprintf (fun s -> print_endline s) fmt
let error fmt = Printf.ksprintf (fun s -> print_endline s) fmt
