(*BISECT-IGNORE-BEGIN*)
let debug fmt = Lwt_log.debug_f fmt
let info  fmt = Lwt_log.info_f fmt
let error fmt = Lwt_log.error_f fmt
let verbose = ref false
let trace list =
  if !verbose
  then begin
    info "%s" (Sexplib.Sexp.to_string (Shared_block.S.sexp_of_traced_operation_list list));
    Lwt.return ()
  end else Lwt.return ()
(*BISECT-IGNORE-END*)
