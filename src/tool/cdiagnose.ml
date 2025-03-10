(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Wg

let diagnose copts conf id_opt outf = match id_opt with
| None -> failwith "TODO"
| Some id ->
    let c = Wctx.Private.create ?conf Map_db.find id in
    match Wctx.find_map c id with
    | None -> Wlog.err (`Undefined_map id)
    | Some m -> failwith "TODO"

(* Command line interface *)

open Cmdliner

let doc = "show diagnostics about a map"
let man = [
  `S "DESCRIPTION";
  `P "The command $(b,diagnose) shows format dependent diagnostics about
      the map $(i,ID).";
  `P "If $(i,ID) is not present, the URI set of
      all maps in the map path is determined and errors are reported
      for URI or URI path duplicates.";
   ] @ C.copts_man @ [
  `S "SEE ALSO";
  `P "$(b,webglue-maps)(5), $(b,webglue-uriset)(1)" ]


let cmd =
  Cmd.v (Cmd.info "diagnose" ~sdocs:C.copts_sec ~doc ~man)
    Term.(const diagnose $ C.copts $ C.conf $ C.id_opt $ C.output)
