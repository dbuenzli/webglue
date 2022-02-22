(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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

(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
