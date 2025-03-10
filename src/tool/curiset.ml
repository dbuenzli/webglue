(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

let uriset c outf out_kind =
  let c = c.C.c and id = c.C.id and dep_base = c.C.copts.C.dep_base in
  match Wctx.find_map c id with
  | None -> Wlog.err (`Undefined_map id)
  | Some m ->
      match out_kind with
      | `Short -> C.out_v outf (Wuri.Set.pp Wuri.pp) (Wmap.uri_set c m)
      | `Full -> C.out_v outf (Wuri.Set.pp Wuri.pp_full) (Wmap.uri_set c m)
      | `Deps -> C.out_deps outf dep_base (Wmap.uri_set_deps c m)

(* Command line interface *)

open Cmdliner

let out_kind =
  let full = `Full, Arg.info ["full"] ~doc:"Show the full URI data." in
  Arg.(value & vflag `Short [full; C.deps])

let brief = Arg.(value & flag & info ["b"; "brief"] ~doc:"Show only the URIs.")
let doc = "show the URI set of a map"
let man = [
  `S "DESCRIPTION";
  `P "The command $(b,uriset) shows the URI set of the map $(i,ID).";
  `P "If the option $(b,--full) is present, the URI path and locale (if any)
      is also shown.";
  `P "If the option $(b,--deps) is present, the set of dependencies
      needed to define the URI set of the map $(i,ID) is shown.";
   ] @ C.copts_man @ [
  `S "SEE ALSO";
  `P "$(b,webglue-content)(1), $(b,webglue-maps)(5)" ]


let cmd =
  Cmd.v (Cmd.info "uriset" ~sdocs:C.copts_sec ~doc ~man)
    Term.(const uriset $ C.context ~locale_opt:false $ C.output $ out_kind)
