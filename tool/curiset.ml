(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
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

let info = Term.info "uriset" ~sdocs:C.copts_sec ~doc ~man
let cmd =
  Term.(pure uriset $ C.context ~locale_opt:false $ C.output $ out_kind), info

(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of the Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
