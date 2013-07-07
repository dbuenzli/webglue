(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
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

let info = Term.info "diagnose" ~sdocs:C.copts_sec ~doc ~man
let cmd = Term.(pure diagnose $ C.copts $ C.conf $ C.id_opt $ C.output), info

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