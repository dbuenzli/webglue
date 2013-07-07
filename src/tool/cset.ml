(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

let pretty_value v =        (* rem. trailing white and add a space for list. *)
  let v = match List.rev v with (`White _, _) :: rv -> List.rev rv | _ -> v in
  match v with (`List _, _) :: _ as v -> (`White " ", Dict.empty) :: v | v -> v

let push_binding k v res =
  (`List (k :: pretty_value v), Dict.empty) :: (`White "\n", Dict.empty) :: res

let update_map replace es (`Atom k, _ as key) v =
  if not replace then List.rev (push_binding key v (List.rev es)) else
  let rev_es = List.rev es in
  let rec aux acc = function                  (* try to update last binding. *)
    | (`List ((`Atom k', _) :: _), d) :: rest when k' = k -> 
	List.rev_append ((`List (key :: pretty_value v), d) :: rest) acc
    | e :: es -> aux (e :: acc) es
    | [] -> List.rev (push_binding key v rev_es)
  in
  aux [] rev_es 
    
let set _ inf id key update = match Map_db.find_bset id with 
| None -> Wlog.err (`Undefined_map id)
| Some m -> 
    let errs = Wlog.Private.errors () in
    match Sefile.input_full_se_list inf with None -> ()       (* file error. *)
    | Some (v, _) -> 
	if errs <> Wlog.Private.errors () then () (* parse error *) else
	match Map_db.filename id with
	| None -> Wlog.err (`Undefined_map id)       (* file may have moved. *)
	| Some f -> 
	    match Sefile.input_full_se_list f with None -> ()
	    | Some (es, _) ->
		match Sysm.tmp_file f with None -> ()
		| Some tf -> 
		    Sefile.output_full_se_list tf (update_map update es key v);
		    ignore (Sysm.rename tf f)
		      
(* Command line interface *)

open Cmdliner

let doc = "Replace the last binding for $(i,KEY) in the map."
let replace = Arg.(value & flag & info ["r"; "replace"] ~doc)

let doc = "set the value of a map key" 
let man = [
  `S "DESCRIPTION";
  `P "The command $(b,set) sets the value bound to $(i,KEY) in the map 
      $(i,ID). The value is read from stdin or from a file specified with the 
      option $(b,--input).";
  `P "The new binding is added at the end of the map file or, if 
      there is already a binding for $(i,KEY) in the file and the option 
      $(b,--replace) is present, the last binding for $(i,KEY) in the file 
      is replaced.";
   ] @ C.copts_man @ [
  `S "SEE ALSO";
  `P "$(b,webglue-get)(1), $(b,webglue-maps)(5)" ]

let info = Term.info "set" ~sdocs:C.copts_sec ~doc ~man
let cmd = Term.(pure set $ C.copts $ C.input $ C.id $ C.key $ replace), info

(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli.
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
