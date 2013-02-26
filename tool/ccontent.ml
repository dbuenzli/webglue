(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

let err_uri m u = 
  Wlog.msg ~d:(Bset.dict m) "URI `%a' undefined in map ID `%a'"
    Se.pp_atom u Se.pp_atom (Wmap.id m) 

let find_uris c m uri = match uri with 
| None -> Wmap.uri_set c m 
| Some uri -> match Wuri.Set.find uri (Wmap.uri_set c m) with 
  | None -> Wlog.err (err_uri m uri); raise Exit  (* TODO catch *)
  | Some u -> Wuri.Set.singleton u
	
let out_content outf base c m uri =
  let us = find_uris c m uri in 
  let content oc u = Wmap.uri_content c m u (`Channel oc) in
  match base with 
  | None -> Sysm.with_outf outf () (fun oc -> Wuri.Set.iter (content oc) us)
  | Some b -> 
      let out u = 
	let f = Filename.concat b (Se.to_atom_str ((Wuri.path u) :> se)) in
	Sysm.with_out_path f () (fun oc -> content oc u)
      in
      Wuri.Set.iter out us

let out_recurse _ base c m _ = (* TODO cleanup *)
  let rec aux b seen todo = 
    match try Some (Aset.choose todo) with Not_found -> None with 
    | None -> ()
    | Some id ->
	let m = match Wctx.find_map c id with 
	| None -> Wlog.err (`Undefined_map id); raise Exit (* TODO *)
	| Some m -> m 
	in
	let out_uri todo u = 
	  let add_todo d acc = match d with `Other _ | `File _ -> acc
	   | `Map id | `Val (id, _) -> 
	       if Aset.mem id seen then acc else Aset.add id acc 
	  in
	  let content oc = Wmap.uri_content c m u (`Channel oc) in
	  let f = Filename.concat b (Se.to_atom_str ((Wuri.path u) :> se)) in
	  Sysm.with_out_path f () content;
	  Wdep.Set.fold add_todo (Wmap.uri_deps c m u) todo
	in
	let todo' = Wuri.Set.fold out_uri todo (Wmap.uri_set c m) in
	aux b (Aset.add id seen) (Aset.remove id todo')
  in
  match base with 
  | None -> Wlog.err (Wlog.msg "no base directory defined TODO"); raise Exit
  | Some b -> 
      aux b Aset.empty (Aset.singleton (Wmap.id m))
  

let out_deps outf c m uri base = try
  let add_deps acc u = Wdep.Set.union acc (Wmap.uri_deps c m u) in 
  let deps = Wuri.Set.fold add_deps Wdep.Set.empty (find_uris c m uri) in 
  C.out_deps outf base deps
with Exit -> ()

let content c uri outf base recurse out_kind = 
  let c = c.C.c and id = c.C.id and dep_base = c.C.copts.C.dep_base in
  match Wctx.find_map c id with
  | None -> Wlog.err (`Undefined_map id)
  | Some m -> match out_kind with
    | `Deps -> out_deps outf c m uri dep_base
    | `Content -> 
	(if recurse then out_recurse else out_content) outf base c m uri 

	  
(* Command line interface *)
	    
open Cmdliner

let out_kind = Arg.(value & vflag `Content [C.deps])

let doc = "Use $(docv) as the base directory to write the content of URIs."
let base = Arg.(value & opt (some string) None & info [ "d"; "content-base"] 
		~docv:"DIR" ~doc)

let doc = "The URI to generate."
let uri = Arg.(value & pos 1 (some C.atom) None & info [] ~docv:"URI" ~doc)

let doc = "show the content of a map URI"
let man = [
  `S "DESCRIPTION";
  `P "The command $(b,content) shows the content of $(i,URI) 
      defined in the map $(i,ID). If $(i,URI) is absent, the content of
      each element in the URI set of the map $(i,ID) is shown.";
  `P "If the option $(b,--content-base) is specified,
      the content is written to the URI path relative to that 
      directory. Otherwise the file specified by $(b,--output) is used.";
  `P "If the option $(b,--deps) is present, the set of dependencies
      needed to define the content of $(i,URI) or the content of the elements
      in the URI set of the map $(i,ID) is shown.";
  ] @  C.copts_man @ [
  `S "SEE ALSO";
  `P "$(b,webglue-maps)(5), $(b,webglue-uriset)(1)"; ]

let info = Term.info "content" ~sdocs:C.copts_sec ~doc ~man
let cmd =
  Term.(pure content $ C.context ~locale_opt:false $ uri $ C.output $ base $
	C.recurse $ out_kind), info

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
