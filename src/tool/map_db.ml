(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

let id_str (id : atom) = Se.to_atom_str (id :> se) 
let msg_def id file = Wlog.msg "map %s defined in %s" (id_str id) file
let dirs = ref []
let init ds = dirs := ds
let find_file id = Sysm.find_file_rec !dirs (id_str id ^ ".map")
let db = Hashtbl.create 256 
let db_add id m = Hashtbl.replace db (id_str id) m
let db_find id = try Some (Hashtbl.find db (id_str id)) with
| Not_found -> None
      
let filename id = find_file id
let find c id = match db_find id with 
| Some _ as r -> r
| None -> 
    match find_file id with
    | None -> None
    | Some file ->
	Wlog.debug (msg_def id file);
	match Sefile.input_se_list file with
	| None -> None
	| Some (_, d as r) ->
	    let id = `Atom (id_str id), d in (* for error locations. *)
	    let m = Wmap.Private.prepare c id r in
	    db_add id m;
	    let m = Wmap.Private.create c m in 
	    db_add id m; Some m
	      
let find_bset id = match find_file id with
| None -> None
| Some file ->
    Wlog.debug (msg_def id file);
    match Sefile.input_se_list file with 
    | None -> None
    | Some (sl, d) ->                       (* N.B. we don't add to the db *)
	try Some (Se.parse (Bset.p_bindings ~robust:true) (Se.list ~d sl))
	with Exit -> None
   
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
