(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
   %%NAME%% %%VERSION%%
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
