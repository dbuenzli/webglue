(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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
