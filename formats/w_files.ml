(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

module Fset = struct                                    (* sets of filenames *)
  type el = File of Wg.atom | Dir of Wg.atom
  let compare e e' = match e, e' with 
  | File (f, _), File (f', _) -> compare f f'
  | Dir (d, _), Dir (d', _) -> compare d d' 
  | e , e' -> compare e e'

  include Set.Make (struct type t = el let compare = compare end) 

  let dir_contents absolute (`Atom dir, d) = try
    let contents = Sys.readdir (absolute dir) in
    let dir = if dir = Filename.current_dir_name then "" else dir in 
    let mk_path acc f = (Se.atom ~d (Filename.concat dir f)) :: acc in 
    Array.fold_left mk_path [] contents
  with Sys_error e -> Wlog.err (Wlog.msg ~d "%s" e); []

  let of_files suffixes recurse base files = 	  
    let suffixes = match suffixes with [] -> [Se.atom ""] | l -> l in 
    let absolute f = Filename.concat base f in
    let is_dir (`Atom d, _) = try Sys.is_directory (absolute d) with 
    | Sys_error _ -> false
    in
    let rec aux suffixes recurse depth acc = function
      | (f :: fs) :: up when is_dir f ->
	  if recurse || depth = 0 then
	    let files = dir_contents absolute f in
	    let acc' = add (Dir f) acc in
	    aux suffixes recurse (depth + 1) acc' (files :: fs :: up)
	  else
	    aux suffixes recurse depth acc (fs :: up)   
      | ((`Atom file, d as f) :: fs) :: up ->
	  let acc' =
	    let has_suffix f (`Atom s, _) = Filename.check_suffix f s in
	    if not (List.exists (has_suffix file) suffixes) then acc else
	    if Sys.file_exists (absolute file) then add (File f) acc else
	    (Wlog.err (Wlog.msg ~d "%s: No such file" file); acc)
	  in
	  aux suffixes recurse depth acc' (fs :: up)   
      | [] :: [] -> acc
      | [] :: up -> aux suffixes recurse (depth - 1) acc up
      | [] -> assert false
    in
    aux suffixes recurse 0 empty (files :: [])
end

module Files = struct
  include Wformat.Default
  let err_filename d = Wlog.msg ~d "the map has no Dict.Key.filename key"

  let name = Se.atom "w.files"
  let doc = "URIs to raw files"
  let man = [
    Wformat.man_description;
    `P "The $(b,w.files) map format defines URIs whose content is defined
        by raw files stored relative to the map.";
    `S "FILE SET";
    `P "The keys $(b,files), $(b,recurse) and $(b,suffixes) define a set
	of files relative to the map's file.";
    `P "For each file in this set a data binding is added to the map.
        The key of the binding 
        is the filename and its value the URI that refers to the file.";
    Wformat.man_uri_set;
    `P "For each file in the file set a localeless URI is defined. 
        The URI and its path are defined by concatenating the @-text of 
        $(b,uri-base) to the name of the file.";
    `P "If $(b,uri-suffix-chop) is present and the URI ends with one of
        these suffixes, that part is truncated from the URI (but not from
        its path).";
    `P "If the $(b,uri-base-path) key is present, its @-text is used
        instead of $(b,uri-base) to define the base path for the URI's path.";
    Wformat.man_uri_content;
    `P "The content of an URI is the content of the file it corresponds
        to."; 
   ]

  let k_uri_base = Bset.key "uri-base"
  let k_uri_base_path = Bset.key "uri-base-path"
  let k_uri_suffix_chop = Bset.key "uri-suffix-chop"
  let k_files = Bset.key "files"
  let k_suffixes = Bset.key "suffixes"
  let k_recurse = Bset.key "recurse"
    
  let keys = [ 
    k_uri_base, `Required,
    "@-text defining the base URI (without trailing slash).";
    k_uri_suffix_chop, `Optional (Some []),
    "Atom list of suffixes to chop from filenames to define the URIs.";
    k_uri_base_path, `Optional None,
    "@-text defining the base URI path (without trailing slash).";
    k_files, `Required,
    "Atom list of files and directories to include relative to the map.";
    k_suffixes, `Optional (Some []),
    "Atom list of suffixes, only files ending with these suffixes are 
     included.";
    k_recurse, `Optional (Some [Se.atom "false"]),
    "If true, directories are included recursively.";
    (Bset.key "file"), `Derived,
    "For each file in $(b,files) and the content of directories,
     $(i,file) maps to an @-text defining the URI of $(i,file)." ]

  let dk_base = Dict.key ()
  let dk_file_set = Dict.key () (* list of files defined relative to dk_base *)

  let atoms kind c m k = 
    let b, d = Wmap.get c m k in 
    Se.parse ~err:[] (Se.p_list_of (Se.p_atom kind)) (Se.list ~d b)

  let file_set base c m = 
    let suffixes = atoms "file suffix" c m k_suffixes in 
    let recurse = 
      let b, d = Wmap.get c m k_recurse in
      fst (Se.pnext ~err:false ~last:true Se.p_bool b d)
    in
    let files = atoms "file name" c m k_files in
    Fset.of_files suffixes recurse base files

  let create c m = match Dict.find (Bset.dict m) Dict.Key.filename with 
  | None -> Wlog.err (err_filename (Bset.dict m)); None
  | Some filename ->
      let base = Filename.dirname filename in
      let file_set = file_set base c m in
      let uri_base = Setext.to_string (fst (Wmap.get c m k_uri_base)) in
      let uri_chop = atoms "uri suffix" c m k_uri_suffix_chop in
      let make_uri f = 
	let u = Filename.concat uri_base f in 
	let chops u (`Atom s, _) = Filename.check_suffix u s in 
	try
	  let `Atom s, _ = List.find (chops u) uri_chop in 
	  Filename.chop_suffix u s
	with Not_found -> u
      in
      let make_binding f m = 
	begin match f with 
	| Fset.File (`Atom f, d) -> 
	    Wctx.add_dep c (`File (`Atom (Filename.concat base f), d))
	| Fset.Dir _ -> ()
	end;
	match f with Fset.Dir (`Atom f, d) | Fset.File (`Atom f, d) -> 
	Bset.add m (Bset.key f) [Se.atom ~d (make_uri f)]
      in
      let m' = Fset.fold make_binding file_set m in
      let dict = Dict.add (Bset.dict m) dk_file_set file_set in 
      let dict = Dict.add dict dk_base base in 
      Some (Bset.with_dict m' dict)
	
  let uri_set c m =
    let base = Dict.get (Bset.dict m) dk_base in
    let file_set = Dict.get (Bset.dict m) dk_file_set in 
    let uri_base = Setext.to_string (fst (Wmap.get c m k_uri_base)) in
    let uri_base_path = match Wmap.find c m k_uri_base_path with 
    | Some (p, _) -> Setext.to_string p 
    | None -> uri_base 
    in
    let make_uri f acc = match f with
    | Fset.Dir _ -> acc
    | Fset.File (`Atom f, _ as kf) ->
	let d = Bset.dict m in
	let d = Dict.add d Dict.Key.filename (Filename.concat base f) in
	let uri = match Wmap.get c m kf with
	| [`Atom uri, _], _ -> uri | _ -> assert false
	in
	let path = Filename.concat uri_base_path f in
	Wuri.Set.add_opt (Wuri.create ~d (Se.atom uri) (Se.atom path) None) acc
    in
    Fset.fold make_uri file_set Wuri.Set.empty 

  let uri_content eval m u dst =
    try
      let len = 8192 in
      let buffer = String.create 8192 in
      let ic = open_in_bin (Dict.get (Wuri.dict u) Dict.Key.filename) in
      try
	let o = Out.make dst in 
	let rec copy ic buffer len o = match input ic buffer 0 len with
	| 0 -> () (* eof *)
	| l -> Out.ssub o buffer 0 l; copy ic buffer len o
	in
	copy ic buffer len o;
	close_in ic
      with e -> (close_in ic; raise e)
    with Sys_error e -> Wlog.err (Wlog.msg ~d:(Bset.dict m) "%s" e)

end

let () = Wformat.define (module Files : Wformat.T)

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
