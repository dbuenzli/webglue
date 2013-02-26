(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

open Ocamlbuild_plugin
    

let webglue_rules webglue =
  let str = Printf.sprintf in 
  let pct_encode s = 
    let b = Buffer.create 256 in
    let unreserved i = 
      0x41 <= i && i <= 0x5A || 0x61 <= i && i <= 0x7A || 
      0x30 <= i && i <= 0x39 || i = 0x2D || i = 0x2E || i = 0x5F || i = 0x7E
    in
    for k = 0 to (String.length s) - 1 do 
      let i = Char.code s.[k] in 
      if unreserved i then Buffer.add_char b s.[k] else
      begin 
	Buffer.add_char b '%';
	Buffer.add_string b (Printf.sprintf "%02X" i)
      end
    done;
    Buffer.contents b      
  in
  let pct_decode s =                (* warning assumes encoded by pct_encode. *)
    let b = Buffer.create 256 in
    let len = String.length s in
    let k = ref 0 in
    while (!k < len) do 
      if s.[!k] <> '%' then (Buffer.add_char b s.[!k]) else
      begin 
	incr k;
	let hex_ord i = if i <= 0x39 then i - 0x30 else i - 55 in 
	let h = hex_ord (Char.code s.[!k]) in
	incr k;
	let l = hex_ord (Char.code s.[!k]) in
	Buffer.add_char b (Char.chr (h * 16 + l));
      end;
      incr k;
    done;
    Buffer.contents b
  in  
  let building_targets = ref StringSet.empty in          (* cycle prevention *)
  let building t = building_targets := StringSet.add t !building_targets; t in
  let deps_of_file file = 
    let parse_dep l = try
      let split_r p = String.sub l (p + 1) (String.length l - p - 1) in
      let split_m p1 p2 = String.sub l (p1 + 1) (p2 - p1 - 1) in 
      let p1 = String.index l ':' in 
      match String.sub l 0 p1 with 
      | "val" ->
	  let p2 = String.rindex l ':' in 
	  (* if p2 = p1 then raise Exit else 
	     `Val (split_m p1 p2, split_r p2) *)
	  `Map (split_m p1 p2)
      | "map" -> `Map (split_r p1)
      | "file" -> `File (split_r p1)
      | _ -> raise Exit
    with Exit | Not_found -> failwith 
	(str "%s: invalid webglue dependency format" file)
    in
    List.rev_map parse_dep (string_list_of_file file) 
  in
  let build_deps build make_target deps =
    let targets acc d =
      match make_target d with 
      | None -> acc 
      | Some t when StringSet.mem t !building_targets -> acc 
      | Some t -> [t] :: acc
    in
    List.iter Outcome.ignore_good (build (List.fold_left targets [] deps));
  in
  rule "webglue: d -> od" ~dep:"%.d" ~stamp:"%.od" 
    begin fun env build -> 
      let _ = building (env "%.od") in
      let make_target = function 
	| `Val (m, k) -> 
	    Some ((Pathname.remove_extension m) ^ "+" ^ (pct_encode k) ^ ".val")
	| `File f -> Some f
	| `Map m -> Some (Pathname.update_extension "map.od" m)
      in
      build_deps build make_target (deps_of_file (env "%.d")); Cmd N
    end;

  rule "webglue: map -> map.d" ~dep: "%.map" ~prod: "%.map.d" 
    begin fun env build -> 
      let b = building (env "%.map.d") in 
      let m = env "%.map" in
      let mid = Pathname.basename (env "%") in
      let tags = tags_of_pathname m ++ "webglue" ++ "deps" ++ "map" in 
      Cmd (S [A webglue; A "get"; A "--deps"; T tags; A "-o"; P b; P mid])
     end;

  rule "webglue: map.od -> uriset.d" ~dep:"%.map.od" ~prod:"%.uriset.d"
    begin fun env build ->
      let b = building (env "%.uriset.d") in
      let m = env "%.map" in
      let mid = Pathname.basename (env "%") in 
      let tags = tags_of_pathname m ++ "webglue" ++ "deps" ++ "uriset" in 
      Cmd (S [A webglue; A "uriset"; A "--deps"; T tags; A "-o"; P b; P mid])
    end;
      
  rule "webglue: uriset.od -> uriset" ~dep:"%.uriset.od" ~prod:"%.uriset"
    begin fun env build ->
      let b = building (env "%.uriset") in 
      let m = env "%.map" in
      let mid = Pathname.basename (env "%") in
      let tags = tags_of_pathname m ++ "webglue" ++ "compile" ++ "uriset" in
      Cmd (S [A webglue; A "uriset"; T tags; A "-o"; P b; P mid])
    end;

  rule "webglue: uriset -> uriset.o" ~dep:"%.uriset" ~stamp:"%.uriset.o" 
    begin fun env build ->
      let _ = building (env "%.uriset.o") in 
      let make_uri u = [(env "%") ^ "+" ^ (pct_encode u) ^ ".uri"] in 
      let uris = List.rev_map make_uri (string_list_of_file (env "%.uriset")) in
      List.iter Outcome.ignore_good (build uris); Cmd N
    end;

  rule "webglue: map.od -> uri.d" ~dep:"%(id).map.od" ~prod:"%(id)+%(uri).uri.d"
    begin fun env build ->
      let b = building (env "%(id)+%(uri).uri.d") in
      let m = env "%(id).map" in 
      let mid = Pathname.basename (env "%(id)") in 
      let u = pct_decode (Pathname.basename (env "%(uri)")) in 
      let tags = tags_of_pathname m ++ "webglue" ++ "deps" ++ "uri" in
      Cmd (S [A webglue; A "content"; T tags; A "--deps"; A "-o"; P b; P mid; 
	      P u])
    end;

  rule "webglue: uri.od -> uri" ~dep:"%(id)+%(uri).uri.od" 
    ~stamp:"%(id)+%(uri).uri"
    begin fun env build ->
      let _ = building (env "%(id)+%(uri).uri") in
      let m = env "%(id).map" in
      let mid = Pathname.basename (env "%(id)") in 
      let u = pct_decode (Pathname.basename (env "%(uri)")) in 
      let tags = tags_of_pathname m ++ "webglue" ++ "compile" ++ "uri" in
      Cmd (S [A webglue; A "content"; T tags; P mid; P u])
    end;
  
  rule "webglue: uriset.o -> site" ~dep:"%.uriset.o" ~stamp:"%.site"
    begin fun env build -> 
      let _ = building (env "%.site") in 
      let uris = string_list_of_file (env "%.uriset") in 
      let add_deps acc u =
	let deps = deps_of_file (env "%" ^ "+" ^ (pct_encode u) ^ ".uri.d") in 
	List.rev_append deps acc 
      in 
      let deps = List.fold_left add_deps [] uris in 
      let make_target = function 
	| `Val (m, k) -> Some (Pathname.update_extension "site" m)
	| `File f -> None 
	| `Map m -> Some (Pathname.update_extension "site" m)
      in
      build_deps build make_target deps; Cmd N
    end;

  (* Dependencies are looked up in source tree, but reported in build
     tree (hence the --dep-base) *)

  pflag [ "webglue"; "compile"] "map_dir" (fun d -> S [ A "-M"; P d]);
  flag  [ "webglue"; "deps" ] (S [A "--dep-base"; P Pathname.pwd]);
  pflag [ "webglue"; "deps" ] "map_dir" 
    (fun d -> S [ A "-M"; P (Pathname.pwd / d)]);

  pflag [ "webglue"; "uri" ] "base" (fun d -> S [ A "-d"; P d]);
  pflag [ "webglue" ] "format_dir" (fun d -> S [ A "-F"; P d]);
  pflag [ "webglue" ] "conf" (fun c -> S [ A "--conf"; A c]);
  flag  [ "webglue"; "verbose" ] (A "-v");

;;

dispatch begin function
  | Before_rules -> webglue_rules "webglue";
  | _ -> ()
end



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
