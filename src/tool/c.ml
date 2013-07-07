(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg
open Cmdliner 

(* Argument converters *)

let atom = (fun s -> `Ok (Se.atom s)), Se.pp_atom

(* Optional arguments common to all command. *)

type copts =
    { verbosity : Log.verbosity; format_dirs : string list;
      map_dirs : string list; dep_base : string; }

let copts_sec = "COMMON OPTIONS"
let copts_man = [`S copts_sec; `P "These options are common to all commands." ]
let copts_with verbosity format_dirs map_dirs dep_base = 
  let map_dirs = if map_dirs = [] then ["maps"] else map_dirs in 
  let r = Log.reporter verbosity Format.err_formatter in
  Wlog.Private.set_reporter r;
  Map_db.init map_dirs;
  Format_db.init format_dirs; 
  Wformat.Private.set_db (module Format_db : Wformat.Private.Db);
  { verbosity; format_dirs; map_dirs; dep_base }

let get_paths var = match Sysm.getenv var with 
| None -> [] | Some p -> 
    let split_string s sep =
      let rec split accum j = 
	let i = try (String.rindex_from s j sep) with Not_found -> -1 in
	if (i = -1) then 
	  let p = String.sub s 0 (j + 1) in 
	  if p <> "" then p :: accum else accum
	else 
	  let p = String.sub s (i + 1) (j - i) in
	  let accum' = if p <> "" then p :: accum else accum in
	  split accum' (i - 1)
      in
      split [] (String.length s - 1)
    in
    split_string p ':'

let verbosity = 
  let doc = "Do not write informational messages to standard output." in
  let quiet = `Quiet, Arg.info ["q"; "quiet" ] ~docs:copts_sec ~doc in 
  let doc = "Write more informational messages to standard output." in
  let verbose = `Verbose, Arg.info ["v"; "verbose" ] ~docs:copts_sec ~doc in
  Arg.(last & vflag_all [`Normal] [quiet; verbose])
    
let doc = "Adds $(docv) to the format search path."
let formats_dirs = 
  let fdirs = get_paths "WEBGLUE_FORMAT_PATH" @ [Libdir.value] in
  Arg.(value & opt_all string fdirs & info [ "F"; "format-dir"] ~docv:"DIR"
	 ~docs:copts_sec ~doc) 
    
let doc = "Adds $(docv) to the map file search path."
let map_dirs = 
  let mdirs = get_paths "WEBGLUE_MAP_PATH" in
  Arg.(value & opt_all string mdirs & info [ "M"; "map-dir" ] ~docv:"DIR"
	 ~docs:copts_sec ~doc)
    
let doc = "Show file dependencies relative to $(docv)."
let dep_base = 
  Arg.(value & opt string "" & info ["dep-base"] ~docv:"PATH" 
	 ~docs:copts_sec ~doc )

let copts = 
  Term.(pure copts_with $ verbosity $ formats_dirs $ map_dirs $ dep_base)

(* Positional arguments common to some commands. *)

let doc = "The map ID." and docv = "ID"
let id_opt = Arg.(value & pos 0 (some atom) None & info [] ~docv ~doc)
let id = Arg.(required & pos 0 (some atom) None & info [] ~docv ~doc)

let doc = "The map key." and docv = "KEY"
let key_opt = Arg.(value & pos 1 (some atom) None & info [] ~docv ~doc)
let key = Arg.(required & pos 1 (some atom) None & info [] ~docv ~doc)

(* Optional arguments common to some commands. *)

let deps = `Deps, Arg.(info ["deps"] ~doc:"Show dependencies.")

let doc = "Recurse on map dependencies."
let recurse = Arg.(value & flag & info ["r"; "recurse"] ~doc)

let doc = "Output to $(docv), the file name `-' is stdout."
let output = 
  Arg.(value & opt string "-" & info [ "o"; "output"] ~docv:"FILE" ~doc)

let doc = "Input from $(docv), the file name `-' is stdin."
let input = 
  Arg.(value & opt string "-" & info [ "i"; "input"] ~docv:"FILE" ~doc)

let doc = "The locale to act on (value of $(b,w.locale))."
let locale = 
  let loc = 
    (fun s -> 
      let l = Se.atom s in if Wlocale.is_locale l then `Ok l else 
    `Error (Fmt.str "invalid value `%s', expected a BCP 47 language tag" s)),
    Se.pp_atom
  in
  Arg.(value & opt (some loc) None & info ["l"; "locale"] ~docv:"LOCALE"~doc)

let doc = "The configuration to act on (value of $(b,w.conf))."
let conf = 
  Arg.(value & opt (some atom) None & info ["c"; "conf"] ~docv:"CONF" ~doc)

(* Sets of arguments common to some commands. *)

type context = 
    { copts : copts; locale : Wlocale.t option; conf : atom option; 
      id : Wg.id; c : Wctx.t }

let context_with copts locale conf id = 
  { copts; locale; conf; id; 
    c = Wctx.Private.create ?locale ?conf Map_db.find id }

let context ~locale_opt = 
  let locale = if locale_opt then locale else Term.pure None in
  Term.(pure context_with $ copts $ locale $ conf $ id)

(* Functions common to some commands. *)

let out_v outf pp v = Sysm.with_outf_pp outf "@[<v>%a@]@?" pp v
let out_deps outf base ds = 
  let relativise = Sysm.make_relative base in
  let map_file id = match Map_db.filename id with 
  | None -> Wlog.err (`Undefined_map id); None | Some f -> Some (relativise f)
  in
  let pp_dep ppf = function
    | `File (`Atom f, _) -> Fmt.pp ppf "file:%s" (relativise f)
    | `Map id -> 
	Fmt.pp_opt (fun ppf f -> Fmt.pp ppf "map:%s" f) ppf (map_file id)
    | `Val (id, key) -> 
	Fmt.pp_opt (fun ppf f -> Fmt.pp ppf "val:%s:%a" f Se.pp_atom key) 
	  ppf (map_file id)
    | `Other o -> Fmt.pp ppf "other:%a" Se.pp_atom o
  in
  out_v outf (Wdep.Set.pp pp_dep) ds

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
