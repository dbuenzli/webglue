(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg
open Cmdliner

(* Argument converters *)

let atom = Arg.conv' ((fun s -> Ok (Se.atom s)), Se.pp_atom)

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
  Term.(const copts_with $ verbosity $ formats_dirs $ map_dirs $ dep_base)

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
    Arg.conv'
      ((fun s ->
          let l = Se.atom s in if Wlocale.is_locale l then Ok l else
          Error (Fmt.str
                   "invalid value `%s', expected a BCP 47 language tag" s)),
       Se.pp_atom)
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
  let locale = if locale_opt then locale else Term.const None in
  Term.(const context_with $ copts $ locale $ conf $ id)

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
