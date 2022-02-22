(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

let pp_format ppf ((`Atom fm, _), f) = Fmt.pp ppf "@[<v>%s@,  %s@]" fm f

let show env outf kind entity = match kind with
| `Format_path -> `Ok (C.out_v outf (Fmt.pp_list Fmt.pp_str) env.C.format_dirs)
| `Map_path -> `Ok (C.out_v outf (Fmt.pp_list Fmt.pp_str) env.C.map_dirs)
| `Formats -> `Ok (C.out_v outf (Fmt.pp_list pp_format) (Format_db.list ()))
| `Map_file ->
    match entity with
    | None -> `Error (true, "no map ID specified")
    | Some id ->
	match Map_db.filename id with
	| None -> `Ok (Wlog.err (`Undefined_map id))
	| Some f -> `Ok (Sysm.with_outf_pp outf "@[<v>%s@,@]@?" f)

(* Command line interface *)

open Cmdliner

let kinds = [
  "format-path",`Format_path; "map-path", `Map_path; "formats",`Formats;
  "map-file", `Map_file ]

let kinds_str = String.concat ", " (List.map fst kinds)

let doc = Fmt.str "Show information on $(docv) (%s)." kinds_str
let kind =
  Arg.(required & pos 0 (some (enum kinds)) None & info [] ~docv:"KIND"~doc)

let doc = "The entity to show (the map ID for map-files)."
let entity = Arg.(value & pos 1 (some C.atom) None & info [] ~docv:"ENTITY"
		    ~doc)

let doc = "show information about the execution context"
let man = [
  `S "DESCRIPTION";
  `P "The command $(b,show) shows information about the execution context of
      the command line tool. Depending on $(i,KIND) it shows: directories
      searched for formats or maps, available formats and their file,
      or the file of a map ID." ] @ C.copts_man

let cmd =
  Cmd.v (Cmd.info "show" ~sdocs:C.copts_sec ~doc ~man)
    Term.(ret (const show $ C.copts $ C.output $ kind $ entity))


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
