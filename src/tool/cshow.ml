(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
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

let info = Term.info "show" ~sdocs:C.copts_sec ~doc ~man
let cmd = Term.(ret (pure show $ C.copts $ C.output $ kind $ entity)), info
  

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
