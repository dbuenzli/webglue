(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg
open Cmdliner

let cmds = [
  Ccontent.cmd; Cdiagnose.cmd; Cget.cmd; Chelp.cmd; Cset.cmd; Cshow.cmd;
  Curiset.cmd; ]

let version = "%%VERSION%%"
let doc = "Command line tool to build static websites"
let man = [
  `S "DESCRIPTION";
  `P "$(b,webglue) builds static websites from map files.";
  `P "Use 'webglue help maps' for information about map files.";
  `Noblank;
  `P "Use 'webglue help directives' for information about directives.";
  `Noblank;
  `P "Use 'webglue help topics' for a list of help topics.";
  `Noblank;
  `P "Use 'webglue help $(i,COMMAND)' for information about $(i,COMMAND).";
   ] @ C.copts_man @ [
  `S "ENVIRONMENT";
  `P "Webglue commands make use of the following environment variables:";
  `I ("$(i,WEBGLUE_MAP_PATH)",
      "A colon-separated list of directories that are recursively looked
       up for maps.");
  `I ("$(i,WEBGLUE_FORMAT_PATH)",
      "A colon-separated list of directories that are recursively looked
       up for format definitions.");
  `S "BUGS AND FEEDBACK";
  `P "Email them to <%%MAINTAINER%%>.";
  `S "AUTHOR";
  `P "Written by Daniel C. Buenzli, $(i,http://erratique.ch).";
  `S "WARNING";
  `P "This tool is a conjecture. Its interface and mode of operation
      is subject to change.";
  `S "SEE ALSO";
  `P "$(b,webglue-directives)(7), $(b,webglue-formats)(5),
      $(b,webglue-maps)(5)" ]

let info = Term.info "webglue" ~version ~doc ~sdocs:C.copts_sec ~man
let default = Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ C.copts))
let main () = match Term.eval_choice (default, info) cmds with
| `Error _ -> exit 1
| _ ->  if Wlog.Private.errors () > 0 then exit 1 else exit 0

let () = main ()

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
