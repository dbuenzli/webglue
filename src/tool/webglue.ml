(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
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
