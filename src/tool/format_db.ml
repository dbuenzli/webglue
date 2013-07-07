(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

let format_str (fm : atom) = Se.to_atom_str (fm :> se)
let msg_def fm f = Wlog.msg "format %s defined in %s" (format_str fm) f

let file_of_format fm =
  let f = String.copy (format_str fm) in 
  for i = 0 to String.length f - 1 do if f.[i] = '.' then f.[i] <- '_' done;
  Dynlink.adapt_filename (f ^ ".cma")

let format_of_file f = 
  let fm = Filename.chop_extension (Filename.basename f) in
  for i = 0 to String.length fm - 1 do if fm.[i] = '_' then fm.[i] <- '.' done;
  Se.atom fm

let dirs = ref []
let init ds = dirs := ds
let db = Hashtbl.create 64
let db_mem fm = Hashtbl.mem db (format_str fm)
let db_add fm fM = Hashtbl.replace db (format_str fm) fM
let db_find fm = try Some (Hashtbl.find db (format_str fm)) with 
| Not_found -> None

let load_format fm = match Sysm.find_file !dirs (file_of_format fm) with
| None -> ()
| Some path ->
    Wlog.debug (msg_def fm path);
    try Dynlink.loadfile_private path with       (* calls Wformat.define ! *)
    | Dynlink.Error e -> Wlog.err (Wlog.msg "%s" (Dynlink.error_message e))

let list () =
  let ext = Dynlink.adapt_filename ".cma" in 
  let add (set, fmts as acc) f = 
    if not (Filename.check_suffix f ext) then acc else
    let fm = format_of_file f in
    if Aset.mem fm set then acc else (Aset.add fm set, (fm, f) :: fmts)
  in
  List.sort compare (snd (Sysm.fold_files_rec !dirs add (Aset.empty, [])))
    
let is_defined = db_mem
let define fM = let module F = (val fM : Wformat.T) in db_add F.name fM
let find fm = match db_find fm with
| Some _ as r -> r
| None -> load_format fm; (* and try again *) db_find fm 
	
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
