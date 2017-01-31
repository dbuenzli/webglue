(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

let format_str (fm : atom) = Se.to_atom_str (fm :> se)
let msg_def fm f = Wlog.msg "format %s defined in %s" (format_str fm) f

let file_of_format fm =
  let f = Bytes.of_string (format_str fm) in
  for i = 0 to Bytes.length f - 1 do
    if Bytes.get f i = '.' then Bytes.set f i '_'
  done;
  Dynlink.adapt_filename ((Bytes.unsafe_to_string f) ^ ".cma")

let format_of_file f =
  let fm = Bytes.of_string (Filename.chop_extension (Filename.basename f)) in
  for i = 0 to Bytes.length fm - 1 do
    if Bytes.get fm i = '_' then Bytes.set fm i '.'
  done;
  Se.atom (Bytes.unsafe_to_string fm)

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
