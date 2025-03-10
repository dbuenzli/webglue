(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Wg

let pretty_value v =        (* rem. trailing white and add a space for list. *)
  let v = match List.rev v with (`White _, _) :: rv -> List.rev rv | _ -> v in
  match v with (`List _, _) :: _ as v -> (`White " ", Dict.empty) :: v | v -> v

let push_binding k v res =
  (`List (k :: pretty_value v), Dict.empty) :: (`White "\n", Dict.empty) :: res

let update_map replace es (`Atom k, _ as key) v =
  if not replace then List.rev (push_binding key v (List.rev es)) else
  let rev_es = List.rev es in
  let rec aux acc = function                  (* try to update last binding. *)
    | (`List ((`Atom k', _) :: _), d) :: rest when k' = k ->
	List.rev_append ((`List (key :: pretty_value v), d) :: rest) acc
    | e :: es -> aux (e :: acc) es
    | [] -> List.rev (push_binding key v rev_es)
  in
  aux [] rev_es

let set _ inf id key update = match Map_db.find_bset id with
| None -> Wlog.err (`Undefined_map id)
| Some m ->
    let errs = Wlog.Private.errors () in
    match Sefile.input_full_se_list inf with None -> ()       (* file error. *)
    | Some (v, _) ->
	if errs <> Wlog.Private.errors () then () (* parse error *) else
	match Map_db.filename id with
	| None -> Wlog.err (`Undefined_map id)       (* file may have moved. *)
	| Some f ->
	    match Sefile.input_full_se_list f with None -> ()
	    | Some (es, _) ->
		match Sysm.tmp_file f with None -> ()
		| Some tf ->
		    Sefile.output_full_se_list tf (update_map update es key v);
		    ignore (Sysm.rename tf f)

(* Command line interface *)

open Cmdliner

let doc = "Replace the last binding for $(i,KEY) in the map."
let replace = Arg.(value & flag & info ["r"; "replace"] ~doc)

let doc = "set the value of a map key"
let man = [
  `S "DESCRIPTION";
  `P "The command $(b,set) sets the value bound to $(i,KEY) in the map
      $(i,ID). The value is read from stdin or from a file specified with the
      option $(b,--input).";
  `P "The new binding is added at the end of the map file or, if
      there is already a binding for $(i,KEY) in the file and the option
      $(b,--replace) is present, the last binding for $(i,KEY) in the file
      is replaced.";
   ] @ C.copts_man @ [
  `S "SEE ALSO";
  `P "$(b,webglue-get)(1), $(b,webglue-maps)(5)" ]

let cmd =
  Cmd.v (Cmd.info "set" ~sdocs:C.copts_sec ~doc ~man)
    Term.(const set $ C.copts $ C.input $ C.id $ C.key $ replace)
