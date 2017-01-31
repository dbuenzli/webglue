(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

let pp_bindings = Fmt.pp_list Se.pp
let pp_value ppf v =
  Fmt.pp ppf "@[%a@]@," (Fmt.pp_list ~pp_sep:Fmt.pp_sp Se.pp) v

let bindings m = Se.to_list_list (Bset.to_bindings m)
let bindings_eval'd c m =
  let eval acc k _ _ = Se.list ((k :> se) :: (fst (Wmap.get c m k))) :: acc in
  (List.rev (Bset.fold eval [] m))

let out_vals outf m find to_bindings id k = match k with
| None -> C.out_v outf pp_bindings (to_bindings m)
| Some k ->
    match find m k with
    | Some (v, _) -> C.out_v outf pp_value v
    | None ->
	let id = Se.with_dict ~d:(Bset.dict m) (id :> se) in
	Wlog.err (`Undefined_key (id, k))

let get c outf k out_kind =
  let id = c.C.id and c = c.C.c and dep_base = c.C.copts.C.dep_base in
  let find = if out_kind = `Raw then Map_db.find_bset else Wctx.find_map c in
  match find id with
  | None -> Wlog.err (`Undefined_map id)
  | Some m ->
      match out_kind with
      | `Regular -> out_vals outf m Bset.find bindings id k
      | `Raw -> out_vals outf m Bset.find bindings id k
      | `Eval -> out_vals outf m (Wmap.find c) (bindings_eval'd c) id k
      | `Deps -> C.out_deps outf dep_base (Wmap.creation_deps c m)

(* Command line interface *)

open Cmdliner

let out_kind =
  let doc = "Show raw value, before binding inclusion and format definition." in
  let raw = `Raw, Arg.info ["raw"] ~doc in
  let doc = "Show evaluated value." in
  let eval = `Eval, Arg.info ["eval"] ~doc in
  Arg.(value & vflag `Regular [raw; eval; C.deps])

let doc = "show the value of a map key"
let man = [
  `S "DESCRIPTION";
  `P "The command $(b,get) shows the value bound to $(i,KEY) in the map
      $(i,ID). If no $(i,KEY) is provided, every binding of $(i,ID) is shown.";
  `P "If the option $(b,--eval) is present, the directives of the value
      are evaluated (see $(b,webglue-directives)(7)). The configuration atom
      $(b,w.conf) and locale $(b,w.loc) are as specified by
      the $(b,--conf) and $(b,--locale) options and the atom $(b,w.id) is
      set to $(i,ID). False errors may occur during the evaluation
      because the context may not be adequate.";
  `P "If the option $(b,--raw) is present, values are shown as found
      in the raw text file, before binding inclusion and format definition
      (see $(b,webglue-maps)(5)).";
  `P "If the option $(b,--deps) is present, the set of dependencies
      needed to define the keys of the map $(i,ID) is shown.";
   ] @ C.copts_man @ [
  `S "SEE ALSO";
  `P "$(b,webglue-directives)(7), $(b,webglue-maps)(5), $(b,webglue-set)(1)" ]

let info = Term.info "get" ~sdocs:C.copts_sec ~doc ~man
let cmd =
  Term.(pure get $ C.context ~locale_opt:true $ C.output $ C.key_opt $
          out_kind), info

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
