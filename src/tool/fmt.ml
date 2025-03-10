(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

(* Formatters *)

type 'a formatter = Format.formatter -> 'a -> unit

let pp fmt = Format.fprintf fmt
let pp_cut = Format.pp_print_cut
let pp_sp = Format.pp_print_space
let pp_str = Format.pp_print_string
let pp_opt ?(pp_none = fun ppf () -> ()) pp_v ppf = function
  | None -> pp_none ppf ()
  | Some v -> pp_v ppf v

let rec pp_list ?(pp_sep = pp_cut) pp_v ppf = function
  | v :: vs -> pp_v ppf v; pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs
  | [] -> ()

let pp_white_str ~spaces ppf s =
  let left = ref 0 and right = ref 0 and len = String.length s in
  let flush () =
    Format.pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    if s.[!right] = '\n' then (flush (); Format.pp_force_newline ppf ()) else
    if spaces && s.[!right] = ' ' then (flush (); Format.pp_print_space ppf ())
    else incr right;
  done;
  if !left <> len then flush ()

let pp_text = pp_white_str ~spaces:true
let pp_lines = pp_white_str ~spaces:false

let pp_range ppf ((l0, c0), (l1, c1)) = pp ppf "%d.%d-%d.%d" l0 c0 l1 c1

let pp_se_list_ast ppf sl =
  let rec pr_se ppf se =
    let range = match (Dict.find (snd se) Dict.Key.char_range) with
    | Some r -> r | None -> assert false
    in
    match se with
    | `Atom a, _ -> pp ppf "@[<5>Atom(%a,@ '%s')@]" pp_range range a
    | `List l, _ ->
	let pp_sep ppf () = pp ppf ";@ " in
	pp ppf "@[<5>List(%a,@ [%a])@]" pp_range range (pp_list ~pp_sep pr_se) l
  in
  pp ppf "@[<v>%a@]@?" (pp_list pr_se) sl

(* String converters *)

let str = Format.sprintf
let to_str_converter pp_v v =
  Format.fprintf Format.str_formatter "%a" pp_v v;
  Format.flush_str_formatter ()
