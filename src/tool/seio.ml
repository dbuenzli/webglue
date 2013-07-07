(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

let u_bsl  = 0x005C (* backslash *)
let u_lf   = 0x000A (* line feed *)
let u_lpar = 0x0028 (* left paren *)
let u_n    = 0x004E (* n *)
let u_quot = 0x0022 (* double quote *)
let u_rpar = 0x0029 (* right paren *)
let u_semi = 0x003B (* semi colon *)

let push e = function el :: acc' -> (e :: el) :: acc' | _ -> assert false 
let push_opt e acc = match e with None -> acc | Some e -> push e acc

(* S-expression type *)

type 'a t = [ `Atom of string | `List of 'a t list ] * 'a 

(* Input *)

type pos = int * int
type range = pos * pos
type error = [ 
  | `Malformed_char | `Illegal_escape of int | `Illegal_char of int 
  | `Mismatched_par | `Unclosed_quote | `Unclosed_par ]
      
let error_message = function
  | `Malformed_char -> "malformed character"
  | `Mismatched_par -> "right parenthesis mismatch"
  | `Unclosed_quote -> "unclosed quote"
  | `Unclosed_par -> "unclosed left parenthesis"
  | `Illegal_escape i ->
      Printf.sprintf "illegal escape character (%s)" (Ucutf.cp_to_string i)
  | `Illegal_char i ->
      Printf.sprintf "character %s is illegal here" (Ucutf.cp_to_string i)

type 'a input = 
    { i : Ucutf.input; 
      b : Buffer.t; 
      annot : range -> 'a; 
      err : error -> pos -> unit }

let pos i = Ucutf.pos i.i
let err i e p = i.err e p
let err_malformed_char i = err i `Malformed_char (pos i) 
let err_escape i u = err i (`Illegal_escape u) (pos i) 
let err_unclosed_qtoken i pos = err i `Unclosed_quote pos 
let err_mismatch i = err i `Mismatched_par (pos i)
let err_unclosed_list i pos = err i `Unclosed_par pos 
let err_illegal i u = err i (`Illegal_char u) (pos i) 

let badd i u = Ucutf.add_utf8 i.b u
let bdata i = let d = Buffer.contents i.b in Buffer.clear i.b; d
let next i = ignore (Ucutf.input i.i)
let peek i = match Ucutf.peek i.i with 
| `Malformed -> err_malformed_char i; `Uchar Ucutf.u_rep
| (`Eoi | `Uchar _ as c) -> c

let is_white = function 0x0020 | 0x0009 | 0x000D | 0x000A -> true | _ -> false
let is_token_end = function 
  | 0x0020 | 0x0009 | 0x000D | 0x000A (* whitechars *)
  | 0x0022 | 0x0028 | 0x0029 | 0x003B | 0x005C -> true 
  | _ -> false
	
let skip_comment i =
  let rec aux i = match peek i with 
  | `Uchar u when u <> u_lf -> next i; aux i
  | _ -> ()
  in
  next i; aux i; next i; None
    
let skip_white i _ = 
  let rec aux i = match peek i with 
  | `Uchar u when is_white u -> next i; aux i 
  | _ -> ()
  in
  next i; aux i; None
    
let p_comment i = 
  let rec aux i = match peek i with 
  | `Uchar u when u <> u_lf -> next i; badd i u; aux i
  | _ -> `Comment (bdata i)
  in
  next i; let sp = pos i in let c = aux i in let ep = pos i in 
  next i; Some (c, i.annot (sp, ep))
    
let p_white i u = 
  let rec aux i = match peek i with
  | `Uchar u when is_white u -> next i; badd i u; aux i
  | _ -> `White (bdata i)
  in
  next i; badd i u; let sp = pos i in let ws = aux i in let ep = pos i in 
  Some (ws, i.annot (sp, ep))
    
let p_qtoken i = 
  let rec aux i sp = match peek i with
  | `Uchar u when u = u_bsl ->
      begin next i; match peek i with
      | `Uchar u when u = u_bsl || u = u_quot -> next i; badd i u; aux i sp
      | `Uchar u when u = u_n -> next i; badd i u_lf; aux i sp 
      | `Uchar u -> next i; err_escape i u; badd i Ucutf.u_rep; aux i sp
      | `Eoi -> err_unclosed_qtoken i sp; `Atom (bdata i)
      end
  | `Uchar u when u <> u_quot -> next i; badd i u; aux i sp
  | `Uchar _ -> `Atom (bdata i)
  | `Eoi -> err_unclosed_qtoken i sp; `Atom (bdata i)
  in
  next i; let sp = pos i in let qtok = aux i sp in 
  next i; let ep = pos i in 
  qtok, i.annot (sp, ep)
    
let p_token i u =
  let rec aux i = match peek i with
  | `Uchar u when not (is_token_end u) -> next i; badd i u; aux i
  | _ -> `Atom (bdata i)
  in
  next i; badd i u; let sp = pos i in let tok = aux i in let ep = pos i in
  tok, i.annot (sp, ep)
    
let p_list_start i pl acc k = next i; k i (pos i :: pl) ([] :: acc)
let p_list_end i pl acc k = next i; match pl, acc with 
| ps :: pl', el :: acc' -> 
    let list = `List (List.rev el), i.annot (ps, pos i) in
    k i pl' (push list acc') 
| [], acc -> err_mismatch i; k i [] acc
| _ -> assert false
      
let p_eoi i pl acc k = match pl, acc with 
| [], l :: [] -> List.rev l
| sp :: _ as pl, acc -> err_unclosed_list i sp; p_list_end i pl acc k
| _ -> assert false
      
let rec p_sexp p_white p_comment p_eoi i pl acc k = match peek i with
| `Uchar u when is_white u -> k i pl (push_opt (p_white i u) acc)
| `Uchar u when u = u_semi -> k i pl (push_opt (p_comment i) acc)
| `Uchar u when u = u_quot -> k i pl (push (p_qtoken i) acc)
| `Uchar u when u = u_rpar -> p_list_end i pl acc k
| `Uchar u when u = u_lpar -> p_list_start i pl acc k
| `Uchar u when u = u_bsl -> 
    next i; err_illegal i u; p_sexp p_white p_comment p_eoi i pl acc k
| `Uchar u -> k i pl (push (p_token i u) acc)
| `Eoi -> p_eoi i pl acc k
      
let rec p_sexp_list i pl acc = 
  p_sexp skip_white skip_comment p_eoi i pl acc p_sexp_list
    
let make_input err annot i = 
  { i = i; b = Buffer.create 512; annot = annot; err = err}
    
let input ?(err = fun _ _ -> ()) annot i = 
  p_sexp_list (make_input err annot i) [] ([] :: [])
    
(* Output *)
    
type dest = [ 
  | `Channel of out_channel | `Buffer of Buffer.t | `Fun of (int -> unit) ]
      
type output = 
    { outc : char -> unit;                               (* character output. *)
      outs : string -> int -> int -> unit;                  (* string output. *)
      b : Buffer.t }
      
let make_output d = 
  let outc, outs = match d with 
  | `Channel c -> (output_char c), (output c)
  | `Buffer b -> (Buffer.add_char b), (Buffer.add_substring b)
  | `Fun f -> 
      let oc c = f (Char.code c) in 
      let os s p l = 
        for i = p to p + l - 1 do f (Char.code (String.get s p)) done 
      in
      oc, os
  in
  { outc = outc; outs = outs; b = Buffer.create 512 }
    
let quote_token b s =                         (* quotes the token if needed. *)
  let doit b s i len =
    Buffer.clear b; Buffer.add_char b '\"'; Buffer.add_substring b s 0 i;
    let sp = ref i in
    let add_upto b s j = Buffer.add_substring b s !sp (j - !sp); sp := j + 1 in
    for j = i to len - 1 do match s.[j] with 
    | '\n' -> add_upto b s j; Buffer.add_string b "\\n"
    | '\\' -> add_upto b s j; Buffer.add_string b "\\\\"
    | '\"' -> add_upto b s j; Buffer.add_string b "\\\""
    | c -> ()
    done;
    if !sp < len then add_upto b s len;
    Buffer.add_char b '\"'
  in 
  try
    let len = String.length s in
    for i = 0 to len - 1 do
      if is_token_end (Char.code s.[i]) then (doit b s i len; raise Exit)
    done;
    s                                                    (* no need to quote. *)
  with Exit -> Buffer.contents b

let output o e = 
  let outs o s = o.outs s 0 (String.length s) in
  let rec aux o was_atom = function
    | ((`Atom s, _) :: el) :: todo ->
	if was_atom then o.outc ' ';
	outs o (quote_token o.b s);
	aux o true (el :: todo)
    | ((`List l, _) :: el)  :: todo -> o.outc '('; aux o false (l :: el :: todo)
    | [] :: [] -> ()
    | [] :: todo -> o.outc ')'; aux o false todo
    | [] -> assert false
  in
  aux o false ([e] :: [])

let print ppf e =
  let rec aux b ppf space = function
    | ((`Atom s, _) :: el) :: todo ->
	if space then Format.pp_print_space ppf ();
	Format.pp_print_string ppf (quote_token b s);
	aux b ppf true (el :: todo)
    | ((`List l, _) :: el) :: todo -> 
	if space then Format.pp_print_space ppf ();
	Format.pp_open_hovbox ppf 1;
	Format.pp_print_char ppf '(';
	aux b ppf false (l :: el :: todo)
    | [] :: [] -> ()
    | [] :: todo ->
	Format.pp_print_char ppf ')'; Format.pp_close_box ppf (); 
	aux b ppf true todo
    | _ -> assert false
  in
  let b = Buffer.create 512 in
  aux b ppf false ([e] :: [])

let to_string  e =                                       (* NOT thread safe. *)
  Format.fprintf Format.str_formatter "%a" print e;
  Format.flush_str_formatter ()

(* Full representation *)

module Full = struct
  type 'a se = 'a t
  type 'a t = [ 
    | `Atom of string | `List of 'a t list | `White of string 
    | `Comment of string ] * 'a 
	
  let rec p_sexp_list i pl acc =
    p_sexp p_white p_comment p_eoi i pl acc p_sexp_list
      
  let input ?(err = fun _ _ -> ()) annot i =
    p_sexp_list (make_input err annot i) [] ([] :: [])

  let output o e = 
    let outs o s = o.outs s 0 (String.length s) in
    let rec aux o was_atom = function
      | ((`White w, _) :: el) :: todo -> 
	  outs o w; 
	  aux o false (el :: todo)
      | ((`Atom s, _) :: el) :: todo ->
	  if was_atom then o.outc ' ';
	  outs o (quote_token o.b s);
	  aux o true (el :: todo)
      | ((`List l, _) :: el)  :: todo -> 
	  o.outc '('; 
	  aux o false (l :: el :: todo)
      | ((`Comment c, _) :: el) :: todo ->
          o.outc ';'; outs o c; o.outc '\n'; 
	  aux o false (el :: todo)
      | [] :: [] -> ()
      | [] :: todo -> 
	  o.outc ')'; 
	  aux o false (todo)
      | [] -> assert false
    in
    aux o false ([e] :: [])

  let print ppf e =
    let rec aux b ppf space = function
      | ((`White w, _) :: el) :: todo ->
	  let sp = ref true in
	  for i = 0 to (String.length w) - 1 do 
	    if w.[i] = '\n' then (Format.pp_force_newline ppf (); sp := false)
	  done;
	  aux b ppf !sp (el :: todo)
      | ((`Atom s, _) :: el) :: todo ->
	  if space then Format.pp_print_space ppf ();
	  Format.pp_print_string ppf (quote_token b s);
	  aux b ppf true (el :: todo)
      | ((`List l, _) :: el) :: todo -> 
	  if space then Format.pp_print_space ppf ();
	  Format.pp_open_hovbox ppf 1;
	  Format.pp_print_char ppf '(';
	  aux b ppf false (l :: el :: todo)
      | ((`Comment c, _) :: el) :: todo ->
	  Format.pp_print_string ppf (";" ^ c);
	  Format.pp_force_newline ppf ();
	  aux b ppf true (el :: todo)
      | [] :: [] -> ()
      | [] :: todo ->
	  Format.pp_print_char ppf ')'; Format.pp_close_box ppf (); 
	  aux b ppf true todo
      | _ -> assert false
    in
    let b = Buffer.create 512 in
    aux b ppf false ([e] :: [])

  let to_string e =                                     (* NOT thread safe. *)
    Format.fprintf Format.str_formatter "%a" print e;
    Format.flush_str_formatter ()
end

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
