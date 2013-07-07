(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

(* TODO document and verify ! sexp for comments, escape ?  *)

(** S-expressions as CSS data. 

    Outputs UTF-8 encoded CSS data from S-expressions according to
    {{:#seascss}this interpetation}. *)
module SeCSS : sig
  (** {1 CSS output} *)
  
  val output : Out.output -> se list -> unit
  (** [output o sl] outputs the CSS interpretation of [sl] on [o]. *)

    (** {1:seascss S-expressions as CSS data} 
	
	The following interpretation of lists of S-expressions as CSS
	statements is just an alternate {e syntax} for specifying CSS
	data. Checking the validity of generated data or specifying 
	CSS's abstract syntax tree precisely is a non-goal.

	CSS documents are sequences of statements. Each statement
	is either an \@-rule or a ruleset.

	{2:atrules \@-rules and blocks}
      
	An {{:http://www.w3.org/TR/CSS21/syndata.html#at-rules}@-rule} is
	an @-keyword followed by optional data an an optional
	{{:http://www.w3.org/TR/CSS21/syndata.html#block}block}. We
	represent them by S-expressions with the following structure: 
	{v (\@ keyword data atoms (block)) v}
      
	The S-expression starts with the [@] atom, followed by the
	@-keyword identifier, continues with optional data atoms, the
	optional data, and ends with a optional list representing the
	block.  In {!SeCSS} the content of the block is a list of
	S-expressions representing CSS statements.
      
	A space is automatically inserted between data atoms, it can be
	suppressed by adding the [@] atom between them.
      
	Examples :
{v (\@ media print ()) 
==> \@media print \{\}

(\@ media screen and "(" device-width: 800px ")" ())
==> \@media screen and ( device-width: 800px ) \{\}

(\@ import "url(color.css)" screen and "(color)")
(\@ import url @ "(" @ color.css @ ")" screen and "(color)")
==> 
\@import url(color.css) screen and (color);
\@import url(color.css) screen and (color);
v}

	{2:ruleset Rule sets}

	A {{:http://www.w3.org/TR/CSS21/syndata.html#rule-sets}rule set}
	is an optional selector followed by a sequence of
	{{:http://www.w3.org/TR/CSS21/syndata.html#declaration}declarations}. A
	declaration is a property name followed by a value. We represent
	rule sets by S-expressions with the following structure: {v
	(selector atoms (prop1 value1 atoms) (prop2 value2 atoms) ...) v}
	
	The S-expression starts with data atoms, the selector, and
	continues with a possibly empty sequence of lists of data
	atoms. In these lists the first atom is the property name and the
	rest of the atoms the property data.
	
	As usual a space is automatically introduced between data atoms,
	it can be removed by introducing an @ atom between them.
	
	Examples :
{v (body) 
==> body \{\}

(body (font-size 1em))
==> body \{ font-size: 1em; \}

(.content p:first-child (text-indent 0em) (font-size 2em))
==> .content p:first-child \{ text-indent: 0em; font-size: 2em; \}

(div @ .nav (text-transform uppercase) (color blue) 
            (margin 2 @ em 1em 1em 2em))
==> div.nav \{ text-transform: uppercase; color: blue; 
               margin: 2em 1em 1em 2em; \}
v}*)
end = struct

  let rec output_declarations o decls = 
    let rec out_data o data = match Setext.output ~until_list:true o data with
    | [] -> Out.c o ';'
    | (_, d) :: rest -> 
	Wlog.err (Wlog.msg ~d "TODO"); out_data o rest
    in
    match decls with
    | (`List ((`Atom prop, _) :: data), _) :: rest -> 
	Out.s o prop; Out.c o ':';
	out_data o data;
	output_declarations o rest
    | (_, d) :: rest ->
	Wlog.err (Wlog.msg ~d "TODO"); 
	output_declarations o rest
    | [] -> ()
	  
  let output_rule_set o el = match Setext.output ~until_list:true o el with
  | [] -> Out.s o "{}\n";
  | decls -> Out.s o "{"; output_declarations o decls; Out.s o "}\n"
	
  let rec output_at_rule o k el = 
    Out.c o '@'; Out.s o k; Out.c o ' '; 
    match Setext.output ~until_list:true o el with
    | [] -> Out.s o ";\n"; None
    | (`List block, _) :: rest ->
	let err (_, d) = Wlog.err (Wlog.msg ~d "TODO") in
	List.iter err rest;
	Out.s o " {"; Some block
    | _ -> assert false
	  
  let output_statement_list o el =
    let rec aux o = function 
      | ((`List ((`Atom "@", _) :: (`Atom k, _) :: rl), dict) :: sl) :: up ->
	  begin match (output_at_rule o k rl) with 
	  | None -> aux o (sl :: up)
	  | Some block -> aux o (block :: sl :: up) (* down *)
	  end
      | ((`List ((`Atom "!", _) :: comment), _) :: es) :: up ->
	  Out.s o "/*"; ignore (Setext.output o comment); Out.s o "*/\n";
	  aux o (es :: up)
      | ((`List rs, _) :: sl) :: up -> 
	  output_rule_set o rs;
	  aux o (sl :: up)
      | ((`Atom _, d) :: sl) :: up ->
	  Wlog.err (Wlog.msg ~d "TODO");
	  aux o (sl :: up)
      | [] :: [] -> ()
      | [] :: up -> Out.s o "}\n"; aux o up             (* close block. *)
      | [] -> assert false
    in
    aux o (el :: [])
      
  let output o el = output_statement_list o el
end

module CSS = struct
  include Wformat.Default
  include Wformat.Standard_uris 

  let name = Se.atom "w.css"
  let doc = "URIs to CSS documents"
  let man = [
    Wformat.man_description;
    `P "The format $(b,w.css) defines URIs to CSS documents whose 
        statements are described the S-expression syntax given below.";
    Wformat.man_uri_set; ] @ 
    Wformat.Standard_uris.man @ [
    Wformat.man_uri_content;
    `P "The content of an URI is defined by the CSS interpretation of
        the [doc] key.";
    `P "TODO document the interpretation"; ]

  let keys = keys @ [
   Bset.Key.doc, `Required,
    "list of S-expressions interpreted as CSS statements (see below)."; ]

  let uri_content c m uri dest =
    SeCSS.output (Out.make dest) (fst (Wmap.get c m Bset.Key.doc))
end

let () = Wformat.define (module CSS : Wformat.T)

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
