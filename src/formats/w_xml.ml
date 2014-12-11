(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg;;

module XML = struct
  include Wformat.Default
  include Wformat.Standard_uris

  let name = Se.atom "w.xml"
  let doc = "URIs to UTF-8 encoded XML or HTML documents"
  let man = [
    Wformat.man_description;
    `P "The format $(b,w.xml) defines UTF-8 encoded XML or HTML
        documents whose root element is defined by the S-expression
        interpretation of the $(b,doc-root) key (see below).";
    Wformat.man_uri_set; ] @
    Wformat.Standard_uris.man @ [
    Wformat.man_uri_content;
    `P "The content of an URI is defined by writing the @-text defined by the
	key $(b,doc-preamble) followed the XML interpretation of the
	$(b,doc-root) S-expression.";
    `P "If the key $(b,doc-html-markup) is defined, HTML markup is generated
	instead of XML (differs only in closing tag and void element
        subltelties).";
    `P "TODO document interpretation."]

  let k_doc_preamble = Bset.key "doc-preamble"
  let k_doc_html_markup = Bset.key "doc-html-markup"
  let k_doc_root = Bset.key "doc-root"
  let keys = keys @ [
    k_doc_preamble, `Optional None,
    "@-text output before the root element.";
    k_doc_root, `Required,
    "the root element of the XML document.";
    k_doc_html_markup, `Optional (Some [Se.atom "false"]),
    "if true html markup is generated."; ]

  let uri_content c m uri dest = (* TODO review *)
    let o = Out.make dest in
    begin match Wmap.find c m k_doc_preamble with
    | None -> () | Some text -> ignore (Setext.output o (fst text))
    end;
    let html =
      let v, d = Wmap.get c m k_doc_html_markup in
      fst (Se.pnext ~err:false Se.p_bool v d)
    in
    let se =
      let v, d = Wmap.get c m k_doc_root in
      fst (Se.pnext ~err:(`List [], Dict.empty) (Se.p_list "root element") v d)
    in
    SeXML.output ~html o (se :> se)

end

let () = Wformat.define (module XML : Wformat.T)


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
