(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
