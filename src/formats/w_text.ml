(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg;;

module Text = struct
  include Wformat.Default
  include Wformat.Standard_uris

  let name = Se.atom "w.text"
  let doc = "URIs to plain UTF-8 encoded text documents"
  let man = [
    Wformat.man_description;
    `P "The format $(b,w.text) generates plain, UTF-8 encoded text documents
        by interpreting the S-expression list of the $(b,doc) key.";
    Wformat.man_uri_set; ] @
    Wformat.Standard_uris.man @ [
    Wformat.man_uri_content;
    `P "The URI content is defined by the list of S-expression in [doc]. Each
	S-expression is interepeted as @-text and defines one line of the
	document." ]

  let keys = keys @ [
    Bset.Key.doc, `Required,
    "list of @-text defining each line of the document.";]

  let uri_content c m uri dest =
    Setext.output_lines (Out.make dest) (fst (Wmap.get c m Bset.Key.doc))
end

let () = Wformat.define (module Text : Wformat.T)
