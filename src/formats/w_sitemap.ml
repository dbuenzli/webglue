(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

module Sitemap = struct
  include Wformat.Default
  include Wformat.Standard_uris

  let name = Se.atom "w.sitemap"
  let doc = "URI to sitemap documents for search engines"
  let man = [
    Wformat.man_description;
    `P "The format $(b,w.sitemap) is an URI containing a sitemap for search
        engines (see $(i,http://www.sitemaps.org)).";
    Wformat.man_uri_set ] @
    Wformat.Standard_uris.man @ [
    Wformat.man_uri_content;
    `P "The content is an XML sitemap that indexes all URIs of the maps
        mentioned in the $(b,roots) key aswell as the URIs of the maps
        that are needed to define these URIs and recursively.";
    `P "The maps can interact with this process by defining a few keys:";
    `I ("$(b,sm-indexed)", "if false the URI does not appear in the sitemap");
    `I ("$(b,sm-lastmod)", "last modification date of the URI.");
    `I ("$(b,sm-changefreq)", "change frequency of the URI, must be one
	 of always, hourly, daily, weekly, monthly, yearly, never");
    `I ("$(b,sm-priority)", "priority of this URI relative to other URI of
        the site");
    `P "If these keys are defined in the map for the sitemap itself, they
	defined the default value when the keys are not present in the
        indexed maps.";
    `S "BUGS";
    `P "Markup delimiters are not entity escaped for now." ]


  let k_uri_base = Bset.key "uri-base"
  let k_roots = Bset.key "roots"
  let k_sm_indexed = Bset.key "sm-indexed"
  let k_sm_lastmod = Bset.key "sm-lastmod"
  let k_sm_changefreq = Bset.key "sm-changefreq"
  let k_sm_priority = Bset.key "sm-priority"

  let keys = keys @ [
    k_uri_base, `Required,
    "The base URI of the website.";
    k_roots, `Required,
    "Lists of map IDs to recursively look for URIs.";
    k_sm_indexed, `Optional (Some [Se.atom "true"]),
    "Default value for the sm-indexed key of indexed maps.";
    k_sm_lastmod, `Required,
    "Default value for the sm-lastmod key of indexed maps.";
    k_sm_changefreq, `Optional (Some [Se.atom "monthly"]),
    "Default value for the sm-changefreq key of indexed maps.";
    k_sm_priority, `Optional (Some [Se.atom "0.5"]),
    "Default value for the sm-priority key of the indexed maps." ]

  let roots c m =                                      (* Root ids in a set. *)
    let add_id s id = Aset.add id s in
    let p_aset = Se.p_fold add_id (fun _ -> Aset.empty) (Se.p_atom "map ID") in
    let b, d = Wmap.get c m k_roots in
    Se.parse ~err:Aset.empty p_aset (Se.list ~d b)

  let pct_encode ?(b = Buffer.create 256) s =
    let reserved i =
      i = 0x2F || i = 0x21  || i = 0x2A || i = 0x27 || i = 0x28 || i = 0x29 ||
      i = 0x3B || i = 0x3A  || i = 0x40 || i = 0x26 || i = 0x3D || i = 0x2B ||
      i = 0x24 || i = 0x2C  || i = 0x3F	|| i = 0x23 || i = 0x5B	|| i = 0x5D
    in
    let unreserved i =
      0x41 <= i && i <= 0x5A || 0x61 <= i && i <= 0x7A ||
      0x30 <= i && i <= 0x39 || i = 0x2D || i = 0x2E || i = 0x5F || i = 0x7E
    in
    Buffer.clear b;
    for k = 0 to (String.length s) - 1 do
      let i = Char.code s.[k] in
      if unreserved i || reserved i then Buffer.add_char b s.[k] else
      (Buffer.add_char b '%'; Buffer.add_string b (Printf.sprintf "%02X" i))
    done;
    Buffer.contents b

  let uri_deps c um u seen acc =               (* map dependencies of an uri. *)
    let add d acc = match d with `Other _ | `File _ -> acc
    | `Map id | `Val (id, _) ->
	if Aset.mem id seen then acc else Aset.add id acc
    in
    Wdep.Set.fold add (Wmap.uri_deps c um u) acc

  let p_changefreq =
    let vs = ["always";"hourly";"daily";"weekly";"monthly";"yearly";"never"] in
    Se.p_enum (List.map (fun v -> (v, v)) vs)

  let out_uri o c sm um base seen acc u =
    let c = Wctx.with_locale c (Wuri.locale u) in
    let get k err p =
      let v, d = match Wmap.find c um k with
      | None -> Wmap.get c sm k | Some b -> b
      in
      fst (Se.pnext ~err p v d)
    in
    if not (get k_sm_indexed false Se.p_bool) then acc else
    let `Atom uri, _ = Wuri.uri u in
    let uri = pct_encode (base ^ uri) in
    let lastmod = get k_sm_lastmod SeUTC.unix_epoch SeUTC.p_timestamp in
    let changefreq = get k_sm_changefreq "monthly" p_changefreq in
    let priority = get k_sm_priority 0.5 Se.p_float in
    Out.s o " <url>";
    Out.s o "<loc>"; Out.s o uri; Out.s o "</loc>";
    Out.s o "<lastmod>"; Out.s o (SeUTC.timestamp_to_rfc3339 lastmod);
    Out.s o"</lastmod>";
    Out.s o "<changefreq>"; Out.s o changefreq; Out.s o "</changefreq>";
    Out.s o "<priority>"; Out.s o (Printf.sprintf "%G" priority);
    Out.s o "</priority>";
    Out.s o "</url>\n";
    uri_deps c um u seen acc

  let rec out_uris o c sm base seen todo =
    match try Some (Aset.choose todo) with Not_found -> None with
    | None -> ()
    | Some id when Aset.mem id seen ->
	out_uris o c sm base seen (Aset.remove id todo)
    | Some id ->
	let pp_sep = Format.pp_print_space in
	let pp_aset = Aset.pp ~pp_sep Se.pp_atom in
	let todo' = match Wctx.find_map c id with
	| None -> Wlog.err (`Undefined_map id); todo
	| Some um ->
	    Wuri.Set.fold (out_uri o c sm um base seen) todo (Wmap.uri_set c um)
	in
	Wlog.debug (
	Wlog.msg "doing:%a@\ntodo: @[%a@]@\ntodo':@[%a@]@\nseen:@[%a@]"
		Se.pp_atom id pp_aset todo pp_aset todo' pp_aset seen);

	out_uris o c sm base (Aset.add id seen) (Aset.remove id todo')

  let uri_content c sm uri dest =
    let base = Setext.to_string (fst (Wmap.get c sm k_uri_base)) in
    let roots = roots c sm in
    let o = Out.make dest in
    Out.s o "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    Out.s o "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n";
    out_uris o c sm base (Aset.singleton (Wmap.id sm)) roots;
    Out.s o "</urlset>\n"

end

let () = Wformat.define (module Sitemap : Wformat.T)


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
