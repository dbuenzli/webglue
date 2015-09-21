(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

(* Format manpages *)

let format_manual = "Webglue Formats Manual"
let man_of_format f = match Wformat.Private.find (Se.atom f) with
| None -> `Error (false, (Fmt.str "map format %s, invalid definition" f))
| Some fM ->
    let module F = (val fM : Wformat.T) in
    let title =
      let `Atom name, _  = F.name in
      let left_foot = (Fmt.str "%s %s" name F.version) in
      let center_head = format_manual in
      let name = String.uppercase name in
      name, 5, "", left_foot, center_head
    in
    `Ok (title, Wformat.man fM)

(* Additional man pages *)

let webglue_version = "Webglue %%VERSION%%"
let maps_page =
  ("WEBGLUE-MAPS", 5, "", webglue_version, format_manual), [
   `S "NAME";
   `P "webglue-maps - information about webglue map files";
   `S "DESCRIPTION";
   `P "A map file is an UTF-8 encoded text file with a $(b,.map) suffix. The
      file name without the suffix defines the $(b,map ID).";
   `P "A map file contains a sequence of whitespace, comments and
       s-expression lists defining $(b,data bindings) (see below). For more
       information about the syntax of comments and s-expressions see
       $(b,webglue-s-expressions)(5).";
   `S "MAP SEARCH";
   `P "Maps are searched by ID in directories specified with
       $(b,--map-dir) options and via the $(i,WEBGLUE_MAP_PATH) environment
       variable, see $(b,webglue)(1). Directories are searched recursively.";
   `P "If no directories are specified at all, webglue automatically
       tries to find maps in a 'maps' directory in the current working
       directory.";
   `S "KEYS, VALUES AND DATA BINDINGS";
   `P "Each s-expression list in the map file defines a $(b,data binding),
       it has the form:";
   `P "($(b,key) $(i,e1) $(i,e2) ...)";
   `P "The first atom of the list is the $(b,key), and the remaining sequence of
       s-expressions is the $(b,value) the key binds to. The order of
       bindings is not significant, except if a key is defined more than
       once. In that case the last definition takes precedence (globally).";
   `P "On key lookup the expressions $(i,e1), $(i,e2) ... are evaluated
       according to a data language, see $(b,webglue-directives)(7) for
       details.";
   `P "The value of keys can be shown and set with the $(b,webglue-get)(1) and
       $(b,webglue-set)(1) commands.";
   `P "To ensure compatibility with future versions of webglue avoid keys
       prefixed by the $(b,w.) string.";
   `S "BINDING INCLUSION";
   `P "The bindings of other maps can be textually included with the
       $(b,w.includes) key whose value must be a list of atoms:";
   `P "($(b,w.includes) id1 id2 ...)";
   `P "This takes the bindings of the maps id1, id2, ... and includes
       them textually and in that order, as if
       they were written before the bindings of the includer map.";
   `P "Note that the $(b,w.self) atoms (see $(b,webglue-directives)(7))
       of the included bindings will refer to the ID of the includer.";
   `S "MAP FORMAT";
   `P "Every map must have a $(b,w.format) key whose value must be an atom:";
   `P "($(b,w.format) f)";
   `P "The atom f defines the format of the map. The format determines the
       URIs defined by the map and their content, see below.";
   `P "The format $(b,w.map)(5) is the only built-in format. Other formats
       may be defined by plugins, see $(b,webglue-formats)(5) for details.";
   `S "URI SET";
   `P "The URI set of a map is the set of URIs it defines, it can be
       shown with the $(b,webglue-uriset)(1) command. The definition
       of the URI set depends on the map format, consult their
       documentation. The URI set of the default
       map format $(b,w.map)(5) is always empty.";
   `P "Many map formats define a single localized ressource,
       in that case the URI set is determined according to a standard
       procedure, see $(b,webglue-standard-uris)(7) for more details.";
   `S "URI CONTENT";
   `P "The content of an URI can be shown with the $(b,webglue-content)(1)
       command. URI contents are determined by the map format, consult
       their documentation.";
   `S "SEE ALSO";
   `P "$(b,webglue)(1), $(b,webglue-content)(1), $(b,webglue-directives)(7),
       $(b,webglue-formats)(5), $(b,webglue-s-expressions)(5),
       $(b,webglue-standard-uris)(5), $(b,webglue-uriset).";]

let s_expression_page =
  ("WEBGLUE-S-EXPRESSIONS", 5, "", webglue_version, format_manual), [
   `S "NAME";
   `P "webglue-s-expressions - the syntax of webglue s-expressions";
   `S "DESCRIPTION";
   `P "An s-expression is either an $(b,atom) or an $(b,s-expression list).
       A $(b,map) is a sequence of whitespace, $(b,comments) and s-expression
       lists.";
   `S "ATOMS";
   `P "Atoms are represented by tokens or quoted tokens, for example
       abc or \"abc\". A token is any sequence of unicode characters
       except whitespace, '\"', '(', ')', ';' and '$(p,\\)$(g,\\e)'.";
   `P "Given a token, its quoted form represents the same atom. However quoted
       tokens can represent atoms with characters that are forbidden in
       tokens:";
   `P "\"(hey; $(p,\\)$(g,\\e)\" ho)\"";
   `P "In quoted tokens the sequences $(p,\\)$(g,\\e)\",
       $(p,\\)$(g,\\e)$(p,\\)$(g,\\e) and $(p,\\)$(g,\\e)n are
       respectively interpreted as a double quote character ('\"', U+0022),
       a backslash character ('$(p,\\)$(g,\\e)', U+005C), or a line
       feed character ('$(p,\\)$(g,\\e)n', U+000A). Any other character
       following a $(p,\\)$(g,\\e) is an illegal sequence of characters.";
   `S "LISTS";
   `P "Lists are delimited by left and right parentheses and their elements
       are s-expressions separated by whitespace for example: ";
   `P "(a list (of four) expressions)";
   `P "The list () is the empty s-expression list.";
   `S "COMMENTS";
   `P "Outside quoted tokens anything that follows a semi-colon (';', U+003B)
       is ignored until the next line feed character ('$(p,\\)$(g,\\e)n',
       U+000A). The following defines the same list as the example above:";
   `P "(a list (of ; This is a comment"; `Noblank;
   `P "four) expressions)";
   `S "S-EXPRESSION GRAMMAR";
   `P "The grammar is defined on a stream of unicode characters by:";
   `P "\xA0\xA0\xA0\xA0<sexp> ::= <white> | <comment> | <token> | <qtoken> |
       <list>"; `Noblank;
   `P "\xA0\xA0\xA0\xA0<list> ::= <LPAR> <sexp>* <RPAR>"; `Noblank;
   `P "\xA0\xA0\xA0<token> ::= [^<tokenend>]+"; `Noblank;
   `P "\xA0\xA0<qtoken> ::= <QUOT> <qseq>* <QUOT>"; `Noblank;
   `P "\xA0\xA0\xA0\xA0<qseq> ::= [^<QUOT><BSLASH>] | <BSLASH> (<QUOT> |
       <BSLASH> | <N>)";
	 `Noblank;
   `P "\xA0\xA0\xA0<white> ::= <whitec>*"; `Noblank;
   `P "\xA0<comment> ::= <SEMI> [^<LF>]* <LF>"; `Noblank;
   `P "<tokenend> ::= <whitec> | <QUOT> | <LPAR> | <RPAR> | <SEMI> | <BSLASH>";
	 `Noblank;
   `P "\xA0\xA0<whitec> ::= <SP> | <HT> | <CR> | <LF>"; `Noblank;
   `P "\xA0\xA0<BSLASH> ::= U+005C  ('$(p,\\)$(g,\\e)')"; `Noblank;
   `P "\xA0\xA0\xA0\xA0\xA0\xA0<CR> ::= U+000D ('$(p,\\)$(g,\\e)r')"; `Noblank;
   `P "\xA0\xA0\xA0\xA0\xA0\xA0<HT> ::= U+0009 ('$(p,\\)$(g,\\e)t')"; `Noblank;
   `P "\xA0\xA0\xA0\xA0\xA0\xA0<LF> ::= U+000A ('$(p,\\)$(g,\\e)n')"; `Noblank;
   `P "\xA0\xA0\xA0\xA0<LPAR> ::= U+0028 ('(')"; `Noblank;
   `P "\xA0\xA0\xA0\xA0\xA0\xA0\xA0<N> ::= U+004E ('n')"; `Noblank;
   `P "\xA0\xA0\xA0\xA0<QUOT> ::= U+0022 ('\"')"; `Noblank;
   `P "\xA0\xA0\xA0\xA0<RPAR> ::= U+0028 (')')"; `Noblank;
   `P "\xA0\xA0\xA0\xA0<SEMI> ::= U+003B (';')"; `Noblank;
   `P "\xA0\xA0\xA0\xA0\xA0\xA0<SP> ::= U+0020  (' ')";
   `S "MAP FILE GRAMMAR";
   `P "A map file is defined by the following production:";
   `P "\xA0\xA0\xA0\xA0\xA0<map> ::= (<white> | <comment> | <binding>)*";
      `Noblank;
   `P "\xA0<binding> ::= <LPAR> (<token> | <qtoken>) <sexp>* <RPAR>";
   `P "For more information on map files see $(b,webglue-maps)(5).";
   `S "SEE ALSO";
   `P "$(b,webglue-maps)(5)"; ]

let formats_page =
  ("WEBGLUE-FORMATS", 5, "", webglue_version, format_manual), [ ]

let directives_page =
  ("WEBGLUE-DIRECTIVES", 7, "", webglue_version, "Webglue Manual"),[
   `S "NAME";
   `P "webglue-directives - webglue directives to define data";
   `S "DESCRIPTION";
   `P "Directives in data definitions are evaluated on map key lookup.";
   `P "The directives avoid data redundancy and perform
       basic data formatting. Even though they allow for some logic to be
       encoded in the data definitions, this should be avoided and minimized.
       Data processing and logic needs expanding beyond tests for absence or
       presence of data should be done via custom map formats,
       see $(b,webglue-formats)(5).";
   `P "The directives are a few constants
       ($(b,w.id), $(b,w.self), $(b,w.loc) and $(b,w.conf))
       and means to: refer to data in other maps ($(b,w.get), $(b,w.opt-get)),
       conditionalize data ($(b,w.cond)), localize data
       ($(b,w.locales)), refer to localized data ($(b,w.with-loc),
       $(b,w.opt-with-loc)),
       map a sequence of s-expressions to another sequence ($(b,w.seq)),
       format time stamps ($(b,w.time)) and generate universally unique
       ids ($(b,w.uuid)).";
   `S "CONSTANTS";
   `P "The following atoms are automatically substituted by constants.";
   `I ("$(b,w.id)",
      "Substituted with the ID of the map webglue is acting on. For example
       the map whose URI set or URI content is being determined.");
   `I ("$(b,w.self)",
       "Substituted with the ID of the map in which the binding
       is defined.");
   `I ("$(b,w.loc)",
       "Substituted with the locale that is being evaluated. Left as
	is if no locale is defined.");
   `I ("$(b,w.conf)",
       "Substituted with the configuration atom. Left as is if no configuration
        atom is defined. With the command line tool,
	the configuration atom can be specified via the $(b,--conf) option,
	see $(b,webglue)(1).");
   `S "KEY LOOKUP";
   `P "The $(b,w.get) and $(b,w.opt-get) directives lookup
       the value of a key in a map or in an s-expression list
       of bindings. The syntax is:";
   `P "($(b,w.get) $(i,em) $(i,ek)) or ($(b,w.get) $(i,ek))"; `Noblank;
   `P "($(b,w.opt-get) $(i,em) $(i,ek)) or ($(b,w.opt-get) $(i,ek))";
   `P "The s-expression $(i,em) must either evaluate to an atom that is an
       existing map ID or to an s-expression list of bindings.
       If absent $(b,w.self) is used for $(i,em).
       The s-expression $(i,ek) must evaluate to an atom, the key to look up.
       The directive is substituted by the sequence of evaluated s-expressions
       resulting from the lookup.";
   `P "If no binding is found for the key, $(b,w.get) reports an error.
       By contrast, $(b,w.opt-get) silently evaluates to the empty sequence
       (the directive is simply removed).";
   `S "CONDITIONAL";
   `P "The $(b,w.cond) directive conditionally defines data. The syntax is :";
   `P "($(b,w.cond) $(i,e) ($(i,ec1) $(i,e1) ...) ($(i,ec2) $(i,e2) ...) ...)";
   `P "The s-expression $(i,e) is evaluated to an s-expression and compared
       sequentially to the evaluation of each condition $(i,ec1), $(i,ec2), ...
       until the first that matches. The expressions $(i,ek) ...
       corresponding to the  condition $(i,eck) that first matched are
       then evaluated and the result is substituted to the directive.";
   `P "The special atom $(b, w.default), used as a condition, matches any
       s-expression. An error is reported if no condition matches.";
   `P "This directive should not be abused, complex data processing and logic
       should take place in custom map formats, see $(b,webglue-formats)(5).";
   `S "LOCALIZATION";
   `P "The $(b,w.locales) directive defines localized data. The syntax
       is :";
   `P "($(b,w.locales) ($(i,l1) $(i,e1) ...) ($(i,l2) $(i,e2) ...) ...)";
   `P "Each s-expression list defines the data for a locale range. The
       locale range is defined by the evaluation of $(i,li) which
       must be a BCP 47 extended language range tag (see RFC 4647 [1]).";
   `P "The current locale $(b,w.loc) is matched against
       all the locale ranges and the one that matches with most precision
       is selected (see below for details). The expressions $(i,ek) ...
       of the selected locale range $(i,lk) are evaluated and the result
       is substituted to the directive. If no locale range matches or
       the same range is specified twice, an error is reported.
       If no locale is defined in $(b,w.loc),
       the directive remains but all the sub-expressions are evaluated.";
   `P "A locale matches a range if the subtags of the range are a prefix
       of the locale. The wildcard subtag * matches any sequence of
       subtags (as such it cannot be used to denote that a certain number of
       subtags will appear in a matching locale). Subtag comparison is case
       insensitive.";
   `P "For example the locale de-CH-1996-x-mobile-tablet could match
       any of these locale range :";
   `P "de-CH-1996-x-mobile-tablet, de-CH-1997-*-x-mobile-tablet"; `Noblank;
   `P "de-CH-1996-x-mobile, de-CH-1996-*, de-*-CH-x-*, de-*-*-tablet";`Noblank;
   `P "de-CH-1996, de-*-1996, de-*-tablet"; `Noblank;
   `P "de-CH, de-*, *-CH, *-tablet"; `Noblank;
   `P "de, *";
   `P "But it wouldn't match any of these:";
   `P "fr, de-DE, *-FR, de-*-DE-*, de-*-goethe";
   `P "Given a set of matching ranges $(b,w.locales) selects the most
       \"precise\" one, that is the range with most subtags. To resolve ties,
       ranges are sorted lexicographically per subtag with * smaller than
       anything in increasing order and the greatest one is taken. For example
       for the locale de-CH if the ranges de, *-CH, de-* and de-CH are
       selected, de-CH is taken and if *-CH and de-* are selected,
       de-* is taken.";
   `P "Note that RFC 4647 defines no algorithm to lookup a language range
       with a language tag, hence the above definition.";
   `P "The directives $(b,w.with-loc) and $(b,w.opt-with-loc) override
       the current locale $(b,w.loc) to evaluate a sequence of s-expressions:";
   `P "($(b,w.with-loc) $(i,el) $(i,e1) ...)";`Noblank;
   `P "($(b,w.opt-with-loc)  $(i,el) $(i,e1) ...)";
   `P "The $(i,el) expression must evaluate to a locale atom l (a BCP 47
       language tag). The s-expressions
       $(i,e1) ... are then evaluated with $(b,w.loc) bound to l and the
       result is substituted to the directive.";
   `P "If no locale $l$ is found in the $(b,w.locales) directives
       of $(i,e1) ..., $(b,w.with-loc) reports errors. By contrast,
       $(b,opt-with-loc) will silently evaluate the offending $(b,w.locales)
       directives to the empty sequence.";
   `I ("[1]", "$(i,http://tools.ietf.org/html/rfc4647#section-2.2)");
   `S "SEQUENCE MAPPING";
   `P "The $(b,w.seq) directive generates a sequence of s-expressions
       from another sequence. The most general syntax is :";
   `P "($(b,w.seq)"; `Noblank;
   `P "\xA0($(b,items) $(i,ei) ...)"; `Noblank;
   `P "\xA0($(b,sort-key) $(i,ekey) ...)"; `Noblank;
   `P "\xA0($(b,reverse-sort) $(i,er))"; `Noblank;
   `P "\xA0($(b,limit) $(i,emax))"; `Noblank;
   `P "\xA0($(b,eval) $(i,e) ...))";
   `P "but only the $(b,items) and $(b,eval) directive bindings are needed.";
   `P "The expression $(i,ei), ... of $(b,items) are evaluated
       and their results concatenated. This forms a sequence of
       s-expression items.";
   `P "For each item, the $(i,e) ... expressions of $(b,eval) are evaluated
       with occurences of the atom $(b,w.item) substituted by the item. The
       resulting sequences are concatenated and substituted to the directive.";
   `P "For each item, the expressions $(i,ekey) ... of $(b,sort-key) are
       evaluated with occurences of the atom $(b,w.item) substituted by the
       item. The resulting sequence of s-expressions is the item's sort key.
       If $(b,sort-key) is present the items are sorted lexicographically
       according to their key before evaluating the $(b,eval) expressions.
       The $(i,er) expression of $(b,reverse-sort) is evaluated to a
       boolean (atom true or false) and if true the sort is reversed.";
   `P "The $(i,emax) expression of $(b,limit) is evaluated to a non negative
       integer used to bound the number of evaluated items.";
   `S "TIME STAMP FORMATTING";
   `P "The $(b,w.time)$ directive formats time stamps. A time
       stamp is an s-expression that has one of these forms:";
   `P "((YYYY MM dd) (HH mm ss frac) (+ HH mm))"; `Noblank;
   `P "((YYYY MM dd) (HH mm ss frac) (- HH mm))"; `Noblank;
   `P "((YYYY MM dd) (HH mm ss frac) (Z))";
   `P "Precision can be omitted by truncating components from
       the right at the first and second nesting level. The different
       components should be understood as in RFC 3339 [1].";
   `P "The syntax of the $(b,w.time) directive is :";
   `P "($(b,w.time) $(i,et) $(i,e1) ...)";
   `P "The expression $(i,et)$ is evaluated to a time stamp. The
       expressions $(i,e1) ... are evaluated with occurences of
       special atoms like $(b,w.YYYY) substituted by the corresponding
       components of the time stamp. The resulting
       sequence of expressions is substituted to the directive.";
   `P "The syntax and semantics of most special atoms is a subset of [2]
       prefixed by $(b,w.) Only locale less representation are supported,
       localized ones can be derived by using localized lists
       of bindings and $(b,w.get). The list of special atoms:";
   `P "$(b,w.Y), $(b,w.YY), $(b,w.YYYY)     => year"; `Noblank;
   `P "$(b,w.M), $(b,w.MM)       => month"; `Noblank;
   `P "$(b,w.d), $(b,w.dd)       => day of month"; `Noblank;
   `P "$(b,w.e) => day of week (1-7), 1 is Monday";
   `P "$(b,w.a)             => period (am or pm)"; `Noblank;
   `P "$(b,w.h), $(b,w.hh)       => hour 1-12"; `Noblank;
   `P "$(b,w.H), $(b,w.HH)       => hour 0-23"; `Noblank;
   `P "$(b,w.m), $(b,w.mm)       => minutes"; `Noblank;
   `P "$(b,w.s), $(b,w.ss)       => seconds"; `Noblank;
   `P "$(b,w.S), $(b,w.SS), $(b,w.SSS) => fractional seconds"; `Noblank;
   `P "$(b,w.Z)             => zone";
   `P "$(b,w.rfc3339)       => atom formatted according to RFC 3339";
   `I("[1]", "$(i,http://tools.ietf.org/html/rfc3339)");
   `I("[2]", "$(i,http://unicode.org/reports/tr35/#Date_Format_Patterns)");
   `S "UUID GENERATION";
   `P "The $(b,w.uuid) directive generates universally unique
       identifiers according to RFC 4122 [1]. The syntax is :";
   `P "($(b,w.uuid)) or ($(b,w.uuid) $(i,ens) $(i,e1) ...)";
   `P "In the first form a version 4 (random based) UUID atom is generated.
       Each evaluation generates a new UUID.";
   `P "In the second form a version 5 (name based SHA-1 hashing) UUID atom
       is generated. The $(i,ens) expression is evaluated to a valid
       RFC 4122 UUID atom used for the namespace. The expression
       $(i,e1) ... are evaluated and the resulting sequence of expressions
       is interpreted as @-text (see $(b,webglue-@-text)(7)) to define
       the textual data used as the name for the generation.";
   `I ("[1]","$(i,http://tools.ietf.org/html/rfc4122)");
   `S "SEE ALSO";
   `P "$(b,webglue)(1), $(b,webglue-maps)(5), $(b,webglue-@-text)(7)";
]

let at_text_page =
  ("WEBGLUE-@-TEXT", 7, "", webglue_version, "Webglue manual"), [
  `S "NAME";
  `P "webglue-@-text - s-expressions as UTF-8 encoded textual data";
  `S "DESCRIPTION";
  `P "Webglue @-text interprets a sequence of s-expressions
      as UTF-8 encoded textual data.";
  `P "Given a sequence of atoms the interpretation concatenates
      the atoms with a single space character (U+0040) between them.
      Any occuring s-expression list in the sequence is an error.";
  `P "a \"sequence\xA0\xA0of\"\xA0\xA0\xA0\xA0four atoms. =>
      'a sequence\xA0\xA0of four atoms.'";
  `P "The space introduced between two atoms is suppressed if there's an @
      atom between them. Initial and trailing @ atoms are ignored.
      For a single @ use @@.";
  `P "@ @ my @ email @ @@ @ example.org @ @ @ => 'myemail@example.org'";
  `S "SEE ALSO";
  `P "$(b,webglue-s-expressions)(5)"; ]

let standard_uris_page =
  ("WEBGLUE-STANDARD-URIS", 7, "", webglue_version, "Webglue manual"), [
  `S "NAME";
  `P "webglue-standard-uris - standard URI set determination";
  `S "DESCRIPTION" ] @ Wformat.Standard_uris.man

let pages = [
  "maps", maps_page; "s-expressions", s_expression_page;
  "formats", formats_page; "directives", directives_page;
  "@-text", at_text_page; "standard-uris", standard_uris_page; ]

let help _ man_format topic commands = match topic with
| None -> `Help (man_format, None)
| Some topic ->
    let format_name ((`Atom f, _), _) = f in
    let topics = List.rev_map format_name (Format_db.list ()) in
    let topics = List.rev_append (List.rev_map fst pages) topics in
    let topics = List.rev_append ("topics" :: "w.map" :: commands) topics in
    let topics = List.sort compare topics in
    let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" ->
	      `Ok (C.out_v "-" (Fmt.pp_list Fmt.pp_str) topics)
    | `Ok t when List.mem t commands ->
	      `Help (man_format, Some t)
    | `Ok t ->
	      match (try `Ok (List.assoc t pages) with Not_found -> man_of_format t)
	      with `Error _ as e -> e
	      | `Ok p -> `Ok
              (Cmdliner.Manpage.print man_format Format.std_formatter p)

(* Command line interface *)

open Cmdliner

let doc = "The topic to get help on, `topics' lists the topics."
let topic = Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)

let doc = "show help about webglue"
let man = [
  `S "DESCRIPTION";
  `P "The command $(b,help) shows help about webglue commands, map files,
      map formats, directives etc.";
  `P "Use `topics' as $(i,TOPIC) to get a list of topics." ] @ C.copts_man @ [
  `S "SEE ALSO";
  `P "$(b,webglue)(1)"; ]

let cmd =
  Term.(ret (pure help $ C.copts $ Term.man_format $ topic $ Term.choice_names))
    ,Term.info "help" ~sdocs:C.copts_sec ~doc ~man

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
