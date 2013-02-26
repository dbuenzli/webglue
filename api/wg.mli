(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Webglue API. 

    {e Version %%VERSION%% - %%EMAIL%% } *)

(** {1 Basic types} *)

(** Output abstraction. 

    Output abstractions are used by plugins to generate the URI contents. *)
module Out : sig

  (** {1:out Output} *)

  type dest = [ 
    | `Channel of out_channel | `Buffer of Buffer.t | `Fun of (int -> unit) ]
  (** The type for output destinations. For [`Buffer], the buffer won't
      be cleared. For [`Fun] the function is called with the output {e
      bytes} as [int]s. *)

  type output
  (** The type for output abstractions. *)

  val make : dest -> output
  (** [make dest] is an output abstraction writing to [dest]. *)

  val c : output -> char -> unit
  (** [c o char] outputs [char] on [o]. *)
      
  val s : output -> string -> unit
  (** [s o str] outpus [str] on [o]. *)
      
  val ssub : output -> string -> int -> int -> unit
  (** [s o str pos len] outputs [len] characters of [str] starting at [pos]. *)
end

type dict
(** The type for heterogeneous dictionaries. *)

(** Heterogeneous dictionaries. *)
module Dict : sig

  (** {1:dkey Keys} *)

  type 'a key 
  (** The type for dictionary keys whose lookup value is ['a]. *)

  val key : unit -> 'a key
  (** [key ()] is a new dictionary key. *)

  (** Standard keys. *)
  module Key : sig
   
    (** {1:dkeystd Standard keys} *)
   
    val filename : string key
    (** [filename] is a key representing a filename. *)

    val char_range : ((int * int) * (int * int)) key
    (** [char_range] is a key representing a range of characters. *)

    val locale : ([`Atom of string] * dict) key
    (** [locale] is a key holding a locale. *)

    val id : ([`Atom of string] * dict) key
    (** [id] is a key holding a map id. *)

    val format : ([`Atom of string] * dict) key
    (** [format] is a key holding a map id format. *)

    val includes : (([`Atom of string] * dict) list * dict) key
    (** [includes] is a key holding a list of map ids. *)

    val trace : dict list key 
    (** [trace] is a key holding a list of dictionaries tracing
	the source of an error. *)
  end

  (** {1:dict Dictionaries} *) 

  type t = dict
  (** The type for dictionaries. *)

  val empty : dict
  (** The empty dictionary. *)

  val is_empty : dict -> bool
  (** [is_empty d] is [true] iff [d] is empty. *)
  
  val mem : dict -> 'a key -> bool
  (** [mem d k] is [true] iff [k] has a mapping [d]. *)

  val add : dict -> 'a key -> 'a -> dict
  (** [add d k v] is [d] with [k] mapping to [v]. *)
  
  val rem : dict -> 'a key -> dict
  (** [rem d k] is [d] with [k] unbound. *)

  val find : dict -> 'a key -> 'a option
  (** [find d k] is [k]'s mapping in [d], if any. *)

  val get : dict -> 'a key -> 'a
  (** [get d k] is [k]'s mapping in [d]. 

      {b Raises.} [Invalid_argument] if [k] is not bound in [d]. *)
end

(** {1:sexp S-expressions} *)

type se = [ `Atom of string | `List of se list ] * dict
(** The type for s-expressions. Strings are UTF-8 encoded. *)

type atom = [ `Atom of string ] * dict
(** The subtype for s-expression atoms. *)

(** S-expressions. *)
module Se : sig

  (** {1:se Basics} *)

  type t = se
  (** The type for s-expressions. Strings are UTF-8 encoded. *)

  val atom : ?d:dict -> string -> [> `Atom of string] * dict
  (** [atom d a] is [(`Atom a, d)], [d] defaults to {!Dict.empty}. *)

  val list : ?d:dict -> se list -> se
  (** [list d l] is [(`List l, d)], [d] defaults to {!Dict.empty}. *)

  val with_dict : ?d:dict -> se -> se
  (** [with_dict d e] is [(fst e, d)], [d] defaults to 
      {!Dict.empty}. *)

  val to_atom : se -> [> `Atom of string] * dict
  (** [to_atom e] is [e] as an atom.
      {b Raises} [Invalid_argument] if [e] is a [`List] *)

  val to_atom_str : se -> string
  (** [to_atom_str e] is [e]'s atom string.
      {b Raises} [Invalid_argument] if [e] is a [`List] *)

  val to_list : se -> [> `List of se list ] * dict 
  (** [to_list e] is [e] as a list. 
      {b Raises} [Invalid_argument] if [e] is a [`Atom] *)

  val to_list_list : se -> se list
  (** [to_list_list e] is [e]'s list. 
      {b Raises} [Invalid_argment] if [e] is an [`Atom]. *)

  val compare : se -> se -> int
  (** [compare e e'] is a total, lexicographical, order on {!se}
      without taking dictionaries into account. *)

  val equal : se -> se -> bool
  (** [equal e e'] is [compare e e' = 0]. *)

  (** {1:print Printers} *)

  val to_string : se -> string 
  (** [to_string e] is the textual representation of [e]. *)

  val pp : Format.formatter -> se -> unit
  (** [pp ppf e] prints the textual reprentation of [e] on [ppf]. *)

  val pp_atom : Format.formatter -> atom -> unit
  (** [pp ppf a] prints the textual reprentation of [a] on [ppf]. *)

  (** {1:sparsers S-expression parsers} *)

  type 'a parse_result = [ `Ok of 'a | `Error ] 
  (** The type for parse results. *)

  type 'a parser = string * (se -> 'a parse_result)
  (** The type for s-expression parsers. The string is the type
      of element parsed. The parsing function must {{!Wlog.err} log} errors. *)

  val atom_parser : string -> (string -> dict -> 'a parse_result) -> 'a parser
  (** [atom_parser typ f e] uses [f] to parse a [typ] atom from [e]. 
      If [f] returns [`Error] or [e] is a list, [`Error] is returned. [f]
      must log errors. *)

  val list_parser : string -> (se list -> dict -> 'a parse_result) -> 'a parser
  (** [list_parser typ f e] uses [f] to parse a [typ] s-expression list from 
      [e]. If [f] returns [`Error] or [e] is an atom, [`Error] is returned. [f]
      must log errors. *)

  (** {2:sparserrun Running parsers} *)

  val parse : ?err:'a -> 'a parser -> se -> 'a 
  (** [parse p e] parses [e] with [p] and returns the result. 
      If the parsing fails raises [Exit] or, if given, returns
      [err] instead. *)

  val pnext : ?err:'a -> ?empty:'a -> ?last:bool -> 'a parser -> se list -> 
    dict -> 'a * se list
  (** [pnext p es dict] parses the head of [es] with [p]
      and returns the result and the tail of [es]. 

      If [es] is empty and [empty] is not given an error is logged on 
      [dict] and the parsing fails.
      
      if [last] is [true] (default to [false]) and the tail of [es] is 
      not empty an error is logged and the parsing fails.

      If the parsing fails raises [Exit] or, if given, returns [err] and the 
      tail of [es] instead. *)

  (** {2:sparserpredef Predefined parsers} *)

  val p_se : string -> se parser
  (** [p_se typ] parses any s-expression. *)

  val p_atom : ?validate:(string -> bool) -> string -> atom parser
  (** [p_atom typ] parses an atom and returns it as an atom. 
      The parser fails if the atom does not [validate]. *)

  val p_list : string -> ([ `List of se list ] * dict) parser
  (** [p_list typ] parses a list an returns it as a list. *)

  (** {3:sparseratom Atom parsers} *)

  val p_bool : bool parser
  (** [p_bool] parses an atom with [bool_of_string]. *)

  val p_int : int parser
  (** [p_int] parses an atom with [int_of_string]. *)
 
  val p_pos_int : int parser
  (** [p_pos_int] parses an atom with [int_of_string] and
      checks the result is non negative. *)

  val p_int_range : int -> int -> int parser 
  (** [p_int_range min max] parses an atom with [int_of_string] and
      checks the result is in the interval \[[min];[max]\]. *)

  val p_float : float parser
  (** [p_float] parses an atom with [float_of_string]. *)

  val p_float_range : float -> float -> float parser
  (** [p_float_range min max] parses an atom with [float_of_string] and
      checks that result is in the interval \[[min];[max]\]. *)

  val p_string : string parser 
  (** [p_string] parses an atom and returns its string. *)

  val p_enum : (string * 'a) list -> 'a parser
  (** [p_enum l] parses an atom and maps its string to the corresponding
      value in [l]. *)

  (** {3:sparserlist List parsers} *)

  val p_list_of : ?empty:bool -> 'a parser -> 'a list parser 
  (** [p_list_of empty p] parses the elements of a list with 
      [p]. The list may be empty iff [empty] is [true] (default). *)

  val p_pair : 'a parser -> 'b parser -> ('a * 'b) parser 
  (** [p_pair p0 p1] parses a two element list with [p0] and [p1]. *)

  val p_t2 : 'a parser -> 'b parser -> ('a * 'b) parser 
  (** [p_t2] is {!p_pair}. *)
      
  val p_t3 : 'a parser -> 'b parser -> 'c parser -> ('a * 'b * 'c) parser
  (** [p_t3] is like {!p_t2} but for three elements. *)

  val p_t4 : 'a parser -> 'b parser -> 'c parser -> 'd parser -> 
    ('a * 'b * 'c * 'd) parser
  (** [p_t4] is like {!p_t2} but for four elements. *)

  val p_fold : ?robust:bool -> ('a -> 'b -> 'a) -> (dict -> 'a) -> 'b parser ->
    'a parser
  (** [p_fold ~robust f init p] parses and folds over the elements of a list
      with [f]. Given the list's dictionary [init] defines the initial 
      accumulator. If [robust] is [true] (defaults to [false]),
      the parser doesn't fail if an element fails. *)
end

(** Sets of atoms.
    
    The dictionary of atoms is not relevant in comparisons. *)
module Aset : sig
    include Set.S with type elt = atom

    (** {1:print Printers} *)

    val pp : ?pp_sep:(Format.formatter -> unit -> unit) -> 
        (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
    (** [pp ~pp_sep pp_atom us] prints on [ppf] the atom set [ds]. 
	Atoms are printed with [pp_atom] and separated by [pp_sep] 
	(defaults to {!Format.pp_print_cut}). *)
end

(** Atom maps. 

    The dictionary of atoms is not relevant in key comparisons.*)
module Amap : Map.S with type key = atom

(** Sets of bindings. 

    A set of bindings binds keys to values. A key is an atom, the
    atom's dictionary is not relevant in key comparisons. A bound
    value is a sequence of s-expressions and an associated
    dictionary. The set of of bindings itself also has an associated
    dictionary.  *)
    module Bset : sig

  (** {1:amapkeys Keys} *) 

  type key = atom
  (** The type for binding keys. *)

  val key : ?d:Dict.t -> string -> key 
  (** [key d a] is [Se.atom d a]. *)

  (** Standard keys. *)
  module Key : sig
    val w_id : key
    (** The key [w.id]. *)

    val w_self : key 
    (** The key [w.self]. *)

    val w_conf : key 
    (** The key [w.conf]. *)

    val w_loc : key 
    (** The key [w.loc]. *)

    val w_item : key 
    (** The key [w.item]. *)

    val w_format : key
    (** The key [w.format]. *)

    val w_includes : key
    (** The key [w.includes]. *)

    val uri : key 
    (** The key [uri]. *)

    val uri_path : key 
    (** The key [uri_path]. *)
 
    val doc : key 
    (** The key [doc]. *)
  end

  (** {1:bs Sets of bindings} *)

  type t
  (** The type for sets of bindings. *)
	      
  val dict : t -> dict
  (** [dict bs] is the set's dictionnary. *)
      
  val with_dict : t -> dict -> t 
  (** [with_dict bs d] is [bs] with [dict bs = d]. *)
      
  val empty : dict -> t
  (** [empty] is an empty set of bindings. *)
      
  val is_empty : t -> bool
  (** [is_empty bs] is [true] iff [bs] is empty. *)
      
  val add : ?d:dict -> t -> key -> se list -> t 
  (** [add d bs k v] is [bs] but with [k] bound to [v], 
      the binding is annotated by [d] (defaults to [Dict.empty]). *)
	
  val rem : t -> key -> t
  (** [rem bs k] is [bs] but with [k] unbound. *)
      
  val find : t -> key -> (se list * dict) option
  (** [find bs k] is the value bound to [k], if any. *)
      
  val get : t -> key -> se list * dict
  (** [get bs k] is the value bound to [k]. 
      {b Raises.} [Invalid_argument] if [k] isnot bound in [bs]. *)

  val mem : t -> key -> bool
  (** [mem bs k] is [true] iff [k] is bound in [bs]. *)

  val map : (se list -> se list) -> t -> t
  (** [map f bs] is [bs] but with the value of its bindings transformed
      by [f]. *)

  val mapi : (key -> se list -> dict -> se list) -> t -> t
  (** [mapi f bs] is like {!map} but the keys are also given to [f]. *)
      
  val fold : ('b -> key -> se list -> dict -> 'b) -> 'b -> t -> 'b
  (** [fold f acc bs] is [(f (] ... [(f (f acc k1 v1 a1) k2 v2 a2)] ... [) 
      kn vn an)] where [(ki,vi)] are [bs]'s bindings and [ai] the binding 
      annotations. *)

  val iter : (key -> se list -> dict -> unit) -> t -> unit
  (** [iter f bs] applies [f] to all bindings of [bs] in increasing
      order of the keys. *)

  val keys : t -> key list
  (** [keys bs] is the list of keys bound in [bs]. *)
      
  val values : t -> se list list
  (** [value bs] is the list of values bound in [bs]. *)
      
  (** {1:sexp Sets of bindings as s-expressions} *)

  val p_bindings : robust:bool -> t Se.parser
  (** [of_bindings robust] parses a (possibly empty) list of bindings as a set
      of bindings. If [robust] is [true], the parser doesn't fail if a 
      binding fails.

      A binding is a list (k ...) that starts with an atom defining the 
      {e key}, followed by zero ore more s-expressions defining the {e value}. 
   *)

  val to_bindings : t -> se
  (** [to_bindings bs] is [bs] as an s-expression list of bindings. *)
end

(** S-expressions as UTC timestamps.

    [SeUTC] provides s-expression parsers and functions for UTC timestamps. *)
module SeUTC : sig

  (** {1 Basic types} *)

  type date = int * int * int 
  (** The type for dates. Year, month, day. *)

  type time = int * int * int * float 
  (** The type for times. Hour, minute, second, fractional second. *)

  type offset = [`P of int * int | `N of int * int]
  (** The type for time offsets. Hours, minutes, [`N] is for negative 
      offsets. *)

  type timestamp = date * time * offset 
  (** An UTC timestamp is a date, a time and an offset. 
      Subtracting the offset to the time gives the UTC time. *)

  (** {1 S-expression parsers} *)

  val p_date : date Se.parser 
  (** [p_date] parses a list (YYYY \[MM\] \[DD\]) into a date. Missing values
      default to [1]. *)

  val p_time : time Se.parser
  (** [p_time] parses a list (HH \[mm\] \[ss\] \[frac\]) into a time. Missing
      values default to [0]. *)

  val p_offset : offset Se.parser
  (** [p_zone] parses either (Z) or (+ HH \[mm\]) or (- HH \[mm\]). Missing
      values default to [0]. *)

  val p_timestamp : timestamp Se.parser
  (** [p_timestamp] parses a list with first a {!p_date} list
      then (optionally) a {!p_time} list and then (optionally) a
      {!p_offset} list.*)

  (** {1 Constants} *)

  val unix_epoch : timestamp
  (** [unix_epoch] is the unix epoch, i.e. 1st january 1970, 00:00:00 UTC. *)

  (** {1 Converters} *)

  val timestamp_to_rfc3339 : timestamp -> string 
  (** [timestamp_to_rfc3339 t] converts [t] to an 
      {{:http://tools.ietf.org/html/rfc3339}RFC 3339} timestamp. *)
end

(** S-expressions as textual data. 

    [Setext] outputs list of atoms as strings separated 
    by a single space character. The space between two atoms is
    suppressed if there's an ["\@"] atom between them. To output 
    a single ["\@"] use ["\@\@"]. *)
module Setext : sig 
  
  val output : ?out_string:(Out.output -> string -> unit) -> 
    ?until_list:bool -> Out.output -> se list -> se list 	
  (** [output out_string until_list o sl] outputs the [sl] as
      text. [out_string] is used to output the atoms (defaults to
      {!Out.s}). If [until_list] is [true] (defaults to [false]) the
      function stops on the first s-expression list and
      returns the remaining s-expressions. *) 

  val output_lines : ?out_string:(Out.output -> string -> unit) ->
    Out.output -> se list -> unit
  (** [output_lines out_string o sl] outputs each list in [sl]
      with {!output}. *)

  val to_atom : se -> atom 
  (** [to_atom se] is [se] if [se] is an atom or the atom 
      [resulting] from interpreting the list as @-text (the dictonary
      of the atom is the dictionary of the list). *)

  val to_string : se list -> string
  (** [to_string sl] is [sl] interpreted as text. *)
end

(** S-expressions as XML or HTML data.

    Outputs UTF-8 encoded XML data from s-expressions according 
    to {{:#seasxml}this interpetation}. *)
module SeXML : sig

  (** {1 XML output} *)

  val output_decl : Out.output -> unit
  (** [output_decl o] writes an     
      {{:http://www.w3.org/TR/REC-xml/#NT-XMLDecl}XML declaration} 
      on [o]. *)

  val output : ?html:bool -> Out.output -> se -> unit
  (** [output html o s] outputs the XML interpretation of [s] on
      [o]. If [html] is [true], html markup is generated. *)

    (** {1:seasxml s-expressions as XML data} 

	XML documents are trees, so are s-expressions. The following
	interpretation of s-expressions as XML documents is just an
	alternate {e syntax} for specifying XML data. Checking the
	validity of generated data or specifying XML's abstract syntax
	tree precisely is a non-goal.
	
	{2:el Elements} 
	
	An s-expression list starting with an atom is an XML element. The
	first atom is the name of the element and the rest of the list, its
	children.
	
	A single space character ([U+0020]) is automatically inserted
	{e between} the children of the list. This space can be
	removed by placing the special atom [@] between two
	children. If [@] is the first child, last child or on the
	right of another [@] atom it is ignored.

	Examples :
{v (p This (em paragraph) is, perhaps, too short.) 
==> <p>This <em>paragraph</em> is, perhaps, too short.</p>

(p Adding a    single space is "" possible.)
==> <p>Adding a single space is  possible.</p>

(p But space, can be (em problematic).)
==> <p>But space, can be <em>problematic</em> .</p> 

(p However not so (em problematic) @ .)
==> <p>However not so <em>problematic</em>.</p>

(p @ Wh @ at's th @ @ at ? @)
==> <p>What's that ?</p>

(p How do I write a lone @@ ?)
==> <p>How do I write a lone @ ?</p>
v}

	{2:att Attributes} 

	The attributes of an element can be specified by an @-list.  An
	@-list starts with the atom [@] and is made of lists of atoms.  In
	these lists the first atom is the name of the attribute and the
	rest of the list the attribute data. In the attribute data, a
	single space character ([U+0020]) is automatically added between
	the members of the list and like for element children it can be
	suppressed with [@] atoms.
	
	In an element s-expression, the @-list must immediately follow
	the atom specifying the element name (this cannot be confused
	with a child element since @ is not a valid XML element
	name). Any other occurence of an @-list will be treated like
	an element s-expression (and will thus generate invalid XML).

	Examples :
{v (p Click the (a (@ (href http://www.example.org)) link) element.)
==> <p>Click the <a href="http://www.example.org">link</a> element.</p>

(p (@ (id bla) (class intro ex @ ample)) 
   This has id and classes.)
==> <p id="bla" class="intro example">This has id and classes.</p>
v}

    {2:data Attribute and character data}

    Atoms that are not the first element of a list represent attribute
    and character data. In these atoms, the markup delimiters ['<'],
    ['>'], and ['"'] are automatically escaped to entities on output.
    
    Examples:
{v (p Cannot "\"insert\"" <markup/> with atoms)
==> <p>Cannot &quot;insert&quot; &lt;markup/&gt; with atoms</p>

(hr (@ (class thick "\"quoted\"" <markup/>)))
==> <hr class="thick &quot;quoted&quot; &lt;markup/&gt;"/>
v}
*)
end

(** {1 Webglue} *)

type format = atom
(** The type for map formats. *)

type id = atom
(** The type for map ids. *)

type wmap = Bset.t
(** The type for maps. *)

(** Logging module. 

    The datum of log entries should be made as precise
    as possible using dictionaries and {{!Dict.Key}standard 
    keys}. *)
module Wlog : sig

  (** {1:entries Log entries} *)

  type level = [ `Error | `Warning | `Info | `Debug ]
  (** The type for entry levels. *)  

  type entry = [ 
    | `Msg of string * dict
    | `Msg_traces of string * dict * (string * dict) list 
    | `Expected of 
	[ `Eol | `This of string ] * 
	[ `Se of se | `Eol of dict | `That of (string * dict) ]
    | `Undefined_key of se * atom
    | `Undefined_map of atom
    | `Format_exn of format * exn * string * dict ]
  (** The type for log entries. *)

  val msg : ?d:dict -> 
    ('a, Format.formatter, unit, entry) Pervasives.format4 -> 'a 
  (** [msg d fmt a0 a1 ...] is an entry [`Msg (m, d)] with [m] 
      formatted according to [fmt] and [a0], [a1], ... [d] defaults
      to {!Dict.empty}. *)

  (** {1:logging Logging} *)

  val err : entry -> unit
  (** [err entry] logs [entry] as an [`Error] entry. *)

  val warn : entry -> unit
  (** [warn entry] logs [entry] as a [`Warning] entry. *)

  val info : entry -> unit
  (** [info entry] logs [entry] as an [`Info] entry. *)

  val debug : entry -> unit
  (** [debug entry] logs [entry] as a [`Debug] entry. *)

  (** {1 Private functions} *)
  
  (** Private functions.

      Private functions are for systems using webglue, they must 
      not be used by formats. *)
  module Private : sig
    (** {1 Reporting} *)
  
    type reporter = level -> entry -> unit
    (** The type for log entry reporters. *)
 
    val set_reporter : reporter -> unit
    (** [set_reporter r] sets [r] as the function called on 
	log entries. *)

    val errors : unit -> int
    (** [errors] is the number of entries logged with level [`Error]. *)
  end
end


(** Locale identifiers and locale ranges. 

    Locale identifiers and ranges are 
    {{:http://tools.ietf.org/html/bcp47}BCP 47} language tags and ranges. *)
module Wlocale : sig

  (** {1 Locale identifier and ranges} *)

  type t = atom
  (** The type for locale identifiers. *)

  type range = atom
  (** The type for locale ranges. *)

  val is_locale : atom -> bool
  (** [is_locale a] is [true] if [a] is a BCP 47 
      {{:http://tools.ietf.org/html/rfc4647#section-2.2}extended language 
      range} without wildcard subtags. *)

  val is_range : atom -> bool 
  (** [is_range a] is [true] if [a] is a BCP 47 
      {{:http://tools.ietf.org/html/rfc4647#section-2.2}extended language 
      range}. *)

  val subtags : atom -> string list 
  (** [subtags s] is the lowercased list of subtags of the locale or 
      range [s]. *)

  val compare : atom -> atom -> int
  (** [compare r r'] orders [r] and [r'] by the number of subtags and,
      if equal, by by lexicographic order on the subtags. *)

  val equal : atom -> atom -> bool
  (** [equal r r'] is [true] iff the atoms [r] and [r'] are equal. *)

  val matches : t -> range -> bool 
  (** [matches l r] is [true] if locale [l] matches range [r]. *)

  (** {1 Parsers} *)

  val p_locale : t Se.parser 
  (** [p_locale] parses an atom as a BCP 47 language tag. *)

  val p_locale_range : range Se.parser
  (** [p_locale_range] parses an atom as a BCP 47 language range tag. *)
end

(** Dependencies and sets thereof. *)
module Wdep : sig
  (** {1:deps Dependencies} *)

  type t = [ `Val of atom * Bset.key | `Map of atom |
  `File of atom | `Other of atom ]
  (** The type for dependencies. 
      {ul 
      {- [`Val (id, k)] is a dependency on the value of the key
         [k] of the map identified by [id].} 
      {- [`Map id] is a dependency on the map identified by 
         [id].}
      {- [`File f] is a dependency on a file [f] 
         whose path is relative to the site's root path.}
      {- [`Other a] is a dependency described by the atom [a].}} *)

  val compare : t -> t -> int 
  (** [compare d d'] is a total order on dependencies. Dictionaries
      are not taken into account. *)

  (** {1:print Printers} *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf d] prints on [ppf] a textual representation of [d]. *)

  (** {1:depset Sets of dependencies} *)  

  module Set : sig
    include Set.S with type elt = t

    val vals_as_maps : t -> t
    (** [vals_as_maps ds] is the set [ds] with all [`Val] dependencies into
        as [`Map] dependencies. *)

    (** {1:print Printers} *)

    val pp : ?pp_sep:(Format.formatter -> unit -> unit) -> 
        (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
    (** [pp ~pp_sep pp_dep us] prints on [ppf] the dependency set [ds]. 
	Dependencies are printed with [pp_dep] and separated by [pp_sep] 
	(defaults to {!Format.pp_print_cut}). *)
  end
end

(** Uris and sets thereof. 

    A value of type {!Wuri.t} contain an URI an associated meta-data. *)
module Wuri : sig
  (** {1:uris URIs} *)
  
  type t
  (** The type for uris. *)

  val create : ?d:dict -> atom -> atom -> Wlocale.t option -> t option
  (** [create d uri path loc] is an URI where:
      {ul 
      {- [uri] is the URI (usually, but not necessarily 
           relative to the site's URI root).}
      {- [path] is the URI's content path relative to 
         the site's root path.} 
      {- [loc] is the locale associated to the URI.}
      {- [d] is a dictionary of meta data associated to the URI. 
         For good error reports, the generating map's dictionary 
         ({!Bset.dict}) should be included in [d]. }}

      If {!Wlocale.is_locale} [loc] is [false]
      or if [path] is not a relative path errors are logged and 
      [None] is returned. *)

  val uri : t -> atom
  (** [uri u] is [u]'s URI. *)

  val path : t -> atom
  (** [path u] is [u]'s content path. *)

  val locale : t -> Wlocale.t option 
  (** [locale u] is [u]'s locale (if any). *)

  val dict : t -> dict
  (** [dict u]  is [u]'s dictionary. *)

  val compare : t -> t -> int
  (** [compare u u'] is a total order on uris. Only the URI string
      is taken into account for comparisons. *)

  val equal : t -> t -> bool
  (** [equal u u'] is [compare u u' = 0]. *)

  (** {1:print Printers} *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf u] prints on [ppf] the URI of [u]. *)

  val pp_full : Format.formatter -> t -> unit
  (** [pp ppf a] prints on [ppf] the URI, the path and locale (if any) of [u] 
      separated by colons. *)

  (** {1:urisets Sets of uris} *)

  module Set : sig
    type elt = t
    (** The type for set elements. *)

    type t 
    (** The type for URI sets. *)
    
    val empty : t 
    (** [empty] is the empty set *)
 
    val is_empty : t -> bool
    (** [is_empty us] is [true] iff [us] is the empty set. *)
 
    val mem : atom -> t -> bool 
    (** [mem uri us] is [true] iff there is [u] is in [us] with 
	{!uri} [u = uri]. *)

    val find : atom -> t -> elt option
    (** [find uri us] is the uri [u] from [us] such that 
	{!uri} [u = uri] (if any). *)

    val get : atom -> t -> elt
    (** [get uri us] is like {!find} but raises [Invalid_argument]
	there is no such uri. *)

    val add : elt -> t -> t
    (** [add u us] contains exactly the elements of [us] and [u]. *)

    val add_opt : elt option -> t -> t 
    (** [add_opt u us] contains exactly the elements of [us] and [u] (if any). 
     *)

    val rem : elt -> t -> t 
    (** [rem u us] contains exactly the elements of [us] minus [u]. *)

    val singleton : elt -> t
    (** [singleton us] contains exactly [u]. *)

    val union : t -> t -> t
    (** [union us us'] contains exactly the elements of [us] and those
	of [us']. *)

    val inter : t -> t -> t
    (** [inter us us'] contains exactly the elements present in both
	[us] and [us']. *)

    val diff : t -> t -> t 
    (** [diff us us'] contains exactly the elements present in [us] but
	not present in [us']. *)

    val compare : t -> t -> int
    (** [compare us us'] is a total order on sets. *)

    val equal : t -> t -> bool
    (** [equal us us'] is [true] iff [us] and [us'] contain exactly
	the same elements. *)

    val iter : (elt -> unit) -> t -> unit
    (** [iter f us] iterates [f] on every element of [us]. *)

    val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a 
    (** [fold f acc us] is [(f (]...[(f (f acc u1) u2)]...[) un)] 
	where [ui] are the elements of [us]. *)

    val for_all : (elt -> bool) -> t -> bool 
    (** [for_all p us] is [true] iff for all [u] in [us], 
	[p u = true]. *)

    val exists : (elt -> bool) -> t -> bool 
    (** [exists p us] is [true] iff there is [u] in [us] with 
	[p u = true]. *)

    val keep_if : (elt -> bool) -> t -> t
    (** [keep_if p us] is the set of elements satifying [p]. *)

    val cardinal : t -> int
    (** [cardinal us] is the number of elements in [us]. *)

    val elements : t -> elt list
    (** [elements us] is the elements of [us]. *)

    (** {1:print Printers} *)

    val pp : ?pp_sep:(Format.formatter -> unit -> unit) -> 
        (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
    (** [pp ~pp_sep pp_uri us] prints the URI set [us] on [ppf]. 
	URIs are printed with [pp_uri] and separated by [pp_sep] (defaults
	to {!Format.pp_print_cut}). *)
  end
  
end

(** Contexts. 

    A context holds information to evaluate the data language of webglue
    and tracks data dependencies. Formats use the context to lookup data 
    and maps. *)
module Wctx : sig
  (** {1 Contexts} *)

  type t 
  (** The type for contexts. *)

  val with_locale : ?err:bool -> t -> Wlocale.t option -> t
  (** [with_locale c l] is [c] but with locale [l]. If [err] is
      [true] missing localization are reported. *)

  val locale : t -> Wlocale.t option
  (** [locale c] is [c]'s locale, if any. *)

  val find_map : t -> id -> wmap option
  (** [find_map c id] finds the map [id] in [c]. *)

  val get_map : t -> id -> wmap
  (** [get_map c id] is the map [id] in [c].
      
      {b Raises} [Invalid_argument] if [id] is not defined in [c]. *)

  (** {1 Dependency tracking} *)

  val add_dep : t -> Wdep.t -> unit
  (** [add_dep c d] adds [d] to the set of dependencies tracked by [c]. 

      {b Note.} This affects all contexts derived from [c] 
      with {!with_locale} and the contexts used to derive [c]. *)

  val rem_dep : t -> Wdep.t -> unit
  (** [add_dep c d] removes [d] from the set of dependencies tracked by [c]. 

      {b Note.} This affects all contexts derived from [c] 
      with {!with_locale} and the contexts used to derive [c]. *)

  val deps : t -> Wdep.Set.t
  (** [deps c] is the set of dependencies tracked by [c]. *)

  (** {1 Private functions} *)

  (** Private functions. 

      Private functions are for systems using webglue, they must 
      not be used by formats. *)
  module Private : sig
    val create : ?locale:Wlocale.t -> ?conf:atom -> (t -> id -> wmap option) ->
      id -> t
    (** [create locale conf db id] is an evaluation context such that : 
	{ul
	{- [w.id] and [w.self] binds to [id].}
	{- [w.locale] binds to [locale].}
	{- [w.conf] binds to [conf].}
	{- [db] is used for map lookup.}} *)
  end

end

(** Webglue maps. *)
module Wmap : sig
(** {1:maps Maps} *)

  type t = wmap
  (** The type for maps. *)

  val id : wmap -> id
  (** [id m] is [m]'s ID. *)

  val format : wmap -> format
  (** [format m] is [m]'s format. *)

  val find : Wctx.t -> wmap -> Bset.key -> 
    (se list * dict) option 
  (** [find c m k] is like {!Bset.find} except that 
      the value bound to [k] is evaluated in the context [c]. *)

  val get : Wctx.t -> wmap -> Bset.key -> se list * dict
  (** [get c m k] is like {!find} but {b raises} [Invalid_argument]
      if [k] is not bound in [m]. *)

  val locales : Wctx.t -> wmap -> Bset.key -> Aset.t
  (** [locales c m k] is the set of reachable locales and locale
      range of the value bound to [k] in [m] in the context [c]. It
      corresponds to the locales specified in the outermost [w.locales]
      directives. Returns the empty set if [k] is not bound in [m]. 
      The locale of [c] is irrelevant. *)

  (** {1:uris URI set and content} *)

  val uri_set : Wctx.t -> wmap -> Wuri.Set.t
  (** [uri_set c m] is the set of uris defined by [m] in the context
      [c]. The locale of [c] is irrelevant. *)

  val uri_content : Wctx.t -> wmap -> Wuri.t -> Out.dest -> unit
  (** [uri_content c m u d] outputs on [d] the content of the uri 
      [u] of [m] in the context [c]. The locale of [c] is irrelevant. *)

  (** {1:deps Dependencies} *)

  val creation_deps : Wctx.t -> wmap -> Wdep.Set.t 
  (** [creation_deps] is the set of dependencies needed to define
      the (unevaluated) keys of [m]. The locale of [c] is irrelevant. *)

  val uri_set_deps : Wctx.t -> wmap -> Wdep.Set.t 
  (** [uris_deps c m] is the set of dependencies needed to define
      the URI set of [m]. The locale of [c] is irrelevant. *)

  val uri_deps : Wctx.t -> wmap -> Wuri.t -> Wdep.Set.t 
  (** [uri_deps c m u] is the set of dependencies needed to define
      the content of the uri [u]. The locale of [c] is irrelevant. *)

  (** {1 Diagnostics} *)

  val diagnose : Wctx.t -> wmap -> Out.dest -> unit
  (** [diagnose c m d] writes on [d] format specific diagnostics for
      [m] in the context [c]. The locale of [c] is irrelevant. *)

  (** {1 Private functions} *)

  (** Private functions. 

      Private functions are for systems using webglue, they must 
      not be used by formats. *)
  module Private : sig 
    (** {1 Map creation} *)

    val prepare : Wctx.t -> id -> se list * dict -> wmap 
    (** [prepare c id (sl, d)] is the map identified by [id]
	with bindings [sl] and dictionary [d] in the context [c]. 
	The locale of [c] is irrelevant. The map is ready to be
	given to {!create}. *)

    val create : Wctx.t -> wmap -> wmap
    (** [create c m] creates the map according to its format.
	[m] should be the result of {!prepare} and accessible
	from [c] map lookup function. After that, the resulting
	map should replace [m] in the lookup function. *)
  end
end

(** Map formats and database. 

    A map format is a module implementing the signature {!T}. It
    must be defined by calling {!define}. *)
module Wformat : sig

  (** {1:def Format definition} *)

  type key_info = [ `Required | `Optional of se list option | `Derived ]
  (** The type for key information. [`Required] means that the key
      must be defined in the map. [`Optional] means that the key may be
      defined in the map and if a value is given it is used to defined
      the key when it is absent. [`Derived] means that the key is computed by 
      the format at creation time. *)

  type man_block = [
    | `S of string | `P of string | `I of string * string | `Noblank ]
  (** The type for man page blocks. *)

  (** The type for a map format.

      To implement a new format, define a module [F]. In [F], include
      {!Wformat.Default}, override at least the {{!fdoc}format
      documentation} and the functions needed to provide the format
      functionalty.  After the definition of [F] call
      {!Wformat.define} with [F].
   *) 
module type T = sig

  (** {1:fdoc Format documentation.} *)

    val name : format
    (** [name] is the name of the map format. A map using
	this format must use [name] for the key {!Bset.Key.w_format}. *)

    val version : string
    (** [version] is a version string for the map format. *)

    val doc : string
    (** [doc] is a one line description the map format. *)

    val man : man_block list 
    (** [man] is the manual of the format.  *)

    val keys : (Bset.key * key_info * string) list 
    (** [keys] describe the keys of the map format. *)

  (** {1:fcreate Map creation} *)

    val create : Wctx.t -> wmap -> wmap option
    (** [create c m] is called by {!Wmap.Private.create} to create a map of this
	format with [m] in the context [c]. 

	{b Guarantees.} The map [m] is defined on the [`Required] keys
	of {!keys} aswell as on the [`Optional] ones with a default value.
	The locale of [c] is [None].

	{b Implementation duties.}  The function should compute the
	value of [`Derived] keys, add them to [m] and return the
	result. It can also add its own {{!Dict.dkey}dictionary keys} to
	the map or its bindings to store private, format specific,
	information.

	The function should refrain from calling {!Wmap.find} and
	{!Wmap.get}, map creation should be quick and should not
	depend on the evaluation of the data language.

	If the function tries to redefine keys reserved by 
	webglue (e.g. {!Bset.Key.w_id} or 
	{!Bset.Key.w_format}), {!Wmap.Private.create} will revert these 
	changes. 

        {b Dependency tracking.} 
	The context [c] automatically tracks the dependencies that are
	accessed during the function call to derive the result of
	{!Wmap.creation_deps}. The format can interfere with this
	process by using {!Wctx.add_dep} and {!Wctx.rem_dep} on [c].

	{b On errors.} The function should {{!Wlog}log} them
	on the appropriate dictionary and return [None]. *)

    (** {1:furis URI set and content} *)

    val uri_set : Wctx.t -> wmap -> Wuri.Set.t 
    (** [uri_set c m] is called by {!Wmap.uri_set} to determine the URI set 
        defined by [m] in the context [c]. 

	{b Guarantees.} The map [m] is the result of {!create}. The locale
	of [c] is [None]. 

	{b Implementation duties.}
	The function should return the set of URIs defined by [m] in 
	the context [c]. 

	{b Dependency tracking.}
	The context [c] automatically tracks the dependencies that are
	accessed during the function call to derive the result of
	{!Wmap.uri_set_deps}. The format can interfere with this
	process by using {!Wctx.add_dep} and {!Wctx.rem_dep} on [c].

	{b On errors.} The function should {{!Wlog}log}
	them on the appropriate dictionary and return the set of 
	URIs that can be generated without producing errors. 

	{b Important.} If the map defines an URI set for a single localized 
	resource, {{!Wformat.Standard_uris}standard URI set} 
	determination should be used. *)

    val uri_content : Wctx.t -> wmap -> Wuri.t -> Out.dest -> unit
    (** [uri_content c m u d] is called by {!Wmap.uri_content} to write on 
	[d] the content of the URI [u] defined by [m] in the context [c]. 

	{b Guarantees.} The map [m] is the result of {!create}. The URI [u] was
	returned by {!uri_set} on [m] and the same [c] except for its
	locale. The locale of [c] is the locale of [u] (if any).

	{b Implementation duties.} 
	The function should write on [d] the content of [u] defined by [m]
	in the context [c]. 

	{b Dependency tracking.}
	The context [c] automatically tracks the dependencies that are
	accessed during the function call to derive the result of
	{!Wmap.uri_deps}. The format can interfere with this
	process by using {!Wctx.add_dep} and {!Wctx.rem_dep} on [c].

	{b On errors.} The function should {{!Wlog}log}
	them on the appropriate dictionary. *)

   (** {1:fdiagnose Diagnostics} *)

    val diagnose : Wctx.t -> wmap -> Out.dest -> unit
    (** [diagnose c m d] is called by {!Wmap.diagnose} to write on [d] format
	specific diagnostics for [m] in the context [c]. 

	{b Guarantees.} The map [m] was created by {!create}. The 
	locale of [c] is [None]. 

	{b Implementation duties.} 
	The function should report diagnostic information about [m]
	in the context [c]. If errors are diagnosed they should
	be {{!Wlog}logged} on the appropriate dictionary. 

	{b On errors.} The function should {{!Wlog}log}
	them on the appropriate dictionary. *)
  end

  val define : (module T) -> unit
  (** [define F] registers the module [F] as the definition of the 
      format [F.format]. *)

  module Default : T
  (** The default map format. *)

  (** Standard URI set determination. 
      
      Formats defining an URI set for a single localized document
      should use the standard URI set determination procedure.

      Just include this module into your format. 
      {[include Standard_uris
        let doc = doc :: ... TODO]} *)
  module Standard_uris : sig
    val man : man_block list
    (** [man_standard_uris] is a list of paragraphs describing the 
	standard URI set determination rules. *)

    val keys : (Bset.key * key_info * string) list
    (** [standard_uris_keys_doc] are the keys used by standard URI set
	determination, they must be added to the format's 
	{{!T.keys}key specifications}. *)
  
    val uri_set : Wctx.t -> wmap -> Wuri.Set.t
    (** [standard_uris c m] is the standard URI set determined by [m] in 
	the context [c]. *)
  end
      
  (** {1:man Format manual} *)

  val man_description : man_block
  (** [man_description] is a section header [`S] for a high-level
      description of the format. *)

  val man_uri_set : man_block 
  (** [man_uri_set] is a section header [`S] for describing URI sets. *)

  val man_uri_content : man_block
  (** [man_uri_content] is a section header [`S] for describing URI 
      content. *)

  val man_see_also : man_block 
  (** [man_see_also] is a section header [`S] for references to 
      other parts of the manual. *)

  val man : (module T) -> man_block list
  (** [man F] is the manual of [F]. *)
 
  (** {1 Private functions} *)

  (** Private functions. 

      Private functions are for systems using webglue, they must 
      not be used by formats.

      The webglue API is independent from the mechanism used to 
      load formats. It must be specified by the system using webglue 
      with {!set_db}. *)
  module Private : sig

    (** {1 Format databases} *)

    (** The type for format databases. *)
    module type Db = sig
    (** {1 Format database} *)
  
      val is_defined : format -> bool 
      (** [is_defined fm] is [true] iff the format [fm] is defined
	  in the database. 

	  {b Implementation duties.} The function must return [true]
	  iff there is a format [F] in the database such that [F.name
	  = fm] and [F] was previously stored with
	  {!define}. *)

      val define : (module T) -> unit
      (** [define F] stores [F] in the database as the definition
	  of the format [F.name]. 

	  {b Implementation duties.} The function must store
	  [F] in the database under the name [F.name] so that 
	  it can be retrieved later with {!find} [F.name]. 

	  If the format [F.name] is already defined in the database
	  it should be silently replaced by the new definition. *)
 
      val find : format -> (module T) option
      (** [find fm] is the format module for [fm] (if any).

	  {b Implementation duties.} The function must return
	  the last module [F] with [F.name = fm] that was defined
	  with {!define}. If there is no such module it must invoke
	  a mechanism that may trigger this definition and return the
	  corresponing module if that succeded. Otherwise it should
	  silently return [None]. *)
    end
  
    val set_db : (module Db) -> unit
    (** [set_db db] sets the format database to [db]

	{b Note.} This sets the implementations of {!define} and {!find}
	to the corresponding functions of {!Db} and defines the default
	format {!Default} in the database. *)  

    val find : format -> (module T) option
    (** [find fm] is the definition of [fm] (if any). *)
  end
end

(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli
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
