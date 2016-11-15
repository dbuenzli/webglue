(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf
let pr = Format.fprintf

(* Invalid_argument strings *)

let err_format_db = "no format database set with Wformat.set_db"
let err_dict_unbound = str "key unbound in dict"
let err_map_unbound (`Atom k, _) = str "key `%s' unbound in map" k
let err_uri_unbound (`Atom u, _) = str "uri `%s' not in uri set" u
let err_map_id (`Atom id, _) = str "map `%s' is undefined" id
let err_not_atom = "s-expression is not an atom"
let err_not_list = "s-expression is not a list"

(* Basic types *)

(* Output abstraction *)

module Out = struct
  type output = { c : char -> unit; s : Bytes.t -> int -> int -> unit; }
  type dest = [
    | `Channel of out_channel | `Buffer of Buffer.t | `Fun of (int -> unit) ]

  let c o = o.c
  let s o s = o.s (Bytes.unsafe_of_string s) 0 (String.length s)
  let ssub o s pos len = o.s (Bytes.unsafe_of_string s) pos len
  let make = function
  | `Channel c -> { c = output_char c; s = output c }
  | `Buffer b ->
      let add_sub b s = Buffer.add_substring b (Bytes.unsafe_to_string s) in
      { c = Buffer.add_char b; s = add_sub b }
  | `Fun f ->
      let c c = f (Char.code c) in
      let s s p l =
        for i = p to p + l - 1 do f (Char.code (Bytes.get s p)) done
      in
      { c; s }
end

(* Dictionaries, see http://mlton.org/PropertyList. *)

module Dict = struct
  module Id = struct
    type t = int
    let create () = Oo.id (object end)                    (* thread-safe UID. *)
    let compare : int -> int -> int = compare
  end

  module M = (Map.Make (Id) : Map.S with type key = Id.t)
  type t = exn M.t
  type 'a key = Id.t * ('a -> exn) * (exn -> 'a option)

  let key () (type v) =
    let module M = struct exception E of v end in
    Id.create (), (fun x -> M.E x), (function M.E x -> Some x | _ -> None)

  let compare = M.compare compare
  let equal = M.equal ( = )
  let empty = M.empty
  let is_empty = M.is_empty
  let mem d (id, _, _ ) = M.mem id d
  let add d (id, inj, _) v = M.add id (inj v) d
  let rem d (id, _, _) = M.remove id d
  let find d (id, _, proj) = try proj (M.find id d) with Not_found -> None
  let get d (id, _, proj) =
    try match proj (M.find id d) with Some v -> v | None -> raise Not_found
    with Not_found -> invalid_arg err_dict_unbound

  module Key = struct
    let filename = key ()
    let char_range = key ()
    let (locale : ([`Atom of string] * t) key)= key ()
    let (id : ([`Atom of string] * t) key) = key ()
    let (format : ([`Atom of string] * t) key) = key ()
    let (includes : (([`Atom of string] * t) list * t) key) = key ()
    let (trace : t list key) = key ()
  end
end

type dict = Dict.t
type atom = [ `Atom of string ] * dict
type se = [ `Atom of string | `List of se list ] * dict
type id = atom
type format = atom

(* Logging *)

module Wlog = struct
  type level = [ `Error | `Warning | `Info | `Debug ]
  type entry = [
    | `Msg of string * dict
    | `Msg_traces of string * dict * (string * dict) list
    | `Expected of
        [ `Eol | `This of string ] *
        [ `Se of se | `Eol of dict | `That of string * dict ]
    | `Undefined_key of se * atom
    | `Undefined_map of atom
    | `Format_exn of format * exn * string * dict ]

  let (r : (level -> entry -> unit) ref) = ref (fun _ _ -> ())
  let errors = ref 0

  let msg ?(d = Dict.empty) fmt =
    let msg _ = `Msg (Format.flush_str_formatter (), d) in
    Format.kfprintf msg Format.str_formatter fmt

  let err entry = incr errors; !r `Error entry
  let warn entry = !r `Warning entry
  let info entry = !r `Info entry
  let debug entry = !r `Debug entry

  module Private = struct
    type reporter = level -> entry -> unit
    let set_reporter f = r := f
    let errors () = !errors
  end
end

(* Logged errors and warnings *)

let msg_format_redefine (`Atom fm, d) = Wlog.msg ~d "format %s redefined" fm
let msg_format_undefined (`Atom fm, d) = Wlog.msg ~d "format %s undefined" fm
let msg_format_key (`Atom fm, d) (`Atom k, _) =
  Wlog.msg ~d "format %s internal error: key `%s' is reserved by webglue" fm k

let err_uri_path (`Atom p, d) =
  Wlog.msg ~d "URI content path `%s' is not relative" p

let err_uri_loc (`Atom l, d) =
  Wlog.msg ~d "URI locale `%s' is not a locale identifier" l

let err_circular d = Wlog.msg ~d "circular data dependency"
let err_cond_match e d = Wlog.msg ~d "no match for data `%s'" e
let err_locale_match (`Atom l, _) d = Wlog.msg ~d "no match for locale `%s'" l
let err_seq_miss a d = Wlog.msg ~d "missing `%s' directive binding in w.seq" a
let err_range_twice loc d d1 d2 =
  `Msg_traces (str "two bindings for the same locale range `%s'" loc, d,
               ["first binding", d1; "second binding", d2])

let err_eval_uuid_name d =
  Wlog.msg ~d "evaluation to non empty @@-text for UUID name expected"

(* `Expect errors. *)

let exp_found_eol t dict = `Expected (`This t, `That ("nothing left", dict))
let exp t f = `Expected (t, f)
let exp_this t e = `Expected (`This t, `Se e)
let exp_eol e = `Expected (`Eol, `Se e)
let type_format = "format"
let type_bool = "boolean"
let type_int = "integer"
let type_pos_int = "positive integer"
let type_int_range l r = str "integer in [%d;%d] range" l r
let type_float = "float"
let type_float_range l r = str "float in [%g;%g] range" l r
let type_string = "string"
let type_locale = "locale (BCP 47 language tag)"
let type_locale_range = "locale range (BCP 47 language range)"
let type_locale_branch = "locale range branch"
let type_uuid = "RFC 4122 UUID"
let type_id = "ID"
let type_se = "s-expression"
let type_atom = "atom"
let type_list = "list"
let type_list_of k = str "%s list" k
let type_ne_list_of k = str "non empty %s list" k
let type_binding = "binding"
let type_map_spec = "ID or binding list"
let type_key = "key"
let type_cond_branch = "condition branch"
let type_cond = "s-expression for data condition"
let type_seq_spec = "directive binding for w.seq"
let type_enum l = str "%s atom" (String.concat " or " (List.map fst l))
let type_t2 k0 k1 = str "%s, %s 2-element list" k0 k1
let type_t3 k0 k1 k2 = str "%s, %s, %s 3-element list" k0 k1 k2
let type_t4 k0 k1 k2 k3 = str "%s, %s, %s, %s 4-element list" k0 k1 k2 k3
let type_date = "(YYYY [MM] [dd]) date"
let type_unit_frac = "float in [0.;1.[ range"
let type_time = "(HH [mm] [ss] [frac]) time"
let type_toffset = "(Z) or (+ hh [mm]) or (- hh [mm]) time zone offset"
let type_timestamp = "timestamp"

(* S-expressions *)

module Se = struct
  type t = se
  let atom ?(d = Dict.empty) a = (`Atom a), d
  let list ?(d = Dict.empty) l = (`List l), d
  let with_dict ?(d = Dict.empty) e = (fst e), d
  let to_atom = function `Atom _, _ as e -> e | _ -> invalid_arg err_not_atom
  let to_atom_str = function `Atom a, _ -> a | _ -> invalid_arg err_not_atom
  let to_list = function `List _, _ as e -> e | _ -> invalid_arg err_not_list
  let to_list_list = function `List l, _ -> l | _ -> invalid_arg err_not_list
  let compare e e' =                   (* lexicographic order without dicts. *)
    let rec aux s s' = match s, s' with
    | ((`Atom a, _) :: es) :: up, ((`Atom a', _) :: es') :: up' ->
        let c = compare a a' in
        if c <> 0 then c else aux (es :: up) (es' :: up')
    | ((`List l, _) :: es) :: up, ((`List l', _) :: es') :: up' ->
        aux (l :: es :: up) (l' :: es' :: up')
    | [] :: up, [] :: up' -> aux up up'
    | [], [] -> 0
    | ((`Atom _, _) :: _) :: _, ((`List _, _) :: _) :: _ -> -1
    | ((`List _, _) :: _) :: _, ((`Atom _, _) :: _) :: _ -> 1
    | l :: _, [] :: _ -> 1
    | [] :: _, l :: _ -> -1
    | _ :: _, [] | [], _ :: _ -> assert false
    in
    aux ([e] :: []) ([e'] :: [])

  let equal e e' = compare e e' = 0

  (* Printers *)

  let pp = Seio.print
  let pp_atom ppf (`Atom a, _) = Format.pp_print_string ppf a
  let to_string = Seio.to_string

  (* S-expression parsers *)

  type 'a parse_result = [ `Ok of 'a | `Error ]
  type 'a parser = string * (se -> [ `Error | `Ok of 'a])

  let atom_parser typ to_t = typ, fun e -> match e with
    | `List _, dict -> Wlog.err (exp_this typ e); `Error
    | `Atom a, dict -> to_t a dict

  let list_parser typ to_t = typ, fun e -> match e with
    | `Atom _, dict -> Wlog.err (exp_this typ e); `Error
    | `List es, dict -> to_t es dict

  (* Running parsers *)

  let p_err = function None -> raise Exit | Some v -> v
  let parse ?err p e = match (snd p) e with `Error -> p_err err | `Ok v -> v
  let pnext ?err ?empty ?(last = false) p es dict = match es with
  | e :: es ->
      if last && es <> [] then (Wlog.err (exp_eol (List.hd es)); p_err err, es)
      else (parse ?err p e), es
  | [] ->
      match empty with
      | Some v -> v, []
      | None -> Wlog.err (exp_found_eol (fst p) dict); p_err err, []

  (* Predefined parsers *)

  let p_se typ = typ, fun e -> `Ok e
  let p_atom ?(validate = fun _ -> true) typ = typ, fun e -> match e with
    | `List _, _ -> Wlog.err (exp_this typ e); `Error
    | `Atom a, _  as e ->
        if validate a then `Ok e else (Wlog.err (exp_this typ e); `Error)

  let p_list typ = typ, fun e -> match e with
    | `Atom _, _ -> Wlog.err (exp_this typ e); `Error
    | `List _, _ as e -> `Ok e

  (* String parsers for atom parsers *)

  let s_parser typ to_t s dict = try `Ok (to_t s) with
  | Failure _ -> Wlog.err (exp_this typ (`Atom s, dict)); `Error

  let fail () = failwith ""
  let to_bool s = try bool_of_string s with Invalid_argument _ -> fail ()
  let to_int = int_of_string
  let to_pos_int s = let i = to_int s in if i < 0 then fail () else i
  let to_float = float_of_string
  let to_uuid s = match Uuidm.of_string s with Some u -> u | None -> fail ()
  let to_int_range l r s =
    let i = to_int s in if l <= i && i <= r then i else fail ()

  let to_float_range l r s =
    let f = to_float s in if l <= f && f <= r then f else fail ()

  (* Atom parsers *)

  let p_t typ to_t = atom_parser typ (s_parser typ to_t)
  let p_bool = p_t type_bool to_bool
  let p_int = p_t type_int to_int
  let p_pos_int = p_t type_pos_int to_pos_int
  let p_int_range l r = p_t (type_int_range l r) (to_int_range l r)
  let p_float = p_t type_float to_float
  let p_float_range l r = p_t (type_float_range l r) (to_float_range l r)
  let p_string = atom_parser type_string (fun s _ -> `Ok s)
  let p_uuid = p_t type_uuid to_uuid
  let p_enum l = atom_parser (type_enum l) (fun s dict ->
      try `Ok (List.assoc s l) with
      | Not_found -> Wlog.err (exp_this (type_enum l) (`Atom s, dict)); `Error)

  (* List parsers *)

  let p_list_of ?(empty = true) p =
    let k = (if empty then type_list_of else type_ne_list_of) (fst p) in
    k, fun e -> match e with
    | `Atom _, _ -> Wlog.err (exp_this k e); `Error
    | `List l, _ ->
        if not empty && l = [] then (Wlog.err (exp_this k e); `Error)
        else
        let add acc e = match (snd p) e with
        | `Error -> raise Exit | `Ok v -> v :: acc
        in
        try `Ok (List.rev (List.fold_left add [] l)) with Exit -> `Error

  let p_pair p0 p1 =
    let k = type_t2 (fst p0) (fst p1) in
    list_parser k (fun es d -> match es with
      | [e0; e1] ->
          (match (snd p0) e0 with `Error -> `Error | `Ok v0 ->
            (match (snd p1) e1 with `Error -> `Error | `Ok v1 -> `Ok (v0, v1)))
      | es -> Wlog.err (exp_this k (list ~d es)); `Error)

  let p_t2 = p_pair
  let p_t3 p0 p1 p2 =
    let k = type_t3 (fst p0) (fst p1) (fst p2) in
    list_parser k (fun es d -> match es with
      | [e0; e1; e2] ->
          (match (snd p0) e0 with `Error -> `Error | `Ok v0 ->
            (match (snd p1) e1 with `Error -> `Error | `Ok v1 ->
              (match (snd p2) e2 with `Error -> `Error | `Ok v2 ->
                `Ok (v0, v1, v2))))
      | es -> Wlog.err (exp_this k (list ~d es)); `Error)

  let p_t4 p0 p1 p2 p3 =
    let k = type_t4 (fst p0) (fst p1) (fst p2) (fst p3) in
    list_parser k (fun es d -> match es with
      | [e0; e1; e2; e3] ->
          (match (snd p0) e0 with `Error -> `Error | `Ok v0 ->
            (match (snd p1) e1 with `Error -> `Error | `Ok v1 ->
              (match (snd p2) e2 with `Error -> `Error | `Ok v2 ->
                (match (snd p3) e3 with `Error -> `Error | `Ok v3 ->
                  `Ok (v0, v1, v2, v3)))))
      | es -> Wlog.err (exp_this k (list ~d es)); `Error)

  let p_fold ?(robust = false) f acc p =
    list_parser (type_list_of (fst p)) (fun es d ->
        let fold acc e = match (snd p) e with
        | `Error -> if robust then acc else raise Exit
      | `Ok v -> f acc v
        in
        try `Ok (List.fold_left fold (acc d) es) with Exit -> `Error)
end

(* Sets of atoms, atom maps and sets of bindings *)

module Atom = struct
  type t = atom
  let compare (`Atom a, _) (`Atom a', _) = compare a a'
end

module Aset = struct
  include Set.Make (Atom)

  let pp ?(pp_sep = Format.pp_print_cut) pp_dep ppf us =
    let p_el ppf u = pp_dep ppf u; pp_sep ppf () in
    iter (p_el ppf) us
end

module Amap = Map.Make (Atom)
module Bset = struct
  type key = Amap.key
  let key = Se.atom

  module Key = struct
    type t = atom
    let compare (`Atom k, _) (`Atom k', _) = compare k k'
    let w_id = key "w.id"
    let w_self = key "w.self"
    let w_conf = key "w.conf"
    let w_loc = key "w.loc"
    let w_item = key "w.item"
    let w_format = key "w.format"
    let w_includes = key "w.includes"
    let uri = key "uri"
    let uri_path = key "uri-path"
    let doc = key "doc"
    let is_reserved (`Atom k, _) =
      String.length k > 1 && k.[0] = 'w' && k.[1] = '.'
  end

  type t = (se list * dict) Amap.t * Dict.t

  let empty dict = Amap.empty, dict
  let dict (_, dict) = dict
  let with_dict (m, _) dict = m, dict
  let is_empty (m, _) = Amap.is_empty m
  let add ?(d = Dict.empty) (m, md) k v = (Amap.add k (v, d) m, md)
  let rem (m, d) k = (Amap.remove k m, d)
  let find (m, _) k = try Some (Amap.find k m) with Not_found -> None
  let get (m, _) k = try Amap.find k m with
  | Not_found -> invalid_arg (err_map_unbound k)

  let mem (m, _) k = Amap.mem k m
  let map f (m, d) = let f (v, a) = (f v, a) in (Amap.map f m), d
  let mapi f (m, d) = let f k (v, a) = (f k v a, a) in (Amap.mapi f m), d
  let fold f acc (m, _) = let f k (v, a) acc = f acc k v a in Amap.fold f m acc
  let iter f (m, _) = let f k (v, a) = f k v a in Amap.iter f m
  let keys (m, _) = List.rev (Amap.fold (fun k _ acc -> k :: acc) m [])
  let values (m, _) = List.rev (Amap.fold (fun _ (v, _) acc -> v :: acc) m [])
  let p_bindings ~robust =
    Se.list_parser (type_list_of type_binding) (fun es d ->
        let bind acc = function
        | `List ((`Atom _, _ as k) :: v), d -> add ~d acc k v
  | e ->
      Wlog.err (exp_this type_binding e);
      if robust then acc else raise Exit
        in
        try `Ok (List.fold_left bind (empty d) es) with Exit -> `Error)

  let to_bindings m =
    let add acc k v d = ((`List ((k :> se) :: v)), d) :: acc in
    `List (List.rev (fold add [] m)), dict m
end

(* S-expressions as UTC timestamps *)

module SeUTC = struct
  type date = int * int * int
  type time = int * int * int * float
  type offset = [`P of int * int | `N of int * int]
  type timestamp = date * time * offset

  let is_leap y = (y mod 4 = 0) && (y mod 100 <> 0 || y mod 400 = 0)
  let month_len = [|31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]
  let month_len y m = if (m = 2 && is_leap y) then 29 else month_len.(m - 1)
  let weekday ~y ~m ~d =
    let _weekday y m d =      (* according to calendar FAQ 2.6, sunday is 0. *)
      let a = (14 - m) / 12 in
      let y' = y - a in
      let m' = m + 12 * a - 2 in
      (d + y' + (y' / 4) - (y' / 100) + (y' / 400) + (31 * m') / 12) mod 7
    in
    7 - ((7 - _weekday y m d) mod 7)                       (* make sunday 7. *)

  let unix_epoch = (1970, 01, 01), (00, 00, 00, 0.), `P (0, 0)

  let timestamp_to_rfc3339 ((y, m, d), (h, min, s, f), z) =
    let z = match z with
    | `P (0, 0) -> "Z" | `P (h, m) -> str "+%02d:%02d" h m
    | `N (h, m) -> str "-%02d:%02d" h m
    in
    let frac =
      if f = 0. then "" else
      let s = str "%.3f" f in (String.sub s 1 (String.length s - 1))
    in
    (str "%04d-%02d-%02dT%02d:%02d:%02d%s%s" y m d h min s frac z)

  (* Parsers *)

  let p_year = Se.p_int_range 0 9999
  let p_month = Se.p_int_range 1 12
  let p_day y m = Se.p_int_range 1 (month_len y m)
  let p_date = Se.list_parser type_date (fun es dict ->
      try
      let y, es = Se.pnext p_year es dict in
      let m, es = Se.pnext ~empty:1 p_month es dict in
      let d, _  = Se.pnext ~empty:1 ~last:true (p_day y m) es dict in
      `Ok (y, m, d)
    with Exit -> `Error)

  let p_hour = Se.p_int_range 0 23
  let p_min = Se.p_int_range 0 59
  let p_sec = Se.p_int_range 0 60 (* leap secs ! *)
  let p_frac = Se.atom_parser type_unit_frac (fun a dict ->
      try
        let f = float_of_string a in
        if 0. <= f && f < 1. then `Ok f else failwith ""
      with Failure _ -> `Error)

  let p_time = Se.list_parser type_time (fun es d -> try
      let h, es = Se.pnext p_hour es d in
      let m, es = Se.pnext ~empty:0 p_min es d in
      let s, es = Se.pnext ~empty:0 p_sec es d in
      let f, _  = Se.pnext ~empty:0. ~last:true p_frac es d in
      `Ok (h, m, s, f)
    with Exit -> `Error)

  let p_offset = Se.list_parser type_toffset (fun es d -> try match es with
    | [`Atom "Z", _] -> `Ok (`P (0, 0))
    | (`Atom ("+" | "-" as sign), _) :: es ->
        let h, es = Se.pnext p_hour es d in
        let m, _  = Se.pnext ~empty:0 ~last:true p_min es d in
        `Ok (if sign = "+" then `P (h, m) else `N (h, m))
    | _ -> Wlog.err (exp_this type_toffset (Se.list ~d es)); `Error
    with Exit -> `Error)

  let p_timestamp = Se.list_parser type_timestamp (fun es d -> try
      let date, es = Se.pnext p_date es d in
      let time, es = Se.pnext ~empty:(0,0,0,0.) p_time es d in
      let zone, _ = Se.pnext ~empty:(`P(0,0)) ~last:true p_offset es d in
      `Ok (date, time, zone)
    with Exit -> `Error)
end

(* S-expressions as textual data *)

module Setext = struct
  let output ?(out_string = Out.s) ?(until_list = false) o es =
    let rec aux o os ul space = function
    | (`Atom "@", _) :: rest -> aux o os ul false rest
    | (`Atom d, _) :: rest ->
        if space then os o " ";
        if d = "@@" then os o "@" else os o d;
        aux o os ul true rest
    | [] -> []
    | ((`List _, dict as e) :: rest as es) ->
        if ul then es else
        begin
          Wlog.err (exp_this type_atom e);
          aux o os ul space rest
        end
    in
    aux o out_string until_list false es

  let rec output_lines ?out_string o = function
  | (`List l, _) :: r ->
      ignore (output ?out_string o l);
      if r <> [] then Out.c o '\n';
      output_lines ?out_string o r
  | [] -> ()
  | (`Atom _, _ as e) :: r ->
      Wlog.err (exp_this type_list e); output_lines ?out_string o r

  let to_string es =
    let b = Buffer.create 256 in
    ignore (output (Out.make (`Buffer b)) es);
    Buffer.contents b

  let to_atom = function  (* TODO *)
  | `Atom _, _ as a -> a
  | `List al, d ->
      let b = Buffer.create 256 in
      ignore (output (Out.make (`Buffer b)) al);
      `Atom (Buffer.contents b), d
end

(* S-expressions as XML data. *)

module SeXML = struct
  (* TODO review html generation + do a pretty output + escape *)

  let output_decl o =
    Out.s o "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

  let out_data o s =
    let len = String.length s in
    let start = ref 0 in
    let last = ref 0 in
    let escape e =
      Out.ssub o s !start (!last - !start);
      Out.s o e;
      incr last;
      start := !last;
    in
    while (!last < len) do match s.[!last] with
    | '<' -> escape "&lt;"
    | '>' -> escape "&gt;"
    | '\"' -> escape "&quot;"
    | '&' -> escape "&amp;"
    | _ -> incr last;
    done;
    Out.ssub o s !start (!last - !start)

  let output_el_data o stag space d =
    if stag then (Out.c o '>'); if space then Out.c o ' '; out_data o d

  let rec output_atts o = function
  | (`List ((`Atom name, _) :: data), _) :: rest ->
      Out.c o ' '; Out.s o name; Out.s o "=\"";
      ignore (Setext.output ~out_string:out_data o data);
      Out.c o '\"';
      output_atts o rest
  | [] -> ()
  | e :: rest ->
      Wlog.err (exp (`This "attribute") (`Se e)); output_atts o rest

  let output_comment o stag space contents =
    if stag then Out.c o '>'; if space then Out.c o ' ';
    Out.s o "<!--"; ignore (Setext.output o contents); Out.s o "-->"

  let output_stag o stag space tag atts =
    if stag then Out.c o '>'; if space then Out.c o ' ';
    Out.c o '<'; Out.s o tag;
    output_atts o atts

  let output_html_etag o stag tag =
    let is_void = function
    (* from http://www.w3.org/TR/html5/syntax.html#void-elements *)
    | "area" | "base" | "br" | "col" | "command"| "embed" | "hr" | "img"
    | "input"| "keygen" | "link" | "meta" | "param" | "source" | "track"
    | "wbr" -> true | _ -> false
    in
    if stag && is_void tag then Out.c o '>' else
    (if stag then Out.s o ">"; Out.s o "</"; Out.s o tag; Out.c o '>')

  let output_etag html o stag tag =
    if html then output_html_etag o stag tag else
    if stag then Out.s o "/>" else
    (Out.s o "</"; Out.s o tag; Out.c o '>')

  let output ?(html = false) o se =
    let rec aux o stag space tags = function
    | ((`List ((`Atom "&", _) :: (`Atom entity, _) :: []), _) :: es) :: up ->
        if stag then Out.c o '>'; if space then Out.c o ' ';
        Out.c o '&'; Out.s o entity; Out.c o ';';
        aux o false true tags (es :: up)
    | ((`List ((`Atom "!", _) :: comment), _) :: es) :: up ->
        output_comment o stag space comment;
        aux o false false tags (es :: up)
    | ((`List ((`Atom tag, _) :: contents), _) :: es) :: up ->
        let atts, childs = match contents with
        | (`List ((`Atom "@", _) :: atts), _) :: childs -> atts, childs
        | childs -> [], childs
        in
        output_stag o stag space tag atts;
        aux o true false (tag :: tags) (childs :: es :: up)
    | ((`Atom "@", _) :: es) :: up ->
        aux o stag false tags (es :: up)
    | ((`Atom "@@", _) :: es) :: up ->
        if stag then Out.c o '>'; if space then Out.c o ' '; Out.c o '@';
        aux o false true tags (es :: up)
    | ((`Atom d, _) :: es) :: up ->
        output_el_data o stag space d;
        aux o false true tags (es :: up);
    | ((`List _, _ as e) :: es) :: up ->
        Wlog.err (exp (`This "TODO") (`Se e));
        aux o space stag tags (es :: up)
    | [] :: [] -> ()
    | [] :: up ->
        output_etag html o stag (List.hd tags);
        aux o false true (List.tl tags) up
    | [] -> assert false
    in
    aux o false false [] ([se] :: [])
end

(* Webglue *)

type wmap = Bset.t

(* Locale identifiers and ranges *)

module Wlocale = struct
  type t = atom
  type range = atom

  let is_alpha i = 0x41 <= i && i <= 0x5A || 0x61 <= i && i <= 0x7A
  let is_digit i = 0x30 <= i && i <= 0x39
  let is_alnum i = is_alpha i || is_digit i
  let is_bcp47_tag_list ~range s =
    let c = ref 0 in
    let first = ref true in
    try                   (*  parse (1*8alpha | '*') *('-' (1*8alnum | '*')) *)
      for i = 0 to String.length s - 1 do
        if s.[i] = '-' then
          (if !c <> 0 && !c <= 8 then (first := false; c := 0) else raise Exit)
        else if s.[i] = '*' && !c = 0 then
          (if range then c := 8 else raise Exit)
        else if !first then
          (if is_alpha (Char.code s.[i]) then incr c else raise Exit)
        else
        (if is_alnum (Char.code s.[i]) then incr c else raise Exit)
      done;
      !c <> 0 && !c <= 8
    with Exit -> false

  let _is_locale l = is_bcp47_tag_list ~range:false l
  let _is_range l = is_bcp47_tag_list ~range:true l
  let is_locale (`Atom l, _) = _is_locale l
  let is_range (`Atom l, _) = _is_range l
  let subtags (`Atom l, _) =
    let i = ref (String.length l - 1) in               (* start from the end. *)
    let ts = ref [] in
    while (!i >= 0) do
      let i' = try String.rindex_from l !i '-' with Not_found -> -1 in
      ts := (String.lowercase (String.sub l (i' + 1) (!i - i'))) :: !ts;
      i := i' - 1
    done;
    !ts

  let _compare ts ts' =
    let rec aux c ts ts' = match ts, ts' with
    | t :: ts, t' :: ts' ->
        if c <> 0 then aux c ts ts' else aux (compare t t') ts ts'
    | [], [] -> c
    | [], t' -> -1
    | t, [] -> 1
    in
    aux 0 ts ts'

  let rec _matches ls rs = match ls, rs with
  | l :: ls, r :: rs when l = r ->  _matches ls rs
  | (l :: ls as ls'), ("*" :: rs as rs') ->
      _matches ls rs || _matches ls rs' || _matches ls' rs
  | [], "*" :: rs -> _matches [] rs
  | ls, [] -> true
  | _, _ -> false

  let compare l r = _compare (subtags l) (subtags r)
  let equal (`Atom r, _) (`Atom r', _) = r = r'
  let matches l r = _matches (subtags l) (subtags r)

  (* Parsers *)

  let p_locale = Se.p_atom ~validate:_is_locale type_locale
  let p_locale_range = Se.p_atom ~validate:_is_range type_locale_range

  let set es =
    let add acc = function
      | `List ((`Atom _, _ as loc) :: _), _ -> Aset.add loc acc
      | _ -> acc (* note, we don't report syntax errors here. *)
    in
    let rec aux acc = function
    | ((`List ((`Atom "w.locales", _) :: locs), _) :: es) :: up ->
        aux (List.fold_left add acc locs) (es :: up)
    | ((`Atom _, _) :: es) :: up -> aux acc (es :: up)
    | ((`List l, _) :: es) :: up -> aux acc (l :: es :: up)
    | [] :: [] -> acc
    | [] :: up -> aux acc up
    | _ -> assert false
    in
    (aux Aset.empty (es :: []))
end


(* Dependency sets *)

module Wdep = struct
  module Dep = struct
    type t = [
      | `Val of atom * Bset.key | `File of atom | `Map of atom
      | `Other of atom ]

    let compare d d' = match d, d' with
    | `Val ((i, _), (k, _)), `Val ((i', _), (k', _)) ->
        let c = Pervasives.compare i i' in
        if c <> 0 then c else Pervasives.compare k k'
    | `Map (a, _), `Map (a', _)
    | `File (a, _), `File (a', _)
    | `Other (a, _), `Other (a', _) -> Pervasives.compare a a'
    | d, d' -> Pervasives.compare d d'
  end

  type t = Dep.t
  let compare = compare
  let pp ppf = function
  | `File f -> pr ppf "file:%a" Se.pp_atom f
  | `Map id -> pr ppf "map:%a" Se.pp_atom id
  | `Val (id, key) -> pr ppf "val:%a:%a" Se.pp_atom id Se.pp_atom key
  | `Other o -> pr ppf "other:%a" Se.pp_atom o

  module Set = struct
    include Set.Make (Dep)

    let vals_as_maps ds =
      let add e acc = match e with `Val (id, _) -> add (`Map id) acc
                                 | d -> add d acc
      in
      fold add ds empty

    let pp ?(pp_sep = Format.pp_print_cut) pp_dep ppf us =
      let p_el ppf u = pp_dep ppf u; pp_sep ppf () in
      iter (p_el ppf) us
  end
end

(* Uris and sets thereof *)

module Wuri = struct
  type t = { uri : atom; path : atom; locale : Wlocale.t option;
             dict : Dict.t }

  let check (`Atom ps, _ as path) loc =
    let check_path = Filename.is_relative ps in
    let check_loc, loc = match loc with None -> true, Se.atom ""
                                      | Some l -> Wlocale.is_locale l, l
    in
    if not check_path then Wlog.err (err_uri_path path);
    if not check_loc then Wlog.err (err_uri_loc loc);
    check_path && check_loc

  let create ?(d = Dict.empty) uri path locale =
    if not (check path locale) then None else
    Some { uri; path; locale; dict = d }

  let uri u = u.uri
  let path u = u.path
  let locale u = u.locale
  let dict u = u.dict
  let compare u u' = compare u.uri u'.uri
  let equal u u' = u.uri = u'.uri

  (* Printers *)

  let pp ppf u = Se.pp_atom ppf u.uri
  let pp_full ppf u =
    let l = match u.locale with None -> "" | Some (`Atom l, _) -> l in
    pr ppf "%a:%a:%s" Se.pp_atom u.uri Se.pp_atom u.path l

  module Set = struct
    (* N.B. Implemented with a map because we want to be able
            to index them by the uri string.  *)
    type elt = t
    type t = elt Amap.t

    let empty = Amap.empty
    let is_empty = Amap.is_empty
    let mem uri us = Amap.mem uri us
    let find uri us = try Some (Amap.find uri us) with
    | Not_found -> None

    let get uri us = match find uri us with
    | Some u -> u
    | None -> invalid_arg (err_uri_unbound uri)

    let add u us = Amap.add u.uri u us
    let add_opt u us = match u with
    | None -> us
    | Some u -> Amap.add u.uri u us

    let rem u us = Amap.remove u.uri us
    let singleton u = Amap.singleton u.uri u
    let union us us' =
      let union _ l r = match l, r with
      | (Some _ as u), _ | _, (Some _ as u) -> u
      | None, None  -> None
      in
      Amap.merge union us us'

    let inter us us' =
      let inter _ l r = match l,r with Some _ as u, Some _ -> u | _  -> None in
      Amap.merge inter us us'

    let diff us us' =
      let diff _ l r = match l,r with Some _ as u, None -> u | _ -> None in
      Amap.merge diff us us'

    let compare us us' = Amap.compare (fun u u' -> compare u u') us us'
    let equal us us' = Amap.equal equal us us'
    let iter f us = Amap.iter (fun _ u -> f u) us
    let fold f acc us = Amap.fold (fun _ u acc -> f acc u) us acc
    let for_all p us = Amap.for_all (fun _ u -> p u) us
    let exists p us = Amap.exists (fun _ u -> p u) us
    let keep_if p us = Amap.filter (fun _ u -> p u) us
    let cardinal = Amap.cardinal
    let elements us = Amap.fold (fun _ u acc -> u :: acc) us []

    (* Printers *)

    let pp ?(pp_sep = Format.pp_print_cut) pp_uri ppf us =
      let p_el ppf u = pp_uri ppf u; pp_sep ppf () in
      iter (p_el ppf) us
  end
end

(* Contexts and data language evaluation *)

module Wctx = struct

  (* Constant keys for the w.time directive *)

  let k_rfc3339 = Bset.key "w.rfc3339"
  let k_Y = Bset.key "w.Y"
  let k_YY = Bset.key "w.YY"
  let k_YYYY = Bset.key "w.YYYY"
  let k_M = Bset.key "w.M"
  let k_MM = Bset.key "w.MM"
  let k_d = Bset.key "w.d"
  let k_dd = Bset.key "w.dd"
  let k_e = Bset.key "w.e"
  let k_a = Bset.key "w.a"
  let k_h = Bset.key "w.h"
  let k_hh = Bset.key "w.hh"
  let k_H = Bset.key "w.H"
  let k_HH = Bset.key "w.HH"
  let k_m = Bset.key "w.m"
  let k_mm = Bset.key "w.mm"
  let k_s = Bset.key "w.s"
  let k_ss = Bset.key "w.ss"
  let k_S = Bset.key "w.S"
  let k_SS = Bset.key "w.SS"
  let k_SSS = Bset.key "w.SSS"
  let k_Z = Bset.key "w.Z"

  (* Contexts *)

  type t =
    { db : (t -> id -> Bset.t option);        (* function to look up maps. *)
      subst : Bset.t;                               (* atom substitutions. *)
      record_deps : bool;               (* activate direct deps recording. *)
      deps : Wdep.Set.t ref;             (* all deps seen, ugly but works. *)
      seen : Wdep.Set.t;    (* dependencies in callstack for cycle detect. *)
      trace : dict list;                                     (* callstack. *)
      err_locale : bool; }      (* true to report error on missing locale. *)

  module Private = struct
    let create ?locale ?conf db (id : atom) =
      let subst = Bset.empty Dict.empty in
      let subst = Bset.add subst Bset.Key.w_id [(id :> se)] in
      let subst = Bset.add subst Bset.Key.w_self [(id :> se)] in
      let subst = match conf with
      | None -> subst
      | Some c -> Bset.add subst Bset.Key.w_conf [(c :> se)]
      in
      let subst = match locale with
      | None -> subst
      | Some l -> Bset.add subst Bset.Key.w_loc [(l :> se)]
      in
      { db; subst; record_deps = false; deps = ref Wdep.Set.empty;
        seen = Wdep.Set.empty; trace = []; err_locale = true }
  end

  let record_deps c = { c with record_deps = true; deps = ref Wdep.Set.empty }

  let find_map c id = c.db c id
  let get_map c id = match c.db c id with
  | None -> invalid_arg (err_map_id id)
  | Some m -> m

  let locale c = match Bset.find c.subst Bset.Key.w_loc with
  | None -> None
  | Some ([`Atom _, _ as loc], _) -> Some loc
  | Some _ -> assert false

  let with_substs c l =
    let s = List.fold_left (fun su (k, v) -> Bset.add su k v) c.subst l in
    { c with subst = s }

  let with_locale ?(err = true) c (l : atom option) = match l with
  | None -> { c with subst = Bset.rem c.subst Bset.Key.w_loc; err_locale = err }
  | Some loc ->
      { c with subst = Bset.add c.subst Bset.Key.w_loc [(loc :> se)];
               err_locale = err }

  let push_trace c (`Val (id, _) as dep) t =
    let subst = Bset.add c.subst Bset.Key.w_self [(id :> se)] in
    { c with subst; seen = Wdep.Set.add dep c.seen;
             trace = List.rev_append (List.rev t) c.trace; }

  let dict_with_trace d t = Dict.add d Dict.Key.trace t

  (* Dependency tracking *)

  let deps c = !(c.deps)
  let add_dep c d = if c.record_deps then c.deps := Wdep.Set.add d (deps c)
  let rem_dep c d = if c.record_deps then c.deps := Wdep.Set.remove d (deps c)

  (* Directive evaluation.
     N.B. about adding trace to dictionaries remember that before
     an expression went through eval, no trace is in its dictionary. *)

  let p_map_spec c = type_map_spec, function      (* ID or list of bindings. *)
    | `Atom _, _ as id ->
        begin match find_map c id with
        | None -> Wlog.err (`Undefined_map id); `Error
        | Some m -> `Ok (Some id, m)
        end
    | e ->
        match (snd (Bset.p_bindings ~robust:false)) e with
        | `Error -> `Error | `Ok m -> `Ok (None, m)

  let rec eval c es dict =
    let push v acc = (v :: List.hd acc) :: List.tl acc in
    let push_list l acc = List.rev_append l (List.hd acc) :: List.tl acc in
    let rec aux c todo acc dicts lastd = match todo with
    | (e :: es) :: todo ->
        begin match e with
        | `Atom _, dict as at ->
            let dict = dict_with_trace dict c.trace in
            let with_dict (e, _) = (e, dict) in
            begin match Bset.find c.subst at with
            | None -> aux c (es :: todo) (push (with_dict at) acc) dicts dict
            | Some (l, d) ->
                let l = List.rev_map with_dict l in
                aux c (es :: todo) (push_list l acc) dicts dict
            end
        | `List l, dict  ->
            let dict = dict_with_trace dict c.trace in
            begin match l with
            | (`Atom dir, _) :: args as l ->
                let eval_dir = match dir with
                | "w.get" -> Some (eval_get c ~opt:false args dict)
                | "w.opt-get" -> Some (eval_get c ~opt:true args dict)
                | "w.cond" -> Some (eval_cond c args dict)
                | "w.locales" -> Some (eval_locales c l dict)
                | "w.with-loc" -> Some (eval_with_loc c ~opt:false args dict)
                | "w.opt-with-loc" -> Some (eval_with_loc c ~opt:true args dict)
                | "w.seq" ->  Some (eval_seq c args dict)
                | "w.time" -> Some (eval_time c args dict)
                | "w.uuid" -> Some (eval_uuid c args dict)
                | k -> None
                in
                begin match eval_dir with
                | Some (seq, lastd) ->
                    aux c (es :: todo) (push_list seq acc) dicts lastd
                | None ->
                    aux c (l :: es :: todo) ([] :: acc) (dict :: dicts) lastd
                end
            | l -> aux c (l :: es :: todo) ([] :: acc) (dict :: dicts) lastd
            end
        end
    | [] :: [] -> List.rev (List.hd acc), lastd
    | [] :: todo ->
        let l = Se.list ~d:(List.hd dicts) (List.rev (List.hd acc)) in
        aux c todo (push l (List.tl acc)) (List.tl dicts) lastd
    | _ -> assert false
    in
    aux c (es :: []) ([] :: []) [] (dict_with_trace dict c.trace)

  (* evaluates the next expression in [es] and parses it with [p] *)
  and eval_next : 'a. ?last:bool -> 'a Se.parser -> t -> se list -> dict ->
    'a * se list =
    fun ?(last = false) p c es dict -> match es with
    | e :: es ->
        if last && es <> [] then
          let unexpected = List.hd es in
          let d = dict_with_trace (snd unexpected) (dict :: c.trace) in
          (Wlog.err (exp_eol (Se.with_dict ~d unexpected)); raise Exit)
        else
        let v, dict = eval c [e] dict in
        fst (Se.pnext ~last:true p v dict), es
    | [] -> Se.pnext ~last:true p [] dict (* logs error and raises Exit *)

  (* evaluates the w.get and w.opt-get directives *)
  and eval_get c ~opt es d = try
    let es = match es with [_] as l -> (Se.atom ~d "w.self") :: l | es -> es in
    let (id, m), es = eval_next (p_map_spec c) c es d in
    let key, _ = eval_next ~last:true (Se.p_atom type_key) c es d in
    match Bset.find m key with
    | None ->
        if opt then begin
          match id with
          | None -> raise Exit
          | Some id ->
              let dep = `Val (id, key) in
              add_dep c dep; raise Exit (* no error *)
        end else
        let map = match id with
        | None -> Bset.to_bindings m
        | Some (`Atom id, d) ->
            let t = d :: Dict.get d Dict.Key.trace in (* trace to id *)
            Se.atom ~d:(dict_with_trace (Bset.dict m) t) id
        in
        Wlog.err (`Undefined_key (map, key)); raise Exit
    | Some (es, bd as b) ->
        match id with
        | None -> b                      (* list of bindings, already eval'd *)
        | Some id ->                                           (* map lookup *)
            let dep = `Val (id, key) in
            if Wdep.Set.mem dep c.seen then
              (Wlog.err (err_circular d); raise Exit)
            else
            (add_dep c dep; eval (push_trace c dep [d]) es bd)
  with Exit -> [], d

  (* evaluates the w.cond directive *)
  and eval_cond c es dict = try
    let find_branch cond acc e =
      let e = Se.with_dict ~d:(dict_with_trace (snd e) c.trace) e in
      let `List es, dict = Se.parse (Se.p_list type_cond_branch) e in
      let cond', branch = eval_next (Se.p_se type_cond) c es dict in
      match acc with
      | Some _ -> (* branch already found, continue for syntax check. *) acc
      | None ->
          match cond' with
          | `Atom "w.default", _ -> Some (branch, dict)
          | cond' -> if Se.equal cond cond' then Some (branch, dict) else None
    in
    let cond, branches = eval_next (Se.p_se type_cond) c es dict in
    match List.fold_left (find_branch cond) None branches with
    | Some (branch, dict) -> eval c branch dict
    | None ->
        let t = (snd cond) :: Dict.get (snd cond) Dict.Key.trace in
        let dict = dict_with_trace dict t in (* directive + trace to cond *)
        Wlog.err (err_cond_match (Se.to_string cond) dict); raise Exit
  with Exit -> [], dict

  (* evaluates the w.locales directive *)
  and eval_locales c dir d = try
    let branch (acc, rset) e =
      let e = Se.with_dict ~d:(dict_with_trace (snd e) c.trace) e in
      let `List es, bd = Se.parse (Se.p_list type_locale_branch) e in
      let r, es as branch = eval_next Wlocale.p_locale_range c es bd in
      match Bset.find rset r with              (* checks no same range twice *)
      | None -> (branch, bd) :: acc, Bset.add ~d:(snd r) rset r []
      | Some (_, d1) ->
          let `Atom range, d2 = r in
          Wlog.err (err_range_twice range d d1 d2);
          raise Exit
    in
    let branches, _ =
      List.fold_left branch ([], Bset.empty Dict.empty) (List.tl dir)
    in
    match locale c with
    | None ->
        let eval_branch acc ((range, branch), d) =
          Se.list ~d (range :: fst (eval c branch d)) :: acc
        in
        let branches = List.fold_left eval_branch [] branches in
        [Se.list ~d (List.hd dir :: branches)], d
    | Some l ->
        let loc = Wlocale.subtags l in
        let find_branch acc ((range, branch), d) =
          let range = Wlocale.subtags range in
          if not (Wlocale._matches loc range) then acc else
          match acc with
          | None -> Some (range, branch, d)
          | Some (range', _, _) ->
              if Wlocale._compare range range' < 0 then acc else
              Some (range, branch, d)
        in
        match List.fold_left find_branch None branches with
        | Some (_, branch, d) -> eval c branch d
        | None ->
            if not (c.err_locale) then [], d else
            (Wlog.err (err_locale_match l d); raise Exit)
  with Exit -> [], d

  (* evaluates the w.with-loc and w.opt-with-loc directives. *)
  and eval_with_loc c ~opt es d = try
    let loc, es = eval_next Wlocale.p_locale c es d in
    let c' = with_locale ~err:(not opt) c (Some loc) in
    (* add directive to the trace to make user aware of loc. change. *)
    let c' = { c' with trace = d :: c'.trace; } in
    eval c' es d
  with Exit -> [], d

  (* evaluates the w.seq directive *)
  and eval_seq c es d = try
    let eval_with_item i l d =
      fst (eval (with_substs c [Bset.Key.w_item, [i]]) l d)
    in
    let items = ref None in
    let ieval = ref None in
    let sort_key = ref None in
    let reverse_sort = ref false in
    let limit = ref None in
    let parse e =
      let e = Se.with_dict ~d:(dict_with_trace (snd e) c.trace) e in
      let `List es, d as l = Se.parse (Se.p_list type_seq_spec) e in
      match es with
      | (`Atom "items", _) :: is -> items := Some (is, d)
      | (`Atom "eval", _) :: es -> ieval := Some (es, d)
      | (`Atom "sort-key", _) :: k -> sort_key := Some (k, d)
      | (`Atom "reverse-sort", _) :: rs ->
          reverse_sort := fst (eval_next ~last:true Se.p_bool c rs d)
      | (`Atom "limit", _) :: ls ->
          limit := Some (fst (eval_next ~last:true Se.p_pos_int c ls d))
      | _ -> Wlog.err (exp_this type_seq_spec l)
    in
    List.iter parse es;
    let sort_key = match !sort_key with
    | None -> None
    | Some (k, d) -> Some (fun i -> Se.list ~d (eval_with_item i k d))
    in
    let ieval = match !ieval with
    | None -> Wlog.err (err_seq_miss "eval" d); raise Exit
    | Some (es, d) -> fun i -> (eval_with_item i es d)
    in
    let items = match !items with
    | None -> Wlog.err (err_seq_miss "items" d); raise Exit
    | Some (is, d) ->
        let items, _ = eval c is d in
        match sort_key with
        | None -> items
        | Some sort_key ->
            let kis = List.rev_map (fun i -> sort_key i, i) items in
            let sort =
              if !reverse_sort then fun (k, _) (k', _) -> Se.compare k k'
              else fun (k, _) (k', _) -> Se.compare k' k
            in
            List.rev_map snd (List.stable_sort sort kis)
    in
    let mapped_items = match !limit with
    | None -> List.rev (List.rev_map ieval items)
    | Some max ->
        let rec keep count acc items =
          if count = 0 then acc else
          match items with
          | i :: is -> keep (count - 1) ((ieval i) :: acc) is
          | [] -> acc
        in
        List.rev (keep max [] items)
    in
    List.flatten mapped_items, d
  with Exit -> [], d

  and eval_time c es d = try
    let stamp, es = eval_next SeUTC.p_timestamp c es d in
    let (yyyy, mm, dd), (h, m, s, f), z = stamp in
    let h' = if h = 0 || h = 12 then 12 else h mod 12 in
    let p, zh, zm = match z with
    | `P (h, m) -> '+', h, m | `N (h, m) -> '-', h, m
    in
    let substs = [                                                (* hum... *)
      k_rfc3339, [Se.atom (SeUTC.timestamp_to_rfc3339 stamp)];
      k_Y, [Se.atom (str "%d" yyyy)];
      k_YY, [Se.atom (str "%02d" (yyyy mod 100))];
      k_YYYY, [Se.atom (str "%04d" yyyy)];
      k_M, [Se.atom (str "%d" mm)];
      k_MM, [Se.atom (str "%02d" mm)];
      k_d, [Se.atom (str "%d" dd)];
      k_dd, [Se.atom (str "%02d" dd)];
      k_e, [Se.atom (str "%d" (SeUTC.weekday yyyy mm dd))];
      k_a, [Se.atom (if h < 12 then "am" else "pm")];
      k_h, [Se.atom (str "%d" h')];
      k_hh, [Se.atom (str "%02d" h')];
      k_H, [Se.atom (str "%d" h)];
      k_HH, [Se.atom (str "%02d" h)];
      k_m, [Se.atom (str "%d" m)];
      k_mm, [Se.atom (str "%02d" m)];
      k_s, [Se.atom (str "%d" s)];
      k_ss, [Se.atom (str "%02d" s)];
      k_S, [Se.atom (String.sub (str "%.1f" f) 2 1)];
      k_SS, [Se.atom (String.sub (str "%.2f" f) 2 2)];
      k_SSS, [Se.atom (String.sub (str "%.3f" f) 2 3)];
      k_Z, [Se.atom (str "%c%02d%02d" p zh zm)]];
    in
    eval (with_substs c substs) es d
  with Exit -> [], d

 and eval_uuid c es d = try match es with
 | [] -> [Se.atom (Uuidm.to_string (Uuidm.create `V4))], d
 | es ->
     let ns, es = eval_next Se.p_uuid c es d in
     let name, d = eval c es d in  (* TODO better error *)
     let name = match Setext.to_string name with
     | "" -> Wlog.err (err_eval_uuid_name d) ; raise Exit
     | n -> n
     in
     [Se.atom (Uuidm.to_string (Uuidm.create (`V5 (ns, name))))], d
 with Exit -> [], d
end

(* The following belongs to the Wmap module but is defined here
   to avoid a recursive definition between Wformat and Wmap. *)

let wmap_find c m k = match Bset.find m k with
| None -> None
| Some (es, d) ->
    let id = Dict.get (Bset.dict m) Dict.Key.id in
    let t = match Dict.find d Dict.Key.trace with None -> [d] | Some t -> t in
    let dep = `Val (id, k) in
    let c = Wctx.push_trace c dep t in (* TODO cleanup *)
    Wctx.add_dep c dep;
    Some (fst (Wctx.eval c es d), d)

let wmap_get c m k = match wmap_find c m k with
| None -> invalid_arg (err_map_unbound k)
| Some (es, d) -> es, d

let wmap_locales c m k = match wmap_find (Wctx.with_locale c None) m k with
| None -> Aset.empty
| Some (es, d) -> Wlocale.set es

(* Format definition and database *)

module Wformat = struct
  type key_info = [ `Required | `Optional of se list option | `Derived ]
  type man_block =
    [ `S of string | `P of string | `Pre of string | `I of string * string
    | `Noblank ]

  module type T = sig
    val name : format
    val version : string
    val doc : string
    val man : man_block list
    val keys : (Bset.key * key_info * string) list
    val create : Wctx.t -> wmap -> wmap option
    val uri_set : Wctx.t -> wmap -> Wuri.Set.t
    val uri_content : Wctx.t -> wmap -> Wuri.t -> Out.dest -> unit
    val diagnose : Wctx.t -> wmap -> Out.dest -> unit
  end

  (* Format manual *)

  let man_description = `S "DESCRIPTION"
  let man_keys = `S "KEYS"
  let man_uri_set = `S "URI SET"
  let man_uri_content = `S "URI CONTENT"
  let man_see_also = `S "SEE ALSO"
  let man_key ((`Atom k, _), i, doc) =
    let label = match i with
    | `Derived -> str "$(i,%s) (derived)" k
    | `Required -> str "$(b,%s) (required)" k
    | `Optional (None | Some []) -> str "$(b,%s)" k
    | `Optional Some l ->
        str "$(b,%s) (defaults to %s)" k
          (String.concat " " (List.map Se.to_string l))
    in
    `I (label, doc)

  let rec merge_keys_section acc insert keys = function
  | `S _ as s :: bs ->
      let acc = if insert then List.rev_append keys acc else acc in
      merge_keys_section (s :: acc) (s = man_description) keys bs
  | b :: bs -> merge_keys_section (b :: acc) insert keys bs
  | [] -> if insert then List.rev_append keys acc else acc

  let man fM =
    let module F = (val fM : T) in
    let rev_cmp ((`Atom k, _), _, _) ((`Atom k', _), _, _) = compare k' k in
    let keys = List.sort rev_cmp F.keys in
    let keys =
      if keys = [] then [] else
      man_keys :: (List.rev_map man_key keys)
    in
    let name_p =
      let `Atom name, _ = F.name in
      let d = if F.doc = "" then "" else (str " - %s" F.doc) in
      `P (str "%s%s" name d)
    in
    let man = [`S "NAME"; name_p;] @ F.man in
    List.rev (merge_keys_section [] false keys man)


  module Default = struct
    let name = Se.atom "w.map"
    let version = "%%VERSION%%"
    let doc = "the default map format"
    let man = [
      man_description;
      `P "The format $(b,w.map) is the default map format. It can be used to
    share definitions across maps.";
      man_uri_set;
      `P "The URI set is empty."]

    let keys = []
    let create _ m = Some m
    let uri_set _ _ = Wuri.Set.empty
    let uri_content _ _ _ _ = assert false
    let diagnose _ _ _ = ()
  end

  (* Format databases *)

  module Private = struct
    module type Db = sig
      val is_defined : format -> bool
      val define : (module T) -> unit
      val find : format -> (module T) option
    end

    module Invalid_db = struct
      let define _ = invalid_arg err_format_db
      let is_defined _ = invalid_arg err_format_db
      let find _ = invalid_arg err_format_db
    end

    let db = ref (module Invalid_db : Db)
    let find fm = let module Db = (val !db : Db) in Db.find fm
    let define fM =
      let module Db = (val !db : Db) in
      let module F = (val fM : T) in
      if Db.is_defined F.name then Wlog.warn (msg_format_redefine F.name);
      Db.define fM

    let set_db m = db := m; define (module Default : T)
  end

  let get fm =
    let module Db = (val !Private.db : Private.Db) in
    match Db.find fm with Some f -> f | None -> assert false

  let define = Private.define

  let protect ~fail fM m f x = try f x with
  | exn ->
      let bt = Printexc.get_backtrace () in
      let module F = (val fM : T) in
      Wlog.err (`Format_exn (F.name, exn, bt, (Bset.dict m)));
      fail

  (* Standard uri set *)

  module Standard_uris = struct
    let man = [
      `P "The URI set is defined by webglue's standard URI determination.";
      `P "The standard URI determination takes the @-text for the $(b,uri) key
          and determines its locales. Each locale defines an URI and the locale
          of its content.";
      `P "If the $(b,uri-path) key is present, its @-text defines the path for
    the URI content. In conjunction with suitable webserver URI rewrites,
          this allows to entirely decouple the URI from its content path
    and define a \"clean\" URI in $(b,uri) for map cross-references."; ]

    let keys = [
      Bset.Key.uri, `Required,
      "@-text defining the URI(s).";
      Bset.Key.uri_path, `Optional None,
      "@-text defining the URI path." ]

    let uri_set c m =
      let uri locale =
        let c = Wctx.with_locale c locale in
        let es, d = wmap_get c m Bset.Key.uri in
        let uri = Setext.to_atom (`List es, d) in
        let path = match wmap_find c m Bset.Key.uri_path with
        | Some (es, d) -> Setext.to_atom (`List es, d)
        | None -> uri
        in
        Wuri.create ~d:(Bset.dict m) uri path locale
      in
      let locs = wmap_locales c m Bset.Key.uri in
      if Aset.is_empty locs then Wuri.Set.add_opt (uri None) Wuri.Set.empty else
      let add loc a = Wuri.Set.add_opt (uri (Some loc)) a in
      Aset.fold add locs Wuri.Set.empty
  end

  (* The default map format *)

  let default = (module Default : T)
end

module Wmap = struct
  type t = wmap

  (* Map data *)

  let format m = Dict.get (Bset.dict m) Dict.Key.format
  let id m = Dict.get (Bset.dict m) Dict.Key.id
  let includes m = Dict.get (Bset.dict m) Dict.Key.includes
  let find = wmap_find
  let get = wmap_get
  let locales = wmap_locales

  (* Map creation *)

  module Private = struct
    let key_creation_deps = Dict.key () (* to store the creation dependencies *)

    let add_format_keys ks m = try
      let add m (k, i, _) =
        if Bset.Key.is_reserved k then
          (Wlog.err (msg_format_key (format m) k); raise Exit)
        else match i with
        | `Derived | `Optional None -> m
        | `Required ->
            begin match Bset.find m k with
            | Some _ -> m
            | None -> Wlog.err (`Undefined_key ((id m :> se), k)); raise Exit
            end
        | `Optional (Some v) ->                     (* optional with default. *)
            match Bset.find m k with Some _ -> m
                                   | None -> Bset.add ~d:(Bset.dict m) m k v
      in
      `Ok (List.fold_left add m ks)
    with Exit -> `Error

    let add_reserved_keys fm id (lincs, dincs as incs) m =
      let m = Bset.add m Bset.Key.w_format [(fm :> se)] in
      let m = Bset.add m Bset.Key.w_id [(id :> se)] in
      let m = Bset.add ~d:dincs m Bset.Key.w_includes (lincs :> se list) in
      let d = Dict.add (Bset.dict m) Dict.Key.format fm in
      let d = Dict.add d Dict.Key.id id in
      let d = Dict.add d Dict.Key.includes incs in
      Bset.with_dict m d

    let parse_map es d =
      let err = Bset.empty d in
      Se.parse ~err (Bset.p_bindings ~robust:true) (Se.list ~d es)

    let parse_includes_key m = match Bset.find m Bset.Key.w_includes with
    | None -> [], Dict.empty
    | Some (es, d) ->
        let ids = [Se.list ~d es] in
        (fst (Se.pnext ~err:[] (Se.p_list_of (Se.p_atom type_id)) ids d)), d

    let parse_format_key id m =
      let err = Wformat.Default.name in
      match Bset.find m Bset.Key.w_format with
      | Some (es, d) ->
          fst (Se.pnext ~err ~last:true (Se.p_atom type_format) es d)
      | None -> Wlog.err (`Undefined_key ((id :> se), Bset.Key.w_format)); err

    let include_includes c id incs m =
      let include_map c acc id =
        let dep = `Val (id, Bset.Key.w_id) in
        if Wdep.Set.mem dep c.Wctx.seen then (* circular includes detection. *)
          begin
            let d = Wctx.dict_with_trace (snd id) c.Wctx.trace in
            Wlog.err (err_circular d); acc
          end
        else
        let c = Wctx.push_trace c dep [(snd id)] in
        match Wctx.find_map c id with
        | None -> Wlog.err (`Undefined_map id); acc
        | Some m ->
            let add acc k v d = (* trace the origin of include *)
              let t = (snd id) :: match Dict.find d Dict.Key.trace with
                | None -> [Bset.dict m]
                | Some t -> t
              in
              let d = Wctx.dict_with_trace (snd id) t in
              Bset.add ~d acc k v
            in
            Bset.fold add acc m
      in
      let m0 = Bset.empty (Bset.dict m) in
      let mi = List.fold_left (include_map c) m0 (fst incs) in
      Bset.fold (fun a k v d ->
          let d = Wctx.dict_with_trace d [d] in
          Bset.add ~d a k v) mi m

    let add_creation_deps c m =
      let deps = Wctx.deps c in
      let add_includes acc id = match Wctx.find_map c id with
      | None -> Wlog.err (`Undefined_map id); acc
      | Some _ -> Wdep.Set.add (`Map id) acc
      in
      let deps = List.fold_left add_includes deps (fst (includes m)) in
      let dict' = Dict.add (Bset.dict m) key_creation_deps deps in
      Bset.with_dict m dict'

    let fallback_fm = Wformat.Default.name, Wformat.default

(*
    let init_format c id (es, d) =
      let c = Wctx.with_locale c None in
      let m = parse_map es d in
      let incs = parse_includes_key m in
      let m = include_includes c id incs m in
      let fm = parse_format_key id m in
      let fm, fM = match Wformat.Private.find fm with
      | Some fM -> fm, fM
      | None -> Wlog.err (msg_format_undefined fm); fallback_fm
      in
      let m = add_reserved_keys fm id incs m in
      let module F = (val fM : Wformat.T) in
      let m, (fm, fM) = match add_format_keys F.keys m with
      | `Ok m -> m, (fm, fM) | `Error -> m, fallback_fm
      in
      let module F = (val fM : Wformat.T) in
      let c = Wctx.record_deps c in
      match Wformat.protect ~fail:None fM m (F.create c) m with
      | None -> add_reserved_keys Wformat.Default.name id incs m
      | Some m -> add_creation_deps c (add_reserved_keys fm id incs m)
*)


    let prepare c id (es, d) =
      let c = Wctx.with_locale c None in
      let m = parse_map es d in
      let incs = parse_includes_key m in
      let m = include_includes c id incs m in
      let fm = parse_format_key id m in
      let fm, fM = match Wformat.Private.find fm with
      | Some fM -> fm, fM
      | None -> Wlog.err (msg_format_undefined fm); fallback_fm
      in
      let m = add_reserved_keys fm id incs m in
      let module F = (val fM : Wformat.T) in
      let m, (fm, fM) = match add_format_keys F.keys m with
      | `Ok m -> m, (fm, fM) | `Error -> m, fallback_fm
      in
      add_reserved_keys fm id incs m

    let create c (m : Bset.t) =
      let c = Wctx.with_locale c None in
      let id = Dict.get (Bset.dict m) Dict.Key.id in
      let fm = Dict.get (Bset.dict m) Dict.Key.format in
      let incs = Dict.get (Bset.dict m) Dict.Key.includes in
      let fm, fM = match Wformat.Private.find fm with
      | Some fM -> fm, fM
      | None -> Wlog.err (msg_format_undefined fm); fallback_fm
      in
      let module F = (val fM : Wformat.T) in
      let c = Wctx.record_deps c in
      match Wformat.protect ~fail:None fM m (F.create c) m with
      | None -> add_reserved_keys Wformat.Default.name id incs m
      | Some m -> add_creation_deps c (add_reserved_keys fm id incs m)

  end

  (* URI set and content *)

  let uri_set c m =
    let fM = Wformat.get (format m) in
    let module F = (val fM : Wformat.T) in
    let c = Wctx.with_locale c None in
    Wformat.protect ~fail:Wuri.Set.empty fM m (F.uri_set c) m

  let uri_content c m u d =
    let fM = Wformat.get (format m) in
    let module F = (val fM : Wformat.T) in
    let c = Wctx.with_locale c (Wuri.locale u) in
    Wformat.protect ~fail:() fM m (F.uri_content c m u) d

  (* Dependencies *)

  let creation_deps c m = Dict.get (Bset.dict m) Private.key_creation_deps

  let uri_set_deps c m =
    let c = Wctx.record_deps c in
    ignore (uri_set c m);
    Wctx.deps c

  let uri_deps c m u =
    let c = Wctx.record_deps c in
    let d = `Fun (fun _ -> ()) in  (* dry run *)
    uri_content c m u d;
    Wctx.deps c

  (* Diagnostics *)

  let diagnose c m d =
    let fM = Wformat.get (format m) in
    let module F = (val fM : Wformat.T) in
    let c = Wctx.with_locale c None in
    Wformat.protect ~fail:() fM m (F.diagnose c m) d
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
