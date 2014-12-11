(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

type verbosity = [ `Verbose | `Quiet | `Normal ]

let file_loc d =
  match Dict.find d Dict.Key.filename, Dict.find d Dict.Key.char_range with
  | None, _ -> None
  | Some f, None -> Some (f, ((1,1), (1,1)))
  | Some f, Some r -> Some (f, r)

let trace d = match Dict.find d Dict.Key.trace with None -> [] | Some l -> l
let full_trace d = match file_loc d with
| None -> trace d | Some _ -> d :: trace d

let factorize_traces d ds = (* extracts the part of d common to all traces. *)
  let rev_trace (name, d) = List.rev (trace d) in
  let retrace (name, d) t = name, Dict.add d Dict.Key.trace (List.rev t) in
  let rec aux acc ft ts = match ft with
  | fd :: fds ->
      let head_fd t = try List.hd t == fd with Failure _ -> false in
      if List.for_all head_fd ts then
	aux (fd :: acc) fds (List.map List.tl ts)
      else
	aux acc [] ts (* finish *)
  | [] -> (retrace d (List.rev acc)), (List.map2 retrace ds ts)
  in
  aux [] (rev_trace d) (List.map rev_trace ds)

(* Printers *)

let pp_file_loc ppf (f, r) = Fmt.pp ppf "@[%s: %a:@]" f Fmt.pp_range r
let pp_file_loc_opt ppf dict = Fmt.pp_opt pp_file_loc ppf (file_loc dict)
let pp_main_loc ppf dict = match file_loc dict with
| None -> () | Some loc -> Fmt.pp ppf "%a@," pp_file_loc loc

let pp_trace h ppf = function [] -> () | t ->
  let rec pr ppf = function  [] -> assert false
    | d :: ds ->
	pp_file_loc_opt ppf d; if ds <> [] then (Fmt.pp_cut ppf (); pr ppf ds)
  in
  Fmt.pp ppf "@,%s@, @[<v>%a@]" h pr t

let pp_eval_trace = pp_trace "evaluation trace:"
let pp_factorized_traces ppf (factor, traces) =
  let pp_trace name = pp_trace (name ^ " trace:") in
  List.iter (fun (name, d) -> pp_trace name ppf (full_trace d)) traces;
  pp_trace (fst factor) ppf (trace (snd factor))

let pp_level ppf level = Fmt.pp ppf "%s:" (match level with
| `Error -> "Error" |`Warning -> "Warning" |`Info -> "Info" |`Debug -> "Debug")

let pp_entry ppf (e : Wlog.entry) = match e with
| `Msg (m, d) ->
    Fmt.pp ppf "@[<v>@[%a@]%a@]" Fmt.pp_text m pp_eval_trace (trace d)
| `Msg_traces (m, d, traces) ->
    let traces = factorize_traces ("evaluation", d) traces in
    Fmt.pp ppf "@[<v>@[%a@]%a@]" Fmt.pp_text m pp_factorized_traces traces
| `Undefined_map (`Atom id, d) ->
    Fmt.pp ppf "@[<v>map ID `%s' undefined%a@]" id pp_eval_trace (trace d)
| `Undefined_key ((_, md as map), (`Atom k, kd)) ->
    let pp_map_spec ppf = function
      | `Atom id, _ -> Fmt.pp ppf "map ID `%s':" id
      | e -> Fmt.pp ppf "map:@, @[%a@]" Se.pp e
    in
    let traces = factorize_traces ("evaluation", md) ["map", md; "key", kd] in
    Fmt.pp ppf "@[<v>key `%s' undefined in %a%a@]" k pp_map_spec map
      pp_factorized_traces traces
| `Expected (this, that) ->
    let pp_this ppf = function
      | `Eol -> Fmt.pp ppf "end of list"
      | `This this -> Fmt.pp ppf "%s" this
    in
    let pp_that ppf = function
      | `Eol d -> Fmt.pp ppf " end of list%a" pp_eval_trace (trace d)
      | `That (that, d) -> Fmt.pp ppf " %s%a" that pp_eval_trace (trace d)
      | `Se e -> Fmt.pp ppf ":@,  @[%a@]%a" Se.pp e
	    pp_eval_trace (trace (snd e))
    in
    Fmt.pp ppf "@[<v>expected %a, found%a@]" pp_this this pp_that that
| `Format_exn ((`Atom fmt, _), e, bt, dict) ->
    Fmt.pp ppf "@[<v>format %s raised:@,%s@,%a%a@]"
      fmt (Printexc.to_string e) Fmt.pp_lines bt pp_eval_trace (trace dict)

let main_loc (e : Wlog.entry) = match e with
| `Msg (_, d)
| `Msg_traces (_, d, _)
| `Expected (_, (`Eol d | `Se (_, d) | `That  (_, d)))
| `Undefined_key (_, (_, d))
| `Undefined_map (_, d)
| `Format_exn (_, _, _, d) -> d

let quiet verbosity = function
  | `Debug when verbosity <> `Verbose -> true
  | (`Info | `Warning) when verbosity = `Quiet -> true
  | _ -> false

let locs = Hashtbl.create 20  (* TODO cleanup *)
let report loc =
  let floc = file_loc loc in
  if Hashtbl.mem locs floc then false else
  (Hashtbl.add locs floc (); true)

let reporter verbosity ppf level (entry : Wlog.entry) =
  if quiet verbosity level then () else
  let main_loc = main_loc entry in
  if report main_loc then
    Fmt.pp ppf "@[<v>@,%a%a %a@,@]@?"
      pp_main_loc main_loc pp_level level pp_entry entry

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
