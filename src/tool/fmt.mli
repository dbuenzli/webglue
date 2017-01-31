(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

(** Pretty printers and string formatters. *)

(** {1 Formatters} *)

type 'a formatter = Format.formatter -> 'a -> unit
(** The type for formatters of values of type ['a]. *)

val pp : Format.formatter -> ('a, Format.formatter, unit) Pervasives.format ->
  'a
(** [pp] is {!Format.fprintf} *)

val pp_cut : unit formatter
(** [pp_cut] is {!Format.pp_print_cut}. *)

val pp_sp : unit formatter
(** [pp_sp] is {!Format.pp_print_space}. *)

val pp_str : string formatter
(** [pp_str] is {!Format.pp_print_string}. *)

val pp_opt : ?pp_none:unit formatter -> 'a formatter -> 'a option formatter
(** [pp_opt pp_none pp_v] formats value of type ['a option]. The default
    value of [pp_none] prints nothing. *)

val pp_list : ?pp_sep:unit formatter -> 'a formatter -> 'a list formatter
(** [pp_list pp_sep pp_v ppf] formats lists of type ['a]. Each value
    is printed with [pp_v] followed by [pp_sep] (defaults to {!pp_cut}).
    Empty lists never print anything. *)

val pp_text : string formatter
(** [pp_text] formats text by replacing spaces and newlines in the string
    with calls to {!Format.pp_print_space} and {!Format.pp_force_newline}. *)

val pp_lines : string formatter
(** [pp_lines] formats lines by replacing newlines in the string
    with calls to {!Format.pp_force_newline}. *)

val pp_range : ((int * int) * (int * int)) formatter
(** [pp_range] formats a range. *)


(** {1 String converters} *)

val str : ('a, unit, string) Pervasives.format -> 'a
(** [str] is {!Format.fprintf}. *)

val to_str_converter : 'a formatter -> ('a -> string)
(** [to_str_converter pp_v] is a function converting values to string
    as [pp_v] prints them. *)

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
