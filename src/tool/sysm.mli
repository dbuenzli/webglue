(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** System abstraction.

    All [Unix] module usage is confined to this module. *)

(** {1 Environment} *)

val getenv : string -> string option
(** [getenv key] is the value of [key] in the process environment (if any). *)

(** {1 File system operations} *)

val mkdir : string -> bool
(** [mkdir d] creates the directory [d] and returns [true]. If an error
    occurs, it is logged and [false] is returned. Intermediary directories
    are created as required. *)

val rename : string -> string -> bool
(** [rename oldf newf] renames [oldf] to [newf] and returns [true]. If an error
    occurs, it is logged and [false] is returned. *)

val tmp_file : string -> string option
(** [tmp_file f] is a temporary file in the same location as [f] and
    named after [f]. If an error occurs it is logged and [None] is returned. *)

(** {1 Finding files} *)

val make_relative : string -> string -> string
(** [make_relative p f] is [f] without the prefix [p] or [f] if
    [p] is not a prefix of [f]. *)

val find_file : string list -> string -> string option
(** [find_file dirs file] is the full path to [file] in the first
    directory of [dirs] in which it can be found or None otherwise.  *)

val find_file_rec : string list -> string -> string option
(** [find_file_rec dirs file] is like {!find_file} but sub directories
    are also searched in depth first order. *)

val fold_files_rec : string list -> ('a -> string -> 'a) -> 'a -> 'a
(** [fold_files_rec dirs f acc] lists the files in [dirs], recursively
    in depth first order and folds the function [f] over the file names. *)

(** {1 IO brackets} *)

val with_inf : string -> 'a -> (in_channel -> 'a) -> 'a
(** [with_inf inf err f] is [f ic] where [ic] is an input channel
    opened and closed on [inf] ([stdin] and not closed if [f] is ["-"]).
    If a [Sys_error] occurs it is logged and [err] is returned. *)

val with_outf : string -> 'a -> (out_channel -> 'a) -> 'a
(** [with_outf outf err f] is [f oc] where [oc] is an output channel
    opened and closed on [outf] ([stdout] and not closed if [f] is ["-"]).
    If a [Sys_error] occurs it is logged and [err] is returned. *)

val with_outf_pp : string ->
  ('a, Format.formatter, unit, unit) Pervasives.format4 -> 'a
(** [with_outf_pp outf fmt a0 a1 ...] formats [a0], [a1], ...
    according to [fmt] on a formatter opened and closed on [outf]
    ([stdout] and not closed if [f] is ["-"]).
    If a [Sys_error] occurs it is logged. *)

val with_out_path : string -> 'a -> (out_channel -> 'a) -> 'a
(** [with_out_path] is like {!with_outf} except the path is created if it
    doesn't exist. An error is logged if the path cannot be created. *)

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
