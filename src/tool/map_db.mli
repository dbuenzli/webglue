(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

(** Map database. *)

(** {1 Map database} *)

val init : string list -> unit
(** [init dirs] initializes the map database, loading maps
    from the directories [dirs].

    Given a map [id], the database tries to load a file [id.map]
    located in [dirs] (recursively and tried from left to right). *)

val filename : Wg.id -> string option
(** [filename id] is the file name of the map [id] (if any). *)

val find : Wctx.t -> Wg.id -> Wmap.t option
(** [find c id] returns the map with id [id] in context [c] (if any).*)

val find_bset : Wg.id -> Bset.t option
(** [find_bset id] returns the set of bindings with id [id]. The set has
    the bindings as found in the file, without includes and format
    definition. *)

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
