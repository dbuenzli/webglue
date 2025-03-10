(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
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
