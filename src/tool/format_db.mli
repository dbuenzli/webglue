(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Format database. *)

open Wg;;

val init : string list -> unit
(** [init dirs] initialize the format database, loading maps from
    the directories [dirs].

    Given a format [prefix.suffix] the database tries to load a file
    [prefix_suffix.cmxs] (or [.cma]) located in [dirs] (recursively
    and tried from left to right). *)

val list : unit -> (Wg.format * string) list
(** [list ()] is the list of defined formats and the location
    of their implementation. *)

include Wformat.Private.Db
