(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Common definitions for commands. *)

open Wg
open Cmdliner

(** {1 Argument converters} *)

val atom : atom Arg.conv

(** {1 Optional arguments for every command} *)

type copts =
    { verbosity : Log.verbosity; format_dirs : string list;
      map_dirs : string list; dep_base : string; }
(** The type collecting information set by options common to all
    commands. *)

val copts : copts Term.t
(** [copts] defines a set of options common to all commands. *)

val copts_sec : string
(** [copts_sec] is the title of the manual's common options section. *)

val copts_man : Manpage.block list
(** [man_copts] is the contents of the manual's common options section. *)

(** {1 Positional arguments} *)

val id_opt : Wg.id option Term.t
(** [id] is an optional positional map id argument at position [0]. *)

val id : Wg.id Term.t
(** [id] is a positional map id argument at position [0]. *)

val key_opt : atom option Term.t
(** [key_opt] is an optional positional map key argument at position [1]. *)

val key : atom Term.t
(** [key] is a positional map key argument at position [1]. *)

(** {1 Optional arguments} *)

val deps : [> `Deps] * Arg.info
(** [deps] an option information for an option to show dependencies. *)

val recurse : bool Term.t
(** [recurse] an option to recurse on map dependencies. *)

val output : string Term.t
(** [output] an option to define the output destination. *)

val input : string Term.t
(** [input] an option to define the input source. *)

val locale : Wlocale.t option Term.t
(** [locale] an option to define the [w.locale] atom. *)

val conf : atom option Term.t
(** [conf] an option to define the [w.conf] atom. *)

(** {1 Sets of arguments common to some commands} *)

type context =
    { copts : copts; locale : Wlocale.t option; conf : atom option;
      id : Wg.id; c : Wctx.t }

val context : locale_opt:bool -> context Term.t
(** [context locale_opt] uses {!copts}, {!id}, {!locale} (if locale_opt
    is [true]) and {!conf} to define a webglue evaluation context and
    a map. *)

(** {1 Functions} *)

val out_v : string -> 'a Fmt.formatter -> 'a -> unit
(** [out_v outf pp v] prints on [outf] the value [v] in a
    vertical box with [pp]. Uses {!Sysm.with_outf_pp}. *)

val out_deps : string -> string -> Wdep.Set.t -> unit
(** [out_deps outf base ds] prints on [outf] the dependency set [ds].
    File paths are expressed relative to [base] and map ids are
    replaced by their defining filename. Uses {!Sysm.with_outf_pp}. *)
