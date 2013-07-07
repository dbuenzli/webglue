(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Common definitions for commands. *)

open Wg
open Cmdliner

(** {1 Argument converters} *)

val atom : atom Arg.converter

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
