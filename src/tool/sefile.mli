(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Wg

(** S-expression IO from files. *)

(** {1 S-expression IO} *)

val input_se_list : string -> (se list * dict) option
(** [input_se_list f] inputs an s-expression list from [f]. *)

val input_full_se_list : string -> (dict Seio.Full.t list * dict) option
(** [input_full_se_list f] input an s-expression list from [f]. *)

val output_full_se_list : string -> 'a Seio.Full.t list -> unit
(** [output_full_se_list f es] output an s-expression list to [f]. *)
