(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Log message output. *)

type verbosity = [ `Verbose | `Quiet | `Normal ]
(** The type for log output verbosity. *)

val reporter : verbosity -> Format.formatter ->  Wg.Wlog.Private.reporter
(** [reporter trace verbosity ppf] reports webglue log messages on [ppf]
    with given [verbosity]. *)
