(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

let _input_se_list file
    (infun : ?err:(Seio.error -> Seio.pos -> unit) -> (Seio.range -> 'a) ->
      Ucutf.input -> 'b) (* doesn't type without annot. why ? *) =
  let input ic =
    let d = Dict.add Dict.empty Dict.Key.filename file in
    let annot r = Dict.add d Dict.Key.char_range r in
    let err_msg e p = Wlog.msg ~d:(annot (p, p)) "%s" (Seio.error_message e) in
    let err e p = Wlog.err (err_msg e p) in
    let i = Ucutf.make_input ~pos:true ~nl:Ucutf.u_lf (`Channel ic) in
    Some (infun ~err annot i, d)
  in
  Sysm.with_inf file None input

let input_se_list file = _input_se_list file Seio.input
let input_full_se_list file = _input_se_list file Seio.Full.input
let output_full_se_list file es =
  let out o = List.iter (Seio.Full.output (Seio.make_output (`Channel o))) es in
  Sysm.with_outf file () out

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
