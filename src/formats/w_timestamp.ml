(*---------------------------------------------------------------------------
   Copyright (c) 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

module Timestamp = struct
  include Wformat.Default

  let name = Se.atom "w.timestamp"
  let doc = "a map with keys to the current time"
  let man = [
    Wformat.man_description;
    `P "The format $(b,w.timestamp) defines the map keys $(b,utc) and
        $(b,local) with S-expression timestamps of the current time.";
    Wformat.man_uri_set;
    `P "The URI set is empty." ]

  let k_utc = Bset.key "utc"
  let k_local = Bset.key "local"
  let keys = [
    k_utc, `Derived, "The current UTC time.";
    k_local, `Derived,
    "The current local time with the time zone offset.";]

  let se_of_time ~d t tz =
    let str = Printf.sprintf in
    let date = Se.list ~d [
      Se.atom ~d (str "%04d" (t.Unix.tm_year + 1900));
      Se.atom ~d (str "%02d" (t.Unix.tm_mon + 1));
      Se.atom ~d (str "%02d" t.Unix.tm_mday)]
    in
    let time = Se.list ~d [
      Se.atom ~d (str "%02d" t.Unix.tm_hour);
      Se.atom ~d (str "%02d" t.Unix.tm_min);
      Se.atom ~d (str "%02d" t.Unix.tm_sec)]
    in
    let timezone = match tz with
    | None -> Se.list ~d [ Se.atom "Z" ];
    | Some m ->
	let m' = abs m in Se.list ~d [
	Se.atom ~d (if m < 0 then "-" else "+");
	Se.atom ~d (str "%02d" (m' / 60));
	Se.atom ~d (str "%02d" (m' mod 60))]
    in
    Se.list ~d [date; time; timezone]

  let tz_offset local utc =      (* computes the timezone offset w.r.t. utc. *)
    let dd = local.Unix.tm_yday - utc.Unix.tm_yday in
    let dh = local.Unix.tm_hour - utc.Unix.tm_hour in
    let dm = dh * 60 + (local.Unix.tm_min - utc.Unix.tm_min) in
    if dd = 1 || dd < -1 (* year wrap *) then dm + (24 * 60) else
    if dd = -1 || dd > 1 (* year wrap *) then dm - (24 * 60) else
    dm (* same day *)

  let create _ m =
    let now = Unix.gettimeofday () in
    let utc = Unix.gmtime now in
    let local = Unix.localtime now in
    let tz_offset = tz_offset local utc in
    let d = Bset.dict m in
    let m = Bset.add m k_utc [(se_of_time ~d utc None)] in
    let m = Bset.add m k_local [(se_of_time ~d local (Some tz_offset))] in
    Some m
end

let () = Wformat.define (module Timestamp : Wformat.T)

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
