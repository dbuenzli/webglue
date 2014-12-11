(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
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
