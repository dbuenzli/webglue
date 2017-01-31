#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let lib_dir =
  let doc = "Use $(docv) for the location of format plugin installs" in
  Conf.(key "lib-dir" fpath ~absent:"src/formats" ~doc)

let lib_dir_config c = match Conf.build_context c with
| `Dev -> Ok () (* Do nothing, the repo src/tool/libdir.ml will do *)
| `Pin | `Distrib ->
    let config = strf "let value = %S" (Conf.value c lib_dir) in
    OS.File.write "src/tool/libdir.ml" config

let () =
  let build = Pkg.build ~pre:lib_dir_config () in
  Pkg.describe "webglue" ~build @@ fun c ->
  Ok [ Pkg.mllib "src/api/wg.mllib";
       Pkg.mllib ~api:[] "src/formats/w_css.mllib";
       Pkg.mllib ~api:[] "src/formats/w_text.mllib";
       Pkg.mllib ~api:[] "src/formats/w_files.mllib";
       Pkg.mllib ~api:[] "src/formats/w_timestamp.mllib";
       Pkg.mllib ~api:[] "src/formats/w_sitemap.mllib";
       Pkg.mllib ~api:[] "src/formats/w_xml.mllib";
       Pkg.bin "src/tool/webglue";
       Pkg.share "share/dotemacs";
       Pkg.share "share/example/home.map" ~dst:"example/";
       Pkg.share "share/example/locale-menus.map" ~dst:"example/";
       Pkg.share "share/example/text.map" ~dst:"example/"; ]
