#!/bin/sh
set -e

# Caml tools
OCAMLBUILD=${OCAMLBUILD:="ocamlbuild"}
BUILDFLAGS=${BUILDFLAGS:="-classic-display"}

LOCAL=${LOCAL:=-tag "base(site-local)" -tag "conf(local)"}
TEST=${TEST:=-tag "base(site-test)" -tag "conf(test)"}
ONLINE=${ONLINE:=-tag "base(site-online)"}

ocb () { $OCAMLBUILD $BUILDFLAGS "$@" ; }

action () 
{
  case $1 in
    local) ocb $LOCAL site.site ;;
    test) ocb $TEST site.site ;;
    online) ocb $ONLINE site.site ;;
    clean) ocb -clean ;;
    *) 
      ocb $LOCAL $*
      while [ $# -gt 0 ]; do shift ; done
      ;;
  esac;
} 

if [ $# -eq 0 ]; then action local ; else
  while [ $# -gt 0 ]; do action $1; shift ; done
fi