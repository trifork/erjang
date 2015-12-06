#!/bin/bash

ERJANG_ROOT=`dirname "$0"`
ERJANG_VSN=0.2

#========== Parameter checks and reads:
if [ $# -lt 1 ] ; then
    echo "Usage: $0 release-dir" >&2
    exit 1
fi

reldir="$1"
ertsdir=`ls -1d "$reldir/erts-"* | head -1`
erjreldir="$reldir/erjang-$ERJANG_VSN"

#========== Sanity checks:
[ -f "$ertsdir/bin/erlexec" ] || { echo "Can't see $ertsdir/bin/erlexec" ; exit 1; }

#========== Changes:
# Additions:
mkdir -p "$erjreldir" || exit 1
cp -v "$ERJANG_ROOT/erjang-$ERJANG_VSN.jar" "$erjreldir/" || exit 1
cp -r "$ERJANG_ROOT/bootstrap" "$erjreldir/" || exit 1
cp -r "$ERJANG_ROOT/jnif" "$erjreldir/" || exit 1

# In-place changes:
[ -f "$ertsdir/bin/erlexec.org" ] || cp -v "$ertsdir/bin/erlexec"{,.org}
cp -v "$ERJANG_ROOT/jerlexec" "$ertsdir/bin/erlexec" || exit 1

#==========
echo "OK, $reldir has been erjangified."

