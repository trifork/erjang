#!/bin/bash

EJC_CMD=`which "$0"`
EJC_DIR=`dirname "$EJC_CMD"`
EJC_BIN=`readlink -m "$EJC_DIR"`

source $EJC_BIN/env_cfg

java -ea -cp erjang-0.1.jar \
    -Derjpath=$ERL_ROOT/lib/erts-$ERTS_VSN/ebin erjang.OTPMain \
    $ERL_ROOT/erts-$ERTS_VSN/bin/erl \
    -- \
    -boot start \
    -root $ERL_ROOT \
    -progname erl \
    -home $HOME \
    -init_debug \
    -loader_debug
