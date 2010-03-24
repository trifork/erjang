#!/bin/bash

if [ "x${OTPROOT}" == "x" ]; then
  OTPROOT=/sw/lib/erlang
fi

java -server -ea -cp erjang-0.1.jar \
    -Derj.threads=2 \
    -Derjpath=$OTPROOT/lib/erts-5.7.3/ebin erjang.OTPMain \
    -root $OTPROOT \
    -progname erl \
    -- \
    -home $HOME \
    $*





