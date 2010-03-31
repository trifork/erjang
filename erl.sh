#!/bin/bash

if [ "x${OTPROOT}" == "x" ]; then
   OTPROOT=/sw/lib/erlang
fi

java -server -jar erjang-0.1.jar \
    -root $OTPROOT \
    $*





