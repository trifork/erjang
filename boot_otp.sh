
if [ "x${OTPROOT}" == "x" ]; then
  OTPROOT=/sw/lib/erlang
fi

java -ea -cp erjang-0.1.jar \
    -Derjpath=$OTPROOT/lib/erts-5.7.3/ebin erjang.OTPMain \
    $OTPROOT/erts-5.7.3/bin/erl \
    -- \
    -boot start \
    -root $OTPROOT \
    -progname erl \
    -home $HOME \
    -init_debug \
    -loader_debug

