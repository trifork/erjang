
java -ea -cp erjang-0.1.jar \
    -Derjpath=/sw/lib/erlang/lib/erts-5.7.3/ebin erjang.OTPMain \
    /sw/lib/erlang/erts-5.7.3/bin/erl \
    -- \
    -boot start \
    -root /sw/lib/erlang \
    -progname erl \
    -home $HOME \
    -init_debug \
    -loader_debug

