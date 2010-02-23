#!/bin/sh

CMD=""
for f in src/test/erl/*_tests.beam; do 
    test=`basename $f .beam`
    CMD="$CMD -run $test test"
done

./erl.sh -noshell -pa ./src/test/erl $CMD -run erlang halt
