#!/bin/sh

(cd src/test/erl; erlc *.erl)

CMD=""
for f in src/test/erl/*_tests.beam; do 
    test=`basename $f .beam`
    CMD="$CMD -s $test test"
done

./erl.sh -noshell -pa ./src/test/erl $CMD -s erlang halt
