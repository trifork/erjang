-module(lc_tests).

-export([test/0]).

test() ->
    %% Basic list comprehension test.
    [{X,X} || X <- [1,2,3,4]].
