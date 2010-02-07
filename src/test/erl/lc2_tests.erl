-module(lc2_tests).

-export([test/0]).

test() ->
    %% Nested list comprehension test.
    [{X,Y} || X <- [1,2,3,4], Y <- [a,b,c,d]].
