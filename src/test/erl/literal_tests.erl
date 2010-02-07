-module(literal_tests).

-export([test/0]).

test() ->
    %% Test literals of different kinds.
    {id(0), id(0.0),
     id(123), id(123456789), id(123456789123456789123456789), id(1.0e299),
     id(-123), id(-123456789), id(-123456789123456789123456789), id(-1.0e299),
     id("Hello"), id(<<"World">>), id($!),
     id({}), id({a,pair}), id({a, quadruple, 'of', atoms}),
     id([]), id([element]), id(["two", "elements"]),
     id([unpure|list]), id(["another", "unpure" | list])
    }.

id(X) -> X.
