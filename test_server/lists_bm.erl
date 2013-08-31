-module(lists_bm).
-export([benchmarks/0,last/1]).

benchmarks() ->
    {100000,[last]}.

last(Iter) ->
    last(Iter, lists:seq(0, 63)).

last(0, List) -> ok;
last(Iter, List) ->
    lists:last(List),
    last(Iter-1, List).

    
