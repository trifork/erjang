-module(sort_bm).
-export([benchmarks/0,sorted/1,reversed/1,random/1,all_same/1]).

benchmarks() ->
    {10000,[sorted,reversed,random,all_same]}.

sorted(Iter) ->
    do_sort(Iter, lists:seq(0, 63)).
    
reversed(Iter) ->
    do_sort(Iter, lists:reverse(lists:seq(0, 63))).

random(Iter) ->
    random:seed(344, 42, 19),
    L = [random:uniform(100) || X <- lists:seq(0, 63)],
    do_sort(Iter, L).

all_same(Iter) ->
    do_sort(Iter, lists:duplicate(64, 42)).

do_sort(0, List) -> ok;
do_sort(Iter, List) ->
    lists:sort(List),
    do_sort(Iter-1, List).

    
