-module(lc_bm).
-export([benchmarks/0,ident_nolc/1,ident_lc/1,filter_nolc/1,filter_lc/1]).

benchmarks() ->
    {10000,[ident_nolc,ident_lc,filter_nolc,filter_lc]}.

ident_nolc(Iter) ->
    ident_nolc(Iter, lists:seq(0, 128)).

ident_nolc(0, List) ->
    ok;
ident_nolc(Iter, List) ->
    ident(List),
    ident_nolc(Iter-1, List).

ident([H|T]) -> [H|ident(T)];
ident([]) -> [].

ident_lc(Iter) ->
    ident_lc(Iter, lists:seq(0, 128)).

ident_lc(0, List) ->
    ok;
ident_lc(Iter, List) ->
    [X || X <- List],
    ident_lc(Iter-1, List).

filter_nolc(Iter) ->    
    filter_nolc(Iter, lists:seq(0, 128)).

filter_nolc(0, List) ->
    ok;
filter_nolc(Iter, List) ->
    filter(List),
    filter_nolc(Iter-1, List).

filter([H|T]) when H < 64 -> [H|filter(T)];
filter([H|T]) -> filter(T);
filter([]) -> [].
    
filter_lc(Iter) ->    
    filter_lc(Iter, lists:seq(0, 128)).

filter_lc(0, List) ->
    ok;
filter_lc(Iter, List) ->
    [X || X <- List, X < 64],
    filter_lc(Iter-1, List).
