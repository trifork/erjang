-module(ref_bm).
-export([benchmarks/0,make_ref/1]).

-define(rep5(X), X, X, X, X, X).
-define(rep10(X), ?rep5(X), ?rep5(X)).
-define(rep20(X), ?rep10(X), ?rep10(X)).

benchmarks() ->
    {200000,
     [make_ref]}.

make_ref(0) -> ok;
make_ref(Iter) ->
    ?rep20(make_ref()),
    make_ref(Iter-1).
