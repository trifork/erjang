-module(call_tail_bm).
-export([benchmarks/0,local_tail_call/1,external_tail_call/1]).
-export([foo/0]).

-define(rep5(X), X, X, X, X, X).
-define(rep10(X), ?rep5(X), ?rep5(X)).
-define(rep20(X), ?rep10(X), ?rep10(X)).

benchmarks() ->
    {400000,[local_tail_call,external_tail_call]}.

local_tail_call(0) ->
    ok;
local_tail_call(Iter) ->
    ?rep20(local_tail_call()),
    local_tail_call(Iter-1).

local_tail_call() ->
    foo().

external_tail_call(0) ->
    ok;
external_tail_call(Iter) ->
    ?rep20(external_tail_call()),
    external_tail_call(Iter-1).

external_tail_call() ->
    ?MODULE:foo().

foo() -> ok.

