-module(fun_bm).
-export([benchmarks/0,make_fun/1,no_args/1,one_arg/1,two_args/1,
	 one_free/1,two_free/1]).

-define(rep5(X), X, X, X, X, X).
-define(rep10(X), ?rep5(X), ?rep5(X)).
-define(rep20(X), ?rep10(X), ?rep10(X)).

benchmarks() ->
    {200000,
     [make_fun,
      no_args,
      one_arg,
      two_args,
      one_free,
      two_free]}.

make_fun(0) ->
    ok;
make_fun(Iter) ->
    ?rep20(fun() -> ok end),
    make_fun(Iter-1).

no_args(Iter) ->
    no_args(Iter, fun() -> ok end).
no_args(0, F) -> ok;
no_args(Iter, F) ->
    ?rep20(F()),
    no_args(Iter-1, F).

one_arg(Iter) ->
    one_arg(Iter, fun(X) -> X+1 end).
one_arg(0, F) -> ok;
one_arg(Iter, F) ->
    ?rep20(F(1)),
    one_arg(Iter-1, F).

two_args(Iter) ->
    two_args(Iter, fun(X, Y) -> X+Y end).
two_args(0, F) -> ok;
two_args(Iter, F) ->
    ?rep20(F(3, 2)),
    two_args(Iter-1, F).

one_free(Iter) ->
    one_free(Iter, fun() -> Iter+1 end).
one_free(0, F) -> ok;
one_free(Iter, F) ->
    ?rep20(F()),
    one_free(Iter-1, F).

two_free(Iter) ->
    two_free(Iter, two_free_make_fun(1, 2)).
two_free(0, F) -> ok;
two_free(Iter, F) ->
    ?rep20(F()),
    two_free(Iter-1, F).

two_free_make_fun(X, Y) ->			   
    fun() -> X+Y end.

	    
