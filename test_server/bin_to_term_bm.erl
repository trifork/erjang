-module(bin_to_term_bm).
-export([benchmarks/0,t_binary_to_term/1]).

benchmarks() ->
    {300000,[t_binary_to_term]}.

t_binary_to_term(Iter) ->
    Term = {a,{nested,tuple,is},nice,lists:seq(-1, 10),33,self()},
    t_binary_to_term(Iter, term_to_binary(Term), lists:duplicate(1024, 255)).

t_binary_to_term(0, Bin, T) -> ok;
t_binary_to_term(Iter, Bin, T) ->
    binary_to_term(Bin),
    t_binary_to_term(Iter-1, Bin, T).
