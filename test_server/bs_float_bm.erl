-module(bs_float_bm).
-export([benchmarks/0,float/1]).

benchmarks() ->
    {50000,[float]}.

float(Iter) ->
    float(Iter, data()).

float(0, Data) -> ok;
float(Iter, Data) ->
    float1(Data, []),
    float(Iter-1, Data).

float1([F|T], Acc) ->
    float1(T, [<<F/float>>|Acc]);
float1([], Acc) -> list_to_binary(lists:reverse(Acc)).

data() ->
    lists:duplicate(100, 3.14159).

