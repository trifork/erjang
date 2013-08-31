-module(freq_bm).
-export([benchmarks/0,vector/1,tuple/1,ets/1]).

benchmarks() ->
    {25,[tuple,ets]}.

tuple(Iter) ->
    tuple(Iter, random_list()).

tuple(0, List) -> ok;
tuple(Iter, List) ->
    Cnt = t_count(List, erlang:make_tuple(256, 0)),
    tuple(Iter-1, List).

t_count([H|T], Cnt) ->    
    I = H+1,
    t_count(T, setelement(I, Cnt, 1+element(I, Cnt)));
t_count([], Cnt) -> Cnt.

vector(Iter) ->
    vector(Iter, random_list()).

vector(0, List) -> ok;
vector(Iter, List) ->
    Cnt = v_count(List, vector:new(256, 0)),
    vector(Iter-1, List).

v_count([H|T], Cnt) ->    
    I = H+1,
    v_count(T, vector:set(I, Cnt, 1+vector:get(I, Cnt)));
v_count([], Cnt) -> Cnt.

ets(Iter) ->
    ets(Iter, random_list()).

ets(0, List) -> ok;
ets(Iter, List) ->
    Tab = ets:new(count, [private,named_table]),
    lists:foreach(fun(Key) -> ets:insert(Tab, {Key,0}) end, lists:seq(0, 255)),
    e_count(List),
    ets:delete(Tab),
    ets(Iter-1, List).

e_count([H|T]) ->
    ets:update_counter(count, H, 1),
    e_count(T);
e_count([]) -> ok.

random_list() ->
    random:seed(44, 255, 73),
    random_list(32767, []).

random_list(0, Acc) -> Acc;
random_list(N, Acc) -> random_list(N-1, [random:uniform(256)-1|Acc]).
