%%
%% This file is part of Triq - Trifork QuickCheck
%%
%% Copyright (c) 2010 by Trifork
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%  
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

%%
%% Remote equivalence tests for erjang/erlang
%% name of remote server is given with -other NAME
%% on command line (assumed to be localhost).
%%

% ../../../ej -sname erj@mcilroy -pz ~/OSS/triq/ebin/ -other erl 
% c(ets_test, [{i, "/home/erik/OSS/triq/include"}]).
% ets_test:main().

% Other:
% erl -pz ~/OSS/triq/ebin/ -sname erl

-module(ets_test).

-include("triq.hrl").

-export([main/0, ets_behaviour/1, ets_behaviour_wrapper/1]).


%% ===========================================
%% 
%% ===========================================
host([$@|Rest]) ->
    Rest;
host([_|T]) ->
    host(T).
host() ->
    host(atom_to_list(node())).

server() ->
    {ok,[[Other]]} = init:get_argument(other),
    list_to_atom(Other++[$@|host()]).

call(Node,Mod,Fun,Args) ->
    case rpc:call(Node, Mod,Fun,Args) of
	{badrpc,{'EXIT',{Reason,[FirstTrace|_]}}} ->
	    {badrpc, {'EXIT',{Reason,[FirstTrace]}}};
	Value -> Value
    end.    

%% ===========================================
%% Run Mod:Fun(Args) on beam
%% ===========================================
other(Mod,Fun,Args) when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    call(server(), Mod,Fun,Args).

other(Fun,Args) when is_list(Args) ->
    call(server(), erlang,Fun,Args).

ets_behaviour_wrapper(Prg) ->
    process_flag(trap_exit, true),
    io:format("ets_behaviour_wrapper: Prg=~p\n", [Prg]),
    try
	ets_behaviour(Prg)
   after [catch(ets:delete(T)) || T <- [table1, table2]]
   end.

ets_behaviour([]) -> [];
ets_behaviour([Cmd | Rest]) ->
    try ets_do(Cmd) of
	Res -> [{ok, Cmd, Res} | ets_behaviour(Rest)]
    catch A:B ->
	    [{error, Cmd, A, B, [scrub(catch(lists:sort(ets:tab2list(T)))) || T <- [table1, table2]]} | ets_behaviour(Rest)]
    end.

scrub({'EXIT', {R, ST}}) -> {'EXIT', {R, hd(ST)}}; % Interpreter stacktraces aren't to be relied upon.
scrub(X) -> X.


ets_do({new, Name, Options}) -> ets:new(Name, [named_table | Options]);
ets_do({insert, Tab, Item})  -> ets:insert(Tab, Item);
ets_do({insert_new, Tab, Item}) -> ets:insert_new(Tab, Item);
ets_do({lookup, Tab, Key}) -> ets:lookup(Tab, Key);
ets_do({lookup_element, Tab, Key, Pos}) -> ets:lookup_element(Tab, Key, Pos);
ets_do({member, Tab, Key}) -> ets:member(Tab, Key);
ets_do({delete, Tab}) -> ets:delete(Tab);
ets_do({delete, Tab, Key}) -> ets:delete(Tab, Key);
ets_do({match_object, Tab, Pattern}) -> ets:match_object(Tab, Pattern);
ets_do({match_delete, Tab, Pattern}) -> ets:match_delete(Tab, Pattern);
ets_do({info, Tab}) -> [{P,ets:info(Tab,P)}
			|| P <- [name, type, size, named_table, keypos, protection]];
ets_do({key_walk, Tab, Dir}) -> sort_if_unordered(key_walk(Tab, Dir), Tab).


sort_if_unordered(L, Tab) ->
    case ets:info(Tab,type) of
	ordered_set -> L;
	_ -> lists:sort(L)
    end.

key_walk(Tab, forward) ->
    key_walk(Tab, ets:first(Tab), fun ets:next/2, []);
key_walk(Tab, backward) ->
    key_walk(Tab, ets:last(Tab), fun ets:prev/2, []).

key_walk(_Tab, '$end_of_table', _NextFun, Acc) ->
    Acc;
key_walk(Tab, Key, NextFun, Acc) ->
    key_walk(Tab, NextFun(Tab,Key), NextFun, [Key | Acc]).

%% ===========================================
%% Run erlang:Fun(Args) here
%% ===========================================
here(Mod,Fun,Args) ->
    call(node(),Mod,Fun,Args).
here(Fun,Args) ->
    call(node(),erlang,Fun,Args).



smaller(Domain) ->
%    ?SIZED(SZ, triq_dom:resize(random:uniform((SZ div 2)+1), Domain)).
    ?SIZED(SZ, triq_dom:resize(random:uniform(round(math:sqrt(SZ)+1)), Domain)).

-define(MIN_INT32, (-(1 bsl 31))).
-define(MAX_INT32, ((1 bsl 31))).

-define(MIN_INT64, (-(1 bsl 63))).
-define(MAX_INT64, ((1 bsl 63))).

table_name() ->
    oneof([table1, table2]).

table_key() ->
    ?SUCHTHAT(X,oneof([key1, "key2", 123, 123.0, smaller(?DELAY(any2()))]), X/=[]). % Workaround "[] key in ordered_set" bug in erts<5.8.1 .

table_tuple() ->
    ?LET({K,L}, {table_key(), list(smaller(oneof([any2(), pattern()])))},
	 list_to_tuple([K | L])).

table_tuple_pattern() ->
    ?LET({K,L}, {oneof([table_key(), pattern()]), list(pattern())},
	 list_to_tuple([K | L])).

pattern() ->
    oneof([pattern_special(), list(smaller(?DELAY(pattern()))), tuple(smaller(?DELAY(pattern()))), any2()]).

pattern_special() ->
    oneof(['_', '$1', '$2', '$3', '$10', '$01', '$$']).

table_type() ->
%%     oneof([set, bag, duplicate_bag, ordered_set]).
    oneof([set, ordered_set]).

ets_cmd() ->
    oneof([{new, ?DELAY(table_name()), [table_type(), ?LET(X, oneof([1,1,1,2,choose(1,100)]), {keypos, X})]},
	   {insert, table_name(), table_tuple()},
	   {insert_new, table_name(), table_tuple()},
	   {lookup, table_name(), table_key()},
	   {lookup_element, table_name(), table_key(), smaller(?DELAY(int()))},
	   {member, table_name(), table_key()},
	   {delete, table_name()},
	   {delete, table_name(), table_key()},
	   {match_object, table_name(), table_tuple_pattern()},
	   {match_delete, table_name(), table_tuple_pattern()},
	   {info, table_name()},
	   {key_walk, table_name(), ?SUCHTHAT(X, oneof([forward, backward]), X/=backward)}  % Implementation of prev & last is missing yet
	  ]).

any2() ->
    oneof([int(), real(), bool(), atom(),

	   [smaller(?DELAY(any())) | smaller(?DELAY(any()))],

	   %% list(any()), but with a size in the range 1..GenSize
	   list(smaller(?DELAY(any()))),

	   tuple(smaller(?DELAY(any()))),

	   ?LET(X, list(choose(0,255)), list_to_binary(X))

	  ]).


ets_program() ->
    smaller(list(ets_cmd())).


prop_same_ets_behaviour() ->
    ?FORALL(X, ets_program(),
	    begin
		Here = here(?MODULE, ets_behaviour_wrapper, [X]),
		There = other(?MODULE, ets_behaviour_wrapper, [X]),
		if Here /= There -> io:format("Diff: here=~p,\n     there=~p~n", [Here,There]); true -> ok end,
		Here==There
	    end).

%%
%% run the test
%%
main() ->
    triq:check(prop_same_ets_behaviour()).

