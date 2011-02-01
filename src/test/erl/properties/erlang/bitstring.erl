%%
%% This file is part of Triq - Trifork QuickCheck
%%
%% Copyright (c) 2011 by Trifork
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

-module(bitstring).

-include("triq.hrl").

-compile(export_all).
-export([test/0, cons_bitstring/2, cons2_bitstring/4]).


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

%% ===========================================
%% Run erlang:Fun(Args) here
%% ===========================================
here(Mod,Fun,Args) ->
    call(node(),Mod,Fun,Args).


%% ===========================================
%% Property test for bitstrings
%% ===========================================

smaller(Domain) ->
    ?SIZED(SZ, triq_dom:resize(round(math:sqrt(SZ)), Domain)).


%% TODO: Check both 'little' and 'big' modifiers
%% TODO: Check unit-size modifiers
cons_bitstring(X,N) ->
    <<X:N>>.
cons2_bitstring(X1,N1, X2,N2) ->
    <<X1:N1, X2:N2>>.

prop_bitstring_cons() ->
    ?FORALL({X,N}, {int(), smaller(int())},
	    begin
		Here  = here (?MODULE, cons_bitstring, [X,N]),
		There = other(?MODULE, cons_bitstring, [X,N]),
		?WHENFAIL(io:format("here=~p~nthere=~p~n~n", [Here,There]),
			  Here == There)
	    end).

prop_bitstring_cons2() ->
    ?FORALL({X1,N1,X2,N2}, {int(), smaller(int()),
			    int(), smaller(int())},
	    begin
		Here  = here (?MODULE, cons2_bitstring, [X1,N1,X2,N2]),
		There = other(?MODULE, cons2_bitstring, [X1,N1,X2,N2]),
		?WHENFAIL(io:format("here=~p~nthere=~p~n~n", [Here,There]),
			  Here == There)
	    end).


%%
%% run the test
%%
test() ->
    triq:check(prop_bitstring_cons()).
