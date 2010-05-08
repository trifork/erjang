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

-module(erjang_test).

-include("triq.hrl").

-export([prop_binop/0, prop_echo/0, main/0, echo/1, echo2/1]).


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

echo(Value) ->
    Value.

echo2(Value) ->
    erlang:display(Value),
    [N|_] = erlang:atom_to_list(Value),
    N.


%% ===========================================
%% Run erlang:Fun(Args) here
%% ===========================================
here(Mod,Fun,Args) ->
    call(node(),Mod,Fun,Args).
here(Fun,Args) ->
    call(node(),erlang,Fun,Args).



%% ===========================================
%% Property test for binary operators
%% ===========================================
prop_binop() ->
    ?FORALL({A,B,OP}, 
	    {any(),any(),
	     elements(['>', '<', 
		       '==', '=:=', '/=', 
		       '=<', '>=', 
		       '++',
		       '+', '-', '/', '*', 'div',
		       'bsl', 'bsr',
		       'or'
		      ])},
	    begin
		Here  = here(OP,[A,B]),
		There = other(OP,[A,B]),
%		io:format("here=~p~nthere=~p~n~n", [Here,There]),
		Here == There
	    end).

prop_echo() ->
    ?FORALL(X, any(),
	    begin
		Here = X,
		There = other(?MODULE, echo, [X]),
%		io:format("here=~p, there=~p~n", [Here,There]),
		Here==There
	    end).

%%
%% run the test
%%
main() ->
    triq:check(prop_echo()).

