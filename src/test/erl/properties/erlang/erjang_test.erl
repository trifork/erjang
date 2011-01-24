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

-export([test/0, echo/1, echo2/1]).


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
	    {xany(),xany(),
	     elements(['>', '<', 
		       '==', '=:=', '/=', 
		       '=<', '>=', 
		       '++',
		       '+', '-', '/', '*', 'div',
%		       'bsl', 'bsr',
		       'or'
		      ])},
	    begin
		Here  = here(OP,[A,B]),
		There = other(OP,[A,B]),
		?WHENFAIL(io:format("here=~p~nthere=~p~n~n", [Here,There]),
			  Here == There)
	    end).

smaller(Domain) ->
    ?SIZED(SZ, triq_dom:resize(random:uniform((SZ div 2)+1), Domain)).

-define(MIN_INT32, (-(1 bsl 31))).
-define(MAX_INT32, ((1 bsl 31))).

-define(MIN_INT64, (-(1 bsl 63))).
-define(MAX_INT64, ((1 bsl 63))).


xany()  ->
    oneof([int(), real(), bool(), atom(), 

	   %% also test integers around +/- MIN_INT32 limits
	   choose(?MIN_INT32-10, ?MIN_INT32+10),
	   choose(?MAX_INT32-10, ?MAX_INT32+10),

	   %% also test integers around +/- MIN_INT64 limits
	   choose(?MIN_INT64-10, ?MIN_INT64+10),
	   choose(?MAX_INT64-10, ?MAX_INT64+10),

	   [smaller(?DELAY(any())) | smaller(?DELAY(any()))],

	   %% list(any()), but with a size in the range 1..GenSize
	   list(smaller(?DELAY(any()))),

	   tuple(smaller(?DELAY(any())))

	  ]).


prop_encode() ->
    ?FORALL({T,ENC,DEC}, {xany(), 
			oneof([server(), node()]), 
			oneof([server(), node()])},
	    begin
		B = call(ENC, 'erlang', 'term_to_binary', [T]),
		R = call(DEC, 'erlang', 'binary_to_term', [B]),
		?WHENFAIL(io:format("T=~p, R=~p, B=~p~n", [T,R,B]),
			  T == R)
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
test() ->
    triq:check(prop_echo()).

