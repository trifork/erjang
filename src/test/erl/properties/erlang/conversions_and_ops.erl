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

-module(conversions_and_ops).

-include("triq.hrl").

-compile(export_all).
-export([test/0, eval_expression_tree/1]).


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
%% Property test for small 'programs' (expression trees)
%% ===========================================
smaller(Domain) ->
    ?SIZED(SZ, triq_dom:resize(random:uniform((SZ div 2)+1), Domain)).

%% smaller_tree() ->
%%     smaller(?DELAY(program_tree())).
%% program_tree() ->
%%     oneof([{literal, any()},
%% 	   {unop, unop(), smaller_tree()},
%% 	   {binop, binop(), smaller_tree(), smaller_tree()}
%% 	  ]).

%% unop() ->
%%     oneof([fun erlang:term_to_binary/1, fun erlang:binary_to_term/1,
%% 	   fun erlang:list_to_binary/1, fun erlang:binary_to_list/1,
%% 	   fun erlang:atom_to_binary/1, fun erlang:binary_to_atom/1,
%% 	   fun erlang:integer_to_list/1, fun erlang:list_to_integer/1,
%% 	   fun erlang:float_to_list/1, fun erlang:list_to_float/1,
%% 	   fun lists:flatten/1,
%% 	   fun lists:iolist_to_binary/1,
%% 	   fun erlang:'-'/1,
%% 	   fun erlang:'not'/1,
%% 	   fun erlang:'bnot'/1,

%% 	   fun erlang:'abs'/1,
%% 	   fun erlang:'float'/1,
%% 	   fun erlang:'trunc'/1,
%% 	   fun erlang:'round'/1,

%% 	   fun erlang:'hd'/1,
%% 	   fun erlang:'tl'/1,

%% 	   fun erlang:'size'/1,
%% 	   fun erlang:'tuple_size'/1,
%% 	   fun erlang:'byte_size'/1,
%% 	   fun erlang:'bit_size'/1,
%% 	   fun erlang:'iolist_size'/1,

%% 	   fun erlang:'list_to_tuple'/1,
%% 	   fun erlang:'tuple_to_list'/1,

%% 	   fun erlang:'crc2'/1,
%% 	   fun erlang:'adler32'/1,
%% 	   fun erlang:'md5'/1
%% 	  ]).

%% binop() ->
%%     oneof([fun erlang:'++'/2,
%% 	   fun erlang:'--'/2,

%% 	   fun erlang:'+'/2,
%% 	   fun erlang:'-'/2,
%% 	   fun erlang:'*'/2,
%% 	   fun erlang:'/'/2,
%% 	   fun erlang:'div'/2,
%% 	   fun erlang:'rem'/2,

%% 	   fun erlang:'=='/2,
%% 	   fun erlang:'=:='/2,
%% 	   fun erlang:'=/='/2,
%% 	   fun erlang:'=/='/2,
%% 	   fun erlang:'<'/2,
%% 	   fun erlang:'>'/2,
%% 	   fun erlang:'=<'/2,
%% 	   fun erlang:'>='/2,

%% 	   fun erlang:'max'/2,
%% 	   fun erlang:'min'/2,

%% 	   fun erlang:'and'/2,
%% 	   fun erlang:'or'/2,
%% 	   fun erlang:'xor'/2,
%% 	   fun erlang:'band'/2,
%% 	   fun erlang:'bor'/2,
%% 	   fun erlang:'bxor'/2,
%% 	   fun erlang:'bsl'/2,
%% 	   fun erlang:'bsr'/2,

%% 	   fun erlang:integer_to_list/2,

%% 	   fun erlang:'element'/2,
%% 	   fun erlang:'make_tuple'/2,
%% 	   fun erlang:'append_element'/2
%% 	  ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Program trees returning values of specific types:
smaller_tree(Type) when is_atom(Type) ->
    smaller(?DELAY(program_tree(Type)));
smaller_tree(Domain) ->
    ?LET(X, Domain, smaller_tree(X)).

program_tree(integer) ->
    oneof([{literal, int()},
	   oneof([
	   {unop, fun erlang:'-'/1,    smaller_tree(integer)},
	   {unop, fun erlang:'bnot'/1, smaller_tree(integer)},
	   {unop, fun erlang:'abs'/1,  smaller_tree(integer)},
	   {unop, fun erlang:list_to_integer/1,  smaller_tree(list)},
 	   {unop, fun erlang:'trunc'/1,  smaller_tree(float)},
	   {unop, fun erlang:'round'/1,  smaller_tree(float)},
	   {unop, fun erlang:'length'/1, smaller_tree(list)},
	   {unop, fun erlang:'size'/1,   smaller_tree(oneof([tuple,binary]))},
	   {unop, fun erlang:'tuple_size'/1, smaller_tree(tuple)},
	   {unop, fun erlang:'byte_size'/1,  smaller_tree(binary)},
	   {unop, fun erlang:'bit_size'/1,   smaller_tree(binary)},
	   {unop, fun erlang:'iolist_size'/1,smaller_tree(oneof([list,binary]))},
%NotImplemented:	   {unop, fun erlang:'crc32'/1,      smaller_tree(binary)},
%NotImplemented:	   {unop, fun erlang:'adler32'/1,    smaller_tree(binary)},

	   {binop, oneof([fun erlang:'+'/2,
			  fun erlang:'-'/2,
			  fun erlang:'*'/2,
			  fun erlang:'div'/2,
			  fun erlang:'rem'/2,

			  fun erlang:'max'/2,
			  fun erlang:'min'/2,

			  fun erlang:'band'/2,
			  fun erlang:'bor'/2,
			  fun erlang:'bxor'/2,
			  fun erlang:'bsl'/2,
			  fun erlang:'bsr'/2
			  ]),
	    smaller_tree(integer), smaller_tree(integer)}
	  ])]);
program_tree(float) ->
    oneof([{literal, real()},
	   oneof([
	   {unop, fun erlang:list_to_float/1, smaller_tree(list)},
	   {unop, fun erlang:'abs'/1,         smaller_tree(float)},
	   {unop, fun erlang:'float'/1,       smaller_tree(number)},
	   {binop, oneof([fun erlang:'+'/2,
			  fun erlang:'-'/2,
			  fun erlang:'*'/2,
			  fun erlang:'/'/2,
			  fun erlang:'max'/2,
			  fun erlang:'min'/2
			 ]),
	    smaller_tree(number), smaller_tree(number)} % Well, almost; ought to check whether at least one of the arguments is a float.
	  ])]);
program_tree(atom) ->
    oneof([{literal, atom()},
	   smaller_tree(bool)%,
%NotImplemented: {unop, fun erlang:binary_to_atom/1, smaller_tree(binary)}
	   ]);
program_tree(bool) ->
    oneof([{unop, fun erlang:'not'/1,          smaller_tree(atom)},
	   {binop, oneof([fun erlang:'=='/2,
			  fun erlang:'=:='/2,
			  fun erlang:'=/='/2,
			  fun erlang:'=/='/2,
			  fun erlang:'<'/2,
			  fun erlang:'>'/2,
			  fun erlang:'=<'/2,
			  fun erlang:'>='/2]),
	    smaller_tree(term), smaller_tree(term)},
	   {binop, oneof([fun erlang:'and'/2,
			  fun erlang:'or'/2,
			  fun erlang:'xor'/2]),
	    smaller_tree(bool), smaller_tree(bool)}
	  ]);
program_tree(tuple) ->
    oneof([{literal, tuple(any())},
	   {unop, fun erlang:list_to_tuple/1, smaller_tree(list)},
	   {binop, fun erlang:'make_tuple'/2,
	    smaller_tree(integer), smaller_tree(term)}%,
%NotImplemented:	   {binop, fun erlang:'append_element'/2,
%	    smaller_tree(tuple), smaller_tree(term)}
	   ]);
program_tree(list) ->
    oneof([{literal, list(any())},
	   oneof([
           {unop, fun erlang:binary_to_list/1,  smaller_tree(binary)},
	   {unop, fun erlang:integer_to_list/1, smaller_tree(integer)},
%Erlang version isn't too reliable on last digits   {unop, fun erlang:float_to_list/1,   smaller_tree(float)},
	   {unop, fun erlang:tuple_to_list/1,   smaller_tree(tuple)},
	   {unop, fun lists:flatten/1,          smaller_tree(list)},
	   {binop, oneof([fun erlang:'++'/2, fun erlang:'--'/2]),
	    smaller_tree(list), smaller_tree(list)},
	   {binop, fun erlang:integer_to_list/2,
	    smaller_tree(integer), smaller_tree(integer)}
	   ])]);
program_tree(binary) ->
    oneof([?LET(L, list(choose(0,255)), {literal, list_to_binary(L)}),
	   oneof([
	   {unop, fun erlang:term_to_binary/1,  smaller_tree(term)},
	   {unop, fun erlang:list_to_binary/1,  smaller_tree(list)},
%NotImplemented:	   {unop, fun erlang:atom_to_binary/1,  smaller_tree(atom)},
%NotImplemented:	   {unop, fun lists:iolist_to_binary/1, smaller_tree(iolist)},
	   {unop, fun erlang:'md5'/1,           smaller_tree(iolist)}
		 ])]);
%% Type unions:
program_tree(number) ->
    smaller_tree(oneof([integer, float]));
program_tree(iolist) ->
    oneof([{literal, list(int())},
	   smaller_tree(binary),
	   [smaller_tree(iolist) | smaller_tree(iolist)]
	  ]);
program_tree(term) ->
    oneof([smaller_tree(oneof([integer, float, atom, tuple, list, binary])),
	   {unop, fun erlang:'hd'/1, smaller_tree(list)},
	   {unop, fun erlang:'tl'/1, smaller_tree(list)},
	   {binop, fun erlang:'element'/2, smaller_tree(integer), smaller_tree(tuple)}

	   ]).


eval({literal, V}) -> V;
eval({unop, F, A}) -> F(eval(A));
eval({unop, F, A, B}) -> F(eval(A), eval(B)).

eval_catch_early(T) ->
    try
	case T of
	    {literal, V} -> V;
	    {unop, F, A} -> F(eval_catch_early(A));
	    {binop, F, A, B} -> F(eval_catch_early(A), eval_catch_early(B))
	end
    catch Cls:Reason ->
	    {'exception', Cls, Reason, hd(erlang:get_stacktrace())}
    end.

eval_catch_late(T) -> catch(eval(T)).

eval_expression_tree(T) -> erlang:display("DB"), erlang:display(T), eval_catch_early(T).


%% prop_trees_eval_to_same() ->
%%     ?FORALL(T, program_tree(),
%% 	    begin
%% 		Here  = here (?MODULE, eval_expression_tree_,[T]),
%% 		There = other(?MODULE, eval_expression_tree,[T]),
%% 		?WHENFAIL(io:format("here=~p~nthere=~p~n~n", [Here,There]),
%% 			  Here == There)
%% 	    end).

prop_typed_trees_eval_to_same() ->
    ?FORALL(T, program_tree(term),
	    begin
		Here  = here (?MODULE, eval_expression_tree,[T]),
		There = other(?MODULE, eval_expression_tree,[T]),
		?WHENFAIL(io:format("here=~p~nthere=~p~n~n", [Here,There]),
			  Here == There)
	    end).

%%
%% run the test
%%
test() ->
%%     triq:check(prop_trees_eval_to_same()),
    triq:check(prop_typed_trees_eval_to_same()).


