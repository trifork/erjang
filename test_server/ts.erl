%%
%% This file is part of Erjang
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
-module(ts).

-export([run/1, run/2, list/1, count/1]).

%% @doc List tests in module Mod
%% @spec list( atom() ) -> [ atom() ]
list(Mod) when is_atom(Mod) ->
    {module, ModSuite} = compile_and_load(Mod),
    
    lists:map(fun(Test) -> get_list(ModSuite,Test) end,
	      ModSuite:all(suite)).

get_list(Mod,Test) ->
    case catch Mod:Test(suite) of
	[_|_]=L ->
	    {Test, lists:map(fun(Test2) -> get_list(Mod,Test2) end, L)};

        _ -> Test
    end.

%% @doc Run all tests in module Mod (i.e. `Mod ++ "_SUITE"' ).
run(Mod) when is_atom(Mod) ->
    run(Mod,list(Mod)).

%% @doc Run test `What' in module Mod (i.e. `Mod ++ "_SUITE"' ).
run(Mod,What) ->
    {module, ModSuite} = compile_and_load(Mod),
    run(ModSuite,What,[]).

run(ModSuite, What, Acc) when is_atom(ModSuite), is_atom(What) ->
    test_loop (ModSuite, [], [What], [], Acc);

run(ModSuite,{Group,List},Acc) when is_atom(Group) ->
    run(ModSuite,List,Acc);

run(_,[],Acc) -> Acc;

run(ModSuite,[First|Next],Acc) ->
    Acc1 = run(ModSuite,First,Acc),
    run(ModSuite,Next,Acc1).

test_loop(_ModSuite,_Sub,[], _Config, Acc) ->
    Acc;

test_loop(ModSuite,Sub,[Test|Rest], Config, Acc) ->
    case catch ModSuite:Test(suite) of

	[_|_]=L ->
	    io:format("Running test category ~p~n", [lists:reverse([Test|Sub])]),
	    Acc1 = test_loop(ModSuite,[Test|Sub],L,Config, Acc),
	    test_loop(ModSuite,Sub,Rest,Config, Acc1);
	    
	_ ->
	    TestResult = run_single_test(ModSuite,Test,Config),
	    test_loop(ModSuite,Sub,Rest, Config, [TestResult|Acc])
	
    end.
    
    
run_single_test(ModSuite,Test,Config) ->
    
    TestName = [ModSuite,Test],
    io:format("~n~n*** Running ~p~n", [TestName]),

    Config1 = [{test_case, Test} | Config],

    Owner = self(),
    {PID,Ref} = spawn_monitor
		  (fun() ->
			   %erlang:group_leader(Owner, self()),
			   put('$ts$owner', Owner),
			   case Test of
			       smp_unfix_fix -> erlang:throw(skipped);
			       t_repair_continuation -> erlang:throw(skipped);
			       evil_rename -> erlang:throw(skipped);
			       _ ->
				   Result = do_run_single_test(ModSuite,Test,Config1),
				   Owner ! {ok, Result}
			   end
		   end),


    case test_control_loop(PID, Ref) of
	{fail, Info} -> 
%	    erlang:display(Info),
	    io:format("~n*** ~p: ~nFailed with:~p ~n", [TestName,Info]),
	    {failure, {ModSuite,Test}, Info};

	ok -> 
	    io:format("~n*** ~p: Succeeded~n", [TestName]),
	    {success, {ModSuite,Test}}
	
    end.

count(Result) ->
    {Succes,Failure} = 
	lists:foldl(fun({success, _},{S,F}) -> 
			    {S+1,F}; 
		       ({failure, _, _},{S,F}) -> 
			    {S,F+1} end, 
		    {0,0}, 
		    Result),
    io:format("Run completed with ~p successes, and ~p failures; total = ~p%~n", 
	      [Succes, Failure, ((100*Succes) div (Succes+Failure))]).


test_control_loop(PID, Ref) ->

    receive

	{io_request,From,To,{put_chars, Enc,Chars}} ->
	    From ! {io_reply, To, ok},
	    test_control_loop(PID, Ref);

	{io_request,From,To,{put_chars, Enc,M,F,A}} ->
	    From ! {io_reply, To, ok},
	    test_control_loop(PID, Ref);

	{'DOWN', Ref, _, _, Info} -> 
	    {fail, Info};
	
	{ok, Result} ->
	    erlang:display(Result),
	    erlang:demonitor(Ref, [flush]),
	    ok
    end.



%%
%% This is the action that runs in a separate process
%%
do_run_single_test(ModSuite,Test,Config1) ->

    %% initialize test
    {Found, Config2} = safe_apply(ModSuite,init_per_testcase,[Test, Config1], Config1),
    
    %% run test
    try
	ModSuite:Test(Config2)
    after
	if Found -> safe_apply(ModSuite,fin_per_testcase,[Test, Config2], ok), ok;
	   true -> ok
	end
    end.

safe_apply(Mod,Fun,Args,Else) ->
    case lists:member({Fun,length(Args)}, Mod:module_info(exports)) of
	true ->
	    {true, apply(Mod,Fun,Args)};
	false ->
	    {false, Else}
    end.

term_to_list(Term) ->
    IOList = io_lib:format("~w", [Term]),
    Bin = erlang:iolist_to_binary(IOList),
    erlang:binary_to_list(Bin).


compile_and_load(Mod) ->
    ModList = atom_to_list(Mod) ++ "_SUITE",
    ModSuite = list_to_atom(ModList),
    ModSuite:module_info(),
    {module, ModSuite}.

%    case os:find_executable("erlc") of
%	false -> 
%	    io:format("cannot find erlc~n"),
%	    exit(bad);
%    
%	ErlC -> 
%	    Cmd = ErlC ++ " -I . " ++ ModList ++ ".erl",
%	    io:format("Running :~p~n", [Cmd]),
%	    os:cmd(Cmd)
%    end,
%    {ok, BeamData} = file:read_file(ModList ++ ".beam"),
%    catch erlang:purge_module(ModSuite),
%    erlang:load_module(ModSuite, BeamData).




    
