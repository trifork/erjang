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

-export([run/1, run/2, list/1]).

%% @doc List tests in module Mod
%% @spec list( atom() ) -> [ atom() ]
list(Mod) when is_atom(Mod) ->
    {module, ModSuite} = compile_and_load(Mod),
    
    lists:map(fun(Test) -> get_list(ModSuite,Test) end,
	      ModSuite:all(suite)).

%% @doc Run all tests in module Mod (i.e. `Mod ++ "_SUITE"' ).
run(Mod) when is_atom(Mod) ->
    run(Mod,all).

%% @doc Run test `What' in module Mod (i.e. `Mod ++ "_SUITE"' ).
run(Mod, What) when is_atom(Mod) ->
    {module, ModSuite} = compile_and_load(Mod),
    run (ModSuite, [], [What], []).


run(_ModSuite,_Sub,[], _Config) ->
    done;

run(ModSuite,Sub,[Test|Rest], Config) ->
    case catch ModSuite:Test(suite) of
	{'EXIT', _} ->
	    run_single_test(ModSuite,Test,Config),
	    run(ModSuite,Sub,Rest, Config);
	    
	[] ->
	    run_single_test(ModSuite,Test,Config),
	    run(ModSuite,Sub,Rest, Config);
	
	L when is_list(L) ->
	    io:format("Running test category ~p~n", 
		      [lists:reverse([Test|Sub])]),
	    run(ModSuite,[Test|Sub],L,Config),
	    run(ModSuite,Sub,Rest,Config)
    end.
    
    
run_single_test(ModSuite,Test,Config) ->
    
    TestName = [ModSuite,Test],
    io:format("~n~n*** Running ~p~n", [TestName]),

    Config1 = [{test_case, Test} | Config],

    Owner = self(),
    {PID,Ref} = spawn_monitor
		  (fun() ->
			   erlang:group_leader(Owner, self()),
			   put('$ts$owner', Owner),
			   Result = do_run_single_test(ModSuite,Test,Config1),
			   Owner ! {ok, Result}
		   end),


    case test_control_loop(PID, Ref) of
	{fail, Info} -> 
	    erlang:display(Info),
	    io:format("~n*** ~p: Failed with: ~n", [TestName]),
	    ok;

	ok -> 
	    io:format("~n*** ~p: Succeeded~n", [TestName]),
	    ok
	
    end.


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
	    erlang:demonitor(Ref, [flush]),
	    ok
    end.



%%
%% This is the action that runs in a separate process
%%
do_run_single_test(ModSuite,Test,Config1) ->

    %% initialize test
    Config2 = ModSuite:init_per_testcase([ModSuite,Test], Config1),
    
    %% run test
    try
	ModSuite:Test(Config2)
    after
	catch ModSuite:fin_per_testcase ([ModSuite,Test], Config2)
    end.


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


get_list(Mod,Test) ->
    io:format("getting ~s:~s~n",[Mod,Test]),
    case catch Mod:Test(suite) of
	{'EXIT', _} -> Test;

	[] -> Test;
	      
	L when is_list(L) ->
	    lists:map(fun(Test2) -> get_list(Mod,Test2) end, L)
    end.



