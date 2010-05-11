-module(test_server).


-export([timetrap/1, timetrap_cancel/1, 
	 lookup_config/2,
	 minutes/1, seconds/1, 
	 run/1, run/2, list/1]).


minutes(Minutes) ->
    seconds (Minutes * 60).

seconds(Seconds) ->
    Seconds * 1000.

timetrap(Millis) ->
    Self = self(),
    spawn(fun() -> timetrapper(Millis, Self) end).

timetrap_cancel(Ref) ->
    Ref ! {cancel, self()}.

timetrapper(Millis,Owner) ->
    receive 
	{cancel, Owner} ->
	    done
    after Millis ->
	    erlang:exit(Owner, kill),
	    io:format("Killed ~p~n", [Owner])
    end.


compile_and_load(Mod) ->
    ModList = atom_to_list(Mod) ++ "_SUITE",
    ModSuite = list_to_atom(ModList),
    case os:find_executable("erlc") of
	false -> 
	    io:format("cannot find erlc~n"),
	    exit(bad);
    
	ErlC -> 
	    Cmd = ErlC ++ " -I . " ++ ModList ++ ".erl",
	    io:format("Running :~p~n", [Cmd]),
	    os:cmd(Cmd)
    end,
    code:purge(ModSuite),
    code:load_file(ModSuite).


get_list(Mod,Test) ->
    case catch Mod:Test(suite) of
	{'EXIT', _} -> Test;

	[] -> Test;
	      
	L when is_list(L) ->
	    lists:map(fun(Test2) -> get_list(Mod,Test2) end, L)
    end.



list(Mod) when is_atom(Mod) ->
    {module, ModSuite} = compile_and_load(Mod),
    
    lists:map(fun(Test) -> get_list(Mod,Test) end,
	      ModSuite:all(suite)).

run(Mod) when is_atom(Mod) ->
    run(Mod,all).

run(Mod, What) when is_atom(Mod) ->
    {module, ModSuite} = compile_and_load(Mod),
    run (ModSuite, [], [What], []).

run(Mod,Sub,[], _Config) ->
    io:format("done with ~p~n", [[Mod|Sub]]);

run(Mod,Sub,[Test|Rest], Config) ->
    case catch Mod:Test(suite) of
	{'EXIT', _} ->
	    run_single_test(Mod,Test,Config),
	    run(Mod,Sub,Rest, Config);
	    
	[] ->
	    run_single_test(Mod,Test,Config),
	    run(Mod,Sub,Rest, Config);
	
	L when is_list(L) ->
	    run(Mod,[Test|Sub],L,Config),
	    run(Mod,Sub,Rest,Config)
    end.
    
    
run_single_test(Mod,Test,Config) ->
    

    io:format("~n~n*** Running ~p~n", [[Mod,Test]]),

    Config1 = [{test_case, Test} | Config],

    %% initialize test
    Config2 = Mod:init_per_testcase([Mod,Test], Config1),
    
    %% run test
    try

	Mod:Test(Config2),
	io:format("~n*** OK ~p~n", [[Mod,Test]])

    
    catch 
	Class:Reason ->
	    Failure = {'EXIT', Class, Reason, erlang:get_stacktrace()},
	    io:format("~n*** FAILED ~p ~p~n", [[Mod,Test], Failure]),
	    Failure

    after
	catch Mod:fin_per_testcase ([Mod,Test], Config2)

    end.


lookup_config(Key,Config) when is_atom(Key) ->
    proplists:lookup(Key,Config).
