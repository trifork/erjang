-module(run_wrapper).

-export([run/1,
	 test_process/2]).

run([Platform, Module, Timeout]) ->
    run(Platform, Module, list_to_integer(atom_to_list(Timeout)));
run([Platform, Module]) ->
    run(Platform, Module, 1).

run(Platform, Module, TimeoutSecs) ->
    TimeoutMillis = TimeoutSecs * 1000,
    Self = self(),
    Module:module_info(), % ensure module is loaded
    spawn(?MODULE, test_process, [Module, Self]),
    Result = receive R -> R after TimeoutMillis -> timeout end,
    report(Platform, Result).

test_process(Module, Recvr) ->
    Result = (catch {run_result, Module:test()}),
    Result2 = strip(Result),
    Recvr ! Result2.

report(_, Result) ->
    %% we need to prefix the data part since stdout gets poluted with io:format from different libs,
    %% such as Triq. We can never exclude that any other lib would do this. Since we use only one stream
    %% for both, result data _and_ io:format output (even stdout), we cannot redirect stdout to /dev/null or such
    %% So we simply separate the data part knowing stdout part always comes first and parse it out later.
    %% If we ever have a problem with a wild stdout flying in, we still can redirect using for example
    %% group_leader(/dev/null, self()) or such
    io:format("~s~s", ["DATA::", term_to_binary(Result)]).




strip({'EXIT', {EX, [{M,F,A} |_Rest]}}) ->
    {'EXIT', {EX, [{M,F,A}]}};

strip([H|T]) ->
    [strip(H) | strip(T)];

strip(TUP) when is_tuple(TUP) ->
    list_to_tuple( strip ( tuple_to_list (TUP) ) );

strip(N) -> N.


