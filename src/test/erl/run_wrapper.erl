-module(run_wrapper).

-export([test/0,
	 run_wrapper/1,
	 test_process/2]).

test() -> ok.

run_wrapper([Platform, Module]) ->
    Self = self(),
    spawn(?MODULE, test_process, [Module, Self]),
    Result = receive R -> R after 1000 -> timeout end,
    report(Platform, Result).

test_process(Module, Recvr) ->
    Result = (catch {run_result, Module:test()}),
    Recvr ! Result.

report(_, Result) ->
    %% we need to prefix the data part since stdout gets poluted with io:format from different libs,
    %% such as Triq. We can never exclude that any other lib would do this. Since we use only one stream
    %% for both, result data _and_ io:format output (even stdout), we cannot redirect stdout to /dev/null or such
    %% So we simply separate the data part knowing stdout part always comes first and parse it out later
    io:format("~s~s", ["DATA::", term_to_binary(Result)]).
