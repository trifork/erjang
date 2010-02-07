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

report(erjang, Result) ->
    throw(Result);
report(erlang, Result) ->
    io:format("~s", [term_to_binary(Result)]).
