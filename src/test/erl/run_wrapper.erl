-module(run_wrapper).

-export([test/0,
	 run_wrapper/1]).

test() -> ok.

run_wrapper([Platform, Module]) ->
    Result = (catch {run_result, Module:test()}),
    report(Platform, Result).

report(erjang, Result) ->
    throw(Result);
report(erlang, Result) ->
    io:format("~w", [Result]).
