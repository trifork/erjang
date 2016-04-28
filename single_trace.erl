
-module(single_trace).
-export([trace/2,trace/4]).

trace(M,F) ->    trace(M,F,'_',[]).

trace(M,F,A,MS) ->
    DisableTrace = {'_',[],[{disable_trace,[call]}]},
    erlang:trace_pattern({M,F,A}, [ DisableTrace | MS], []),
    Tracer = spawn(fun trace_loop/0),
    erlang:trace(all, true, [{tracer, Tracer}, call]).

trace_loop() ->
    receive
        Msg ->
            io:format("~p~n",[Msg]),
            erlang:trace(all, false, [call]),
            trace_loop()
    end.

