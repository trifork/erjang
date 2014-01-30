-module(ets_race_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("triq.hrl").

% ../../../../../jerl -sname erj -pz ../../../../../triq/ebin
% c(ets_race_tests, [{i, "../../../../../triq/include"}]).
% ets_race_tests:test().


give_away_die_race_test() ->
    N = 10000,
    expect_to_leave_N_tables(0,
                             fun() ->
                                     [give_away_die_race()
                                      || _ <- lists:seq(1,N)]
                             end).

give_away_die_race() ->
    Pid = spawn(fun() -> ok end),
    %Tab = ets:new(foo, [{heir, Pid, here_you_are}]),
    Tab = ets:new(foo, []),
    try ets:give_away(Tab, Pid, here_you_are)
    catch _:badarg -> ets:delete(Tab)
    end.




expect_to_leave_N_tables(N, Action) when is_integer(N),
                                            is_function(Action,0) ->
    Before = ets:all(),
    Action(),
    timer:sleep(500),
    After = ets:all(),
    ?assertEqual(N, length(After -- Before)).


test() ->
    eunit:test(?MODULE).
