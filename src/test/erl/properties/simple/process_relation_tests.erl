-module(process_relation_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("triq.hrl").

% ../../../../../jerl -sname erj -pz ../../../../../triq/ebin
% c(process_relation_tests, [{i, "../../../../../triq/include"}]).
% process_relation_tests:test().

child_survives_tail_spawn_link_normal_exit_test() ->
    N = 1000,
    expect_to_leave_N_processes(N,
                                fun() ->
                                        [tail_spawn_link_permanent_child_normal_exit()
                                         || _ <- lists:seq(1,N)]
                                end).

child_survives_nontail_spawn_link_normal_exit_test() ->
    N = 1000,
    expect_to_leave_N_processes(2*N,
                                fun() ->
                                        [nontail_spawn_link_permanent_child_normal_exit()
                                         || _ <- lists:seq(1,N)]
                                end).

%%% Tests link from parent to child.
nontail_spawn_link_parent_exits_abnormally_test() ->
    N = 1000,
    expect_to_leave_N_processes(0,
                                fun() ->
                                        [nontail_spawn_link_parent_exits_abnormally()
                                         || _ <- lists:seq(1,N)]
                                end).

%%% Tests link from child to parent.
nontail_spawn_link_child_exits_abnormally_test() ->
    N = 1000,
    expect_to_leave_N_processes(0,
                                fun() ->
                                        [nontail_spawn_link_child_exits_abnormally()
                                         || _ <- lists:seq(1,N)]
                                end).

%%% Tests propgation.
nontail_spawn_link_child_with_siblings_exits_abnormally_test() ->
    N = 1000,
    expect_to_leave_N_processes(0,
                                fun() ->
                                        [nontail_spawn_link_child_with_siblings_exits_abnormally()
                                         || _ <- lists:seq(1,N)]
                                end).


tail_spawn_link_permanent_child_normal_exit() ->
    spawn(fun() ->
                  spawn_link(fun() ->
                                     receive never -> ok end
                             end) % In tail position
          end).

nontail_spawn_link_permanent_child_normal_exit() ->
    spawn(fun() ->
                  spawn_link(fun() ->
                                     receive never -> ok end
                             end),
                  timer:sleep(1),
                  spawn_link(fun() ->
                                     receive never -> ok end
                             end),
                  io:format("") % Ensure nontail position
          end).

nontail_spawn_link_parent_exits_abnormally() ->
    spawn(fun() ->
                  spawn_link(fun() ->
                                     receive never -> ok end
                             end),
                  timer:sleep(1),
                  spawn_link(fun() ->
                                     receive never -> ok end
                             end),
                  exit(ab)
          end).

nontail_spawn_link_child_exits_abnormally() ->
    spawn(fun() ->
                  %% Abnormally exiting:
                  spawn_link(fun() ->
                                     exit(ab),
                                     receive never -> ok end
                             end),
                  io:format("") % Ensure nontail position
          end).

nontail_spawn_link_child_with_siblings_exits_abnormally() ->
    spawn(fun() ->
                  %% Earlier sibling 1:
                  spawn_link(fun() ->
                                     receive never -> ok end
                             end),
                  timer:sleep(1),
                  %% Earlier sibling 2:
                  spawn_link(fun() ->
                                     receive never -> ok end
                             end),
                  %% Abnormally exiting:
                  spawn_link(fun() ->
                                     exit(ab),
                                     receive never -> ok end
                             end),
                  %% Subsequent sibling 1:
                  spawn_link(fun() ->
                                     receive never -> ok end
                             end),
                  timer:sleep(1),
                  %% Subsequent sibling 2:
                  spawn_link(fun() ->
                                     receive never -> ok end
                             end)
          end).

expect_to_leave_N_processes(N, Action) when is_integer(N),
                                            is_function(Action,0) ->
    Before = processes(),
    Action(),
    timer:sleep(1000),
    After = processes(),
    ?assertEqual(N, length(After -- Before)).


test() ->
    eunit:test(?MODULE).
