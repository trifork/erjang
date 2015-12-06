-module(process_relation_tests).

-compile(export_all).

-include("triq.hrl").
-include("unit.hrl").

% ../../../../../jerl -sname erj -pz ../../../../../triq/ebin
% c(process_relation_tests, [{i, "../../../../../triq/include"}]).
% process_relation_tests:test().


%%%========== Entry point: ====================
test() ->
    eunit_test(?MODULE).

%%%========== Tests: ==============================

child_survives_tail_spawn_nonlink_test() ->
    N = 1000,
    expect_to_leave_N_processes(N,
                                fun() ->
                                        [tail_nonlink_permanent_child()
                                         || _ <- lists:seq(1,N)]
                                end).

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

%%% Tests propagation.
nontail_spawn_link_child_with_siblings_exits_abnormally_test() ->
    N = 1000,
    expect_to_leave_N_processes(0,
                                fun() ->
                                        [nontail_spawn_link_child_with_siblings_exits_abnormally()
                                         || _ <- lists:seq(1,N)]
                                end,
                               2000).

%%%----------
%%% Link storm: Many simultaneous linkings against the same process
link_storm_normal_exit_test() ->
    N=100, M=100,
    expect_to_leave_N_processes(N*M,
                                fun() ->
                                        [link_storm_normal_exit(M)
                                         || _ <- lists:seq(1,N)]
                                end).
link_storm_normal_exit(M) ->
    Linkee = spawn(fun() -> receive wait_then_exit ->
                                    timer:sleep(500),
                                    exit(normal)
                            end
                   end),
    _Spawned =
        lists:map(fun(_) ->
                          spawn(fun() -> link(Linkee),
                                         receive never -> ok after 3000->done end
                                end)
                  end,
                  lists:seq(1,M)),
    Linkee ! wait_then_exit.

%%%----------
%%% Link storm: Many simultaneous linkings against the same process
link_storm_abnormal_exit_test() ->
    N = 1000, M=100,
    expect_to_leave_N_processes(0,
                                fun() ->
                                        [link_storm_abnormal_exit(M)
                                         || _ <- lists:seq(1,N)]
                                end).

link_storm_abnormal_exit(M) ->
    Linkee = spawn(fun() -> receive never -> ok end end),
    _Spawned =
        lists:map(fun(_) ->
                          spawn(fun() -> link(Linkee),
                                         receive never -> ok after 5->done end
                                end)
                  end,
                  lists:seq(1,M)),
    spawn(fun() -> timer:sleep(500),
                   exit(Linkee,crash)
          end).
%%%----------

%%% Tests propagation + possibility of having been sent exit when trying to link
link_kill_race_test() ->
    N = 10000,
    expect_to_leave_N_processes(0,
                                fun() ->
                                        [link_kill_race()
                                         || _ <- lists:seq(1,N)]
                                end).

tail_nonlink_permanent_child() ->
    spawn(fun() ->
                  spawn_link(fun() ->
                               receive never -> ok end
                       end) % In tail position
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
                                     receive never -> ok
                                     after 2000 -> io:format("S1") end
                             end),
                  timer:sleep(1),
                  %% Earlier sibling 2:
                  spawn_link(fun() ->
                                     receive never -> ok
                                     after 2000 -> io:format("S2") end
                             end),
                  %% Abnormally exiting:
                  spawn_link(fun() ->
                                     exit(ab),
                                     io:format("S3")
                             end),
                  %% Subsequent sibling 1:
                  spawn_link(fun() ->
                                     receive never -> ok
                                     after 2000 -> io:format("S4") end
                             end),
                  timer:sleep(1),
                  %% Subsequent sibling 2:
                  spawn_link(fun() ->
                                     receive never -> ok
                                     after 2000 -> io:format("S5") end
                             end),
                  timer:sleep(500)
          end).

link_kill_race() ->
    Pid = spawn(fun() ->
                        io:format("B"),
                        spawn_link(fun() ->
                                           io:format("C"),
                                           receive never -> ok end
                                   end),
                        receive never -> ok end
                end),
    exit(Pid,go_away).

%%%========== Utilities: ====================
expect_to_leave_N_processes(N, Action) ->
    expect_to_leave_N_processes(N, Action,1000).

expect_to_leave_N_processes(N, Action, Timeout)
  when is_integer(N),
       is_function(Action,0),
       is_integer(Timeout) ->
    Before = processes(),
    Action(),
    timer:sleep(Timeout),
    After = processes(),
    ?assertEqual(N, length(After -- Before)).
