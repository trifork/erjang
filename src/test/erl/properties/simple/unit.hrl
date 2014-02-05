-ifndef(_unit_hrl_included_).
-define(_unit_hrl_included_,yes).

eunit_test(Module) ->
    TestFunctions = [F || {F,0} <- Module:module_info(exports),
                          lists:suffix("_test", atom_to_list(F))],
    io:format("========== Running test suite ~s ==========\n", [Module]),
    Results =
        lists:map(fun(F) ->
                          io:format("---------- Running test ~s ----------\n", [F]),
                          Res = try Module:F(),
                                     true
                                catch _:Err ->
                                        io:format("*** Exception: ~p\n", [Err]),
                                        false
                                end,
                          io:format("     ----- Test ~s ~s -----\n",
                                    [F, case Res of
                                            true -> "SUCCEEDED";
                                            false -> "FAILED"
                                        end]),
                          {F,Res}
                  end,
                  TestFunctions),
    FailedTests = [F || {F,Res} <- Results, not Res],
    io:format("     ===== Test suite ~s: Tests: ~p  Failed: ~p =====\n",
              [Module, length(TestFunctions), length(FailedTests)]),
    case FailedTests of
        [] ->
            ok;
        _ ->
            error({test_suite_failed, Module, FailedTests})
    end.

-define(assertEqual(Expected,Actual),
        (fun(X,X) -> ok;
           (Exp,Act) -> error({assertion_failed,
                               {line,?LINE},
                               {expr, ??Actual},
                               {expected_value, Exp},
                               {actual_value, Act}})
        end)(Expected,Actual)).

-endif.
