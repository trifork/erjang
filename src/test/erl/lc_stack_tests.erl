-module(lc_stack_tests).

-export([test/0]).

test() ->
    %% List comprehension stack trace test.
    [begin catch(throw(x)), erlang:get_stacktrace() end
     || X <- [a,b,c]].
