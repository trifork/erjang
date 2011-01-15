-module(funref_tests).

-export([test/0,
	exported/1,
	local/1]).

test() ->
    {catch((fun(X) -> {anonymous,X} end) (123)),
     catch((fun local/1) (123)),
     catch((fun ?MODULE:exported/1) (123)),
     catch((fun erlang:is_number/1) (123))}.


local(X) -> {local, X}.
exported(X) -> {exported, X}.
