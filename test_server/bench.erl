-module(bench).

-export([main/0]).

main() ->
    berk:run(fun() -> ts:run(estone) end, 5).
