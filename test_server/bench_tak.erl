-module(bench_tak).

-export([times/2, main/0, main/1]).

tak(X,Y,Z) when is_integer(X), is_integer(Y), is_integer(Z), Y >= X ->
    Z;
tak(X,Y,Z) when is_integer(X), is_integer(Y), is_integer(Z) ->
    tak( tak(X-1, Y, Z),
	 tak(Y-1, Z, X),
	 tak(Z-1, X, Y) ).


main(N) -> 
    Body = fun() ->
		   Before = erlang:now(),
		   times(10, fun() -> Val = tak(24,16,8) end),
		   After = erlang:now(),
		   io:format("~n"),
		   Diff = timer:now_diff(After, Before),
		   io:format("run: ~pms~n", [Diff div 1000])
    end,
			 
    timer:tc(?MODULE, times, [N, Body]).
    

times(0, _) -> ok;   
times(N, Fun) ->
    io:format("~p:", [N]),
    Fun(),    
    times(N-1, Fun).

main() ->
    case init:get_argument(loops) of
	{ok, [N]} -> main(list_to_integer(N));
	error -> main(1)
    end.
