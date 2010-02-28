-module(list_diff_tests).

-export([test/0, main/1]).

test() ->
    Operands = [[],
		[a], [b], [c],
		[a,a], [b,b], [c,c],
		[a,b,c], [c,b,a], [c,a,b], [b,c,a],
		[z,y,x,w,a,b,c,m,n,o,q],
		[a,n,z], [b,o,y], [x,z,q],
		123, 123456789123456789123456789,
		atom, [a|b]],

    [try A--B catch _:Err->{error,Err} end
     || A <- Operands,
	B <- Operands].

main([]) ->
    io:format("~p\n", [test()]).
