-module(equals_tests).

-export([test/0]).

test() ->
    %% Test equals on small integers, large integers, bigints, and non-numbers.

    Operands = [0, 0.0,
		123, 65536, 123456789, 123456789123456789123456789, 1.0e299,
		-123, -65536, -123456789, -123456789123456789123456789, -1.0e299,
                {1,2}, {2,1},
		atom, [], "abc", "ab", [a|b]],

    [{catch(A==B), catch(A=:=B), catch(A<B)}
	     || A <- Operands,
		B <- Operands].
