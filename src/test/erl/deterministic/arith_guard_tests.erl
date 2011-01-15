-module(arith_guard_tests).

-export([test/0]).

-define(Check(Exp), (if (Exp)>0 -> pos; (Exp)<0 -> neg; (Exp)==0 -> zero; true -> none end)).

test() ->
    %% Test arithmetics() on small integers, large integers, bigints, and non-numbers.
    Operands = [0, 0.0,
		123, 65536, 123456789,
 		123456789123456789123456789, 1.0e149,
 		-123, -65536, -123456789,
 		-123456789123456789123456789, -1.0e149,
%%		1.0e299, -1.0e299, % DISABLED pending handling of overflow.
  		atom, [], "abc", [a|b]],

    [{{binop, A, B},
      ?Check(A+B), ?Check(A-B), ?Check(A*B),
      ?Check(A/B),
      ?Check(A div B), ?Check(A rem B),
      ?Check(A band B), ?Check(A bor B), ?Check(A bxor B),
      if B >  200->skipped; true -> ?Check(A bsl B) end,
      if B < -200->skipped; true -> ?Check(A bsr B) end}
     || A <- Operands,
	B <- Operands]
	++
	[{{unop, A},
	  ?Check(-A),
	  ?Check(bnot A)}
	 || A <- Operands].
