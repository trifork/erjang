-module(arith_tests).

-export([test/0]).

-define(Catch(Exp), (try Exp catch _:Err -> {error,Err} end)).

test() ->
    %% Test arithmetics() on small integers, large integers, bigints, and non-numbers.
    Operands = [0, 0.0,
		123, 65536, 123456789,
 		123456789123456789123456789, 1.0e149,
 		-123, -65536, -123456789,
 		-123456789123456789123456789, -1.0e149,
%%		1.0e299, -1.0e299, % DISABLED pending handling of overflow.
  		atom, [], "abc", [a|b]],

    [if is_number(A), is_number(B) -> % Pending proper handling of non-numbers
	     {{binop, A, B},
	      ?Catch(A+B), ?Catch(A-B), ?Catch(A*B),
	      ?Catch(A/B),
	      ?Catch(A div B), ?Catch(A rem B),
	      ?Catch(A band B), ?Catch(A bor B), ?Catch(A bxor B),
	      if B >  200->skipped; true -> ?Catch(A bsl B) end,
	      if B < -200->skipped; true -> ?Catch(A bsr B) end};
	true -> {binop, A, B, skipped}
     end
     || A <- Operands,
	B <- Operands]
	++
	[if is_number(A) -> % Pending proper handling of non-numbers
		 {{unop, A},
		  ?Catch(-A),
		  ?Catch(bnot A)};
	    true -> {unop, A, skipped}
	 end
	 || A <- Operands].
