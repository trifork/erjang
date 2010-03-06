-module(equals_tests).

-export([test/0]).

test() ->
    %% Test equals on small integers, large integers, bigints, and non-numbers.

    Operands = [0, 0.0,
		123, 65536, 123456789, 123456789123456789123456789, 1.0e299,
		-123, -65536, -123456789, -123456789123456789123456789, -1.0e299,
		%% Atoms:
		atom, list_to_atom("another_atom"), list_to_existing_atom("atom"),
		%% Tuples:
		{}, {a}, {1,2}, {2,1},

		%% Lists:
		[], [a], [a|b],
		"ab", "abc", "abc"++[a], ["abc"|a], % EString
		unicode:characters_to_list(<<"\xC2\xA2">>, utf8), % EBigString

		%% Misc.:
		self() % EPid
		%% TODO: Ports?
	       ],

    {[{catch(A==B), catch(A=:=B), catch(A<B)}
	     || A <- Operands,
		B <- Operands],
     [{catch(A==copy(A)), catch(copy(A)==A),
       catch(A=:=copy(A)), catch(copy(A)=:=A),
       catch(A<copy(A)), catch(copy(A)<A)}
      || A <- Operands]}.

copy(X) when is_atom(X) -> X;
copy(X) when is_number(X) -> X + 0;
copy([X|Y]) -> [copy(X) | copy(Y)];
copy(X) when is_tuple(X) ->
    list_to_tuple(lists:map(fun copy/1, tuple_to_list(X)));
copy(<<X, Rest/bitstring>>) -> <<X, (copy(Rest))/bitstring>>;
copy(X) -> X.
