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
		"aa", "ab", "ba", "abc", "abc"++[a], ["abc"|a], % EString
		[97.0, 98.0], % Nearly "ab"
		unicode:characters_to_list(<<"\xC2\xA2">>, utf8), % EBigString
		unicode:characters_to_list(<<"ab">>, utf8),

		%% Binaries:
		<<>>,
		<<"aa">>, <<"ab">>, <<"abc">>, <<"\xC2\xA2">>,
		<<"ab", 1:1>>, <<"ab", 0:2>>, <<"ab", 1:2>>, <<"ab", 2:2>>,

		%% Misc.:
		self() % EPid.
		%% TODO: Ports?
	       ],

    {[[{scrub(A),scrub(B), catch(A==B), catch(A=:=B), catch(A<B)}
      || A <- Operands]
      || B <- Operands],
     [{scrub(A),
       catch(A==copy(A)), catch(copy(A)==A),
       catch(A=:=copy(A)), catch(copy(A)=:=A),
       catch(A<copy(A)), catch(copy(A)<A)}
      || A <- Operands]}.

copy(X) when is_atom(X) -> X;
copy(X) when is_number(X) -> X + 0;
copy([X|Y]) -> [copy(X) | copy(Y)];
copy(X) when is_tuple(X) ->
    list_to_tuple(copy(tuple_to_list(X)));
copy(<<X, Rest/bitstring>>) -> <<X, (copy(Rest))/bitstring>>;
copy(X) -> X.

scrub(X) when is_pid(X) -> some_pid;
scrub(X) -> X.
