-module(sizes_tests).

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
		self() % EPid
		%% TODO: Ports?
	       ],

    [{scrub(A),
      try length(A)     catch Cls:Err -> {Cls,Err} end,
      try tuple_size(A) catch Cls:Err -> {Cls,Err} end,
      try byte_size(A)  catch Cls:Err -> {Cls,Err} end,
      try bit_size(A)   catch Cls:Err -> {Cls,Err} end}
     || A <- Operands].

scrub(X) when is_pid(X) -> some_pid;
scrub(X) -> X.
