-module(iolist_tests).

-compile(export_all).

-define(Catch(X), cst(catch(X))).

main() ->
    io:format("~p\n", [test()]).

test() ->
    Spec = [
	    % Empty:
	    [], [[]], [<<>>],

	    %% Single:
	    [64], [129], [<<"a">>], [<<"\xA2">>],

	    %% Strings:
	    ["foo"], [98, 97, 122], [98, 230, 114], <<"quux">>, <<"b\xE6r">>,

	    %% Deep:
	    [64], [[129]], [[<<"a">>]], [[<<"\xA2">>]],
	    [[["nested"]]],

	    %% Cons:
	    ["a","bc","def"], [$a, <<"mixed">>, "list"],

	    %% Tails:
	    ["xyz" | $a],
	    ["xyz" | "a"],
	    ["xyz" | <<"www">>],

	    %% Bad:
	    123, atom,
	    [atom]
%	    [123|atom] % DISABLED because of emit_const(EBinList) shortcoming. (TODO)
	   ],
    [{i2b,        [?Catch(erlang:iolist_to_binary(X)) || X<-Spec]},
     {c2b_latin1, [?Catch(unicode:characters_to_binary(X, latin1)) || X<-Spec]},
     {c2b_utf8,   [?Catch(unicode:characters_to_binary(X, utf8)) || X<-Spec]}
%%      {c2l_latin1, [?Catch(unicode:characters_to_list(X, latin1)) || X<-Spec]},
%%      {c2l_utf8, [?Catch(unicode:characters_to_list(X, utf8)) || X<-Spec]}
    ].

cst({'EXIT', {Reason, ST=[Top|_]}}) ->
    {'EXIT', {Reason, [Top]}};
cst(X) -> X.
