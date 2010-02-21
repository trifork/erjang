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
	    [64], [129], [<<"a">>],
%	    [<<"\xA2">>], DISABLED pending handling of characters spanning binary boundary

	    %% Strings:
	    ["foo"], [98, 97, 122], [98, 230, 114], <<"quux">>,
%	    <<"b\xE6r">>, DISABLED pending handling of characters spanning binary boundary

	    %% Deep:
	    [64], [[129]], [[<<"a">>]],
%	    [[<<"\xA2">>]], DISABLED pending handling of characters spanning binary boundary
%% 	    [[["nested"]]],
	    [["An", [<<"undecodable\x80">>, "with"], $S, "ubsequent"], <<"error">>],

	    %% Cons:
	    ["a","bc","def"], [$a, <<"mixed">>, "list"],

	    %% Tails:
	    ["xyz" | $a],
	    ["xyz" | "a"],
	    ["xyz" | <<"www">>],

	    %% Split UTF8:
%% 	    [[<<"\xc8">>], [<<"\xa2">>]] % DISABLED pending handling of characters spanning binary boundary

	    %% Bad:
	    123, atom,
	    [atom]
%	    [123|atom] % DISABLED pending handling of emit_const(EBinList)
	   ],
    [
     {i2b,        [?Catch(erlang:iolist_to_binary(X)) || X<-Spec]},
     {c2b_latin1, [?Catch(unicode:characters_to_binary(X, latin1)) || X<-Spec]},
     {c2b_utf8,   [?Catch(unicode:characters_to_binary(X, utf8)) || X<-Spec]}
%%      {c2l_latin1, [?Catch(unicode:characters_to_list(X, latin1)) || X<-Spec]},
%%      {c2l_utf8, [?Catch(unicode:characters_to_list(X, utf8)) || X<-Spec]}
    ].

cst({'EXIT', {Reason, [Top|_]}}) ->
    {'EXIT', {Reason, [Top]}};
cst(X) -> X.
