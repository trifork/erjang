-module(is_type_guard_tests).

-export([test/0]).

test() ->
    %% Try to test both literals and derived/constructed values:
    L = "Hello World",
    B = <<"Binary data">>,
%%     Bi = <<3:2>>, % TODO: Enable
    T = {a,b,c},
    ET1 = (catch(throw(x))),
    ET2 = (catch(erlang:error(x))),
    Values = [
	      %% Integers:
	      123, -123, 123456789012456789, -123456789012456789,
	      1+2, 1-2, 1*2, 1 div 2, 1 rem 2,
	      123456789*123456789,
	      %% Floats:
	      1.0e200, -2.0e200,
	      1.0+2.0, 1.0-2.0, 1.0*2.0, 1.0/2.0,
	      %% Atoms:
	      atom, list_to_atom("testing"),
	      true, false,
	      %% Lists:
	      [], [a], [a|b], L, tl(L), atom_to_list(atom),
	      %% Tuples:
	      {}, {a}, T, setelement(2,{a,b,c},100), ET1, ET2,
	      %% Binaries and bit strings:
	      <<>>, B, element(1,split_binary(B,5)),
%% 	      Bi,
	      <<B/binary, B/binary>>,
	      %% TODO: construct a bitstring.
	      %% Pids and references:
	      self(), make_ref(),
	      %% Functions:
	      fun()->ok end,
	      fun test/0
%% 	      catch(fun ?MODULE:test/0), % TODO: enable.
%% 	      catch(fun erlang:is_pid/1) % TODO: enable.
	     ],

    TypeTests = [
		 {atom,	      fun(V) when is_atom(V) -> yes; (_) -> no end},
		 {binary,     fun(V) when is_binary(V) -> yes; (_) -> no end},
		 {bitstring,  fun(V) when is_bitstring(V) -> yes; (_) -> no end},
		 {boolean,    fun(V) when is_boolean(V) -> yes; (_) -> no end},
		 {float,      fun(V) when is_float(V) -> yes; (_) -> no end},
		 {function,   fun(V) when is_function(V) -> yes; (_) -> no end},
 		 {function0,  fun (V) when is_function(V, 0) -> yes; (_) -> no end},
		 {integer,    fun(V) when is_integer(V) -> yes; (_) -> no end},
		 {list,	      fun(V) when is_list(V) -> yes; (_) -> no end},
		 {number,     fun(V) when is_number(V) -> yes; (_) -> no end},
		 {pid,	      fun(V) when is_pid(V) -> yes; (_) -> no end},
		 {port,	      fun(V) when is_port(V) -> yes; (_) -> no end},
		 {reference,  fun(V) when is_reference(V) -> yes; (_) -> no end},
		 {tuple,      fun(V) when is_tuple(V) -> yes; (_) -> no end}
		],
    [{TN, [TT(V) || V<-Values]} || {TN,TT}<-TypeTests].
