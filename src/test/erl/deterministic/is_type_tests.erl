-module(is_type_tests).

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
		 {atom,      fun is_atom/1},
		 {binary,    fun is_binary/1},
		 {bitstring, fun is_bitstring/1},
		 {boolean,   fun is_boolean/1},
		 {float,     fun is_float/1},
		 {function,  fun is_function/1},
		 {function0, fun (X) -> is_function(X, 0) end},
		 {integer,   fun is_integer/1},
		 {list,      fun is_list/1},
		 {number,    fun is_number/1},
		 {pid,       fun is_pid/1},
		 {port,      fun is_port/1},
		 {reference, fun is_reference/1},
		 {tuple,     fun is_tuple/1}
		],
    [{TN, [TT(V) || V<-Values]} || {TN,TT}<-TypeTests].
