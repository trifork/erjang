-module(atom_conversion_tests).

-export([test/0]).

test() ->
    L0 = ['', a, abc_xyz, ?MODULE, 'A-Za-z0-9', '\'', '$$', '0123456789', '!"#%&/()=^()[]{}'],
    R1 = [atom_to_list(X) || X <- L0],
    R2 = [list_to_atom(X) || X <- R1],
    R3 = [list_to_existing_atom(X) || X <- R1],
    Chk2 = {L0 == R2, L0 =:= R2}, % Should be 2 x true
    Chk3 = {L0 == R3, L0 =:= R3}, % Should be 2 x true
    R4 = list_to_atom(lists_seq(0,127)),
    {R1, R2, R3, Chk2, Chk3, R4}.

lists_seq(A,B) ->
    if A>B -> [];
       true -> [A | lists_seq(A+1,B)]
    end.
