%%
%% @author Pavlo Baron <pb@pbit.org>
%% @doc test cases for the Erjang binary module
%%

-module(binary_tests).

-include("triq.hrl").

-export([test/0]).

test() ->
    {triq:check(prop_bin_list())}.

prop_bin_list() ->
    ?FORALL(B, oneof([<<1, 2, 3, 4, 5>>, <<11, 22, 33, 44, 55>>, <<0, 100, 200, 255>>]),
                binary:list_to_bin(binary:bin_to_list(B)) == B).        