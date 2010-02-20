-module(bs_get_integer).

-compile(export_all).

main() ->
    io:format("~p\n", [test()]).

test() ->
    {try get_int(atom) catch _:Err -> Err end,
     get_int(<<0,123,45,67,89,0,1,2,3,4>>),
     get_int_little(<<0,123,45,67,89,0,1,2,3,4>>),
     get_int_big(<<0,123,45,67,89,0,1,2,3,4>>)
%     get_int_unit(<<0,123,45,67,89,0,1,2,3,4>>)
    }.

get_int(X) ->
    <<_, A, _/binary>> = X,
    <<_, B:8, _/binary>> = X,
    <<_, C:16, _/binary>> = X,
    <<_, D:32, _/binary>> = X,
    <<_, E:64, _/binary>> = X,
    {A,B,C,D,E}.

get_int_little(X) ->
    <<_, A/little, _/binary>> = X,
    <<_, B:8/little, _/binary>> = X,
    <<_, C:16/little, _/binary>> = X,
    <<_, D:32/little, _/binary>> = X,
    <<_, E:64/little, _/binary>> = X,
    {A,B,C,D,E}.

get_int_big(X) ->
    <<_, A/big, _/binary>> = X,
    <<_, B:8/big , _/binary>> = X,
    <<_, C:16/big, _/binary>> = X,
    <<_, D:32/big, _/binary>> = X,
    <<_, E:64/big, _/binary>> = X,
    {A,B,C,D,E}.
