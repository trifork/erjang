-module(bs_skip).

-compile(export_all).

main() ->
    io:format("~p\n", [test()]).

test() ->
    {try skip(1,atom) catch _:Err -> Err end,
     skip(40, <<0,123,45,67,89,0,1,2,3,4>>)
    }.

skip(W, X) ->
    <<      A, _/binary>> = X,
    <<_,    B, _/binary>> = X,
    <<_:8,  C, _/binary>> = X,
    <<_:16, D, _/binary>> = X,
    <<_:24, E, _/binary>> = X,
    <<_:32, F, _/binary>> = X,
    <<_:W,  G, _/binary>> = X,
    {A,B,C,D,E,F,G}.
