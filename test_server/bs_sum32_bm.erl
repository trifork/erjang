-module(bs_sum32_bm).
-export([benchmarks/0,lists/1,bs/1,bs_unrolled/1]).

benchmarks() ->
    {50000,[lists,bs,bs_unrolled]}.


lists(Iter) ->
    lists(Iter, data()).

lists(0, Data) -> ok;
lists(Iter, Data) ->
    checksum_list(Data, 0),
    lists(Iter-1, Data).

checksum_list([], Sum) -> Sum;
checksum_list([H], Sum) ->
    checksum_list([H,0], Sum);
checksum_list([H1,H2,H3,H4|T], Sum) ->
    checksum_list(T, Sum+((H1 bsl 24) + (H2 bsl 16) + (H3 bsl 8) + H2)).

bs(Iter) ->
    bs(Iter, list_to_binary(data())).

bs(0, Data) -> ok;
bs(Iter, Data) ->
    checksum_bs(Data, 0),
    bs(Iter-1, Data).

checksum_bs(<<>>, Sum) -> Sum;
checksum_bs(<<N:32,T/binary>>, Sum) ->
    checksum_bs(T, Sum + N);
checksum_bs(<<N:8>>, Sum) ->
    Sum + (N bsl 8).

bs_unrolled(Iter) ->
    bs_unrolled(Iter, list_to_binary(data())).

bs_unrolled(0, Data) -> ok;
bs_unrolled(Iter, Data) ->
    checksum_bs_unrolled(Data, 0),
    bs_unrolled(Iter-1, Data).

checksum_bs_unrolled(<<>>, Sum) -> Sum;
checksum_bs_unrolled(<<N1:32,N2:32,N3:32,N4:32,T/binary>>, Sum) ->
    checksum_bs_unrolled(T, Sum + N1 + N2 + N3 + N4);
checksum_bs_unrolled(<<N:32,T/binary>>, Sum) ->
    checksum_bs_unrolled(T, Sum + N);
checksum_bs_unrolled(<<N:8>>, Sum) ->
    Sum + (N bsl 8).

data() -> lists:seq(0, 255).
