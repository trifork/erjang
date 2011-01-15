-module(bs_put_integer).

-compile(export_all).

main() ->
    io:format("~p\n", [test()]).

test() ->
    {try put_int(atom) catch _:Err -> Err end,
     put_int(10),
     put_int(-10),
     put_int(1000),
     put_int(-1000),
     put_int(12345678901234567890),
     put_int(-12345678901234567890),

     put_int_other_widths(10),
     put_int_other_widths(-10),
     put_int_other_widths(1000),
     put_int_other_widths(-1000),
     put_int_other_widths(12345678901234567890),

     put_int_other_units(10),
     put_int_other_units(-10),
     put_int_other_units(1000),
     put_int_other_units(-1000),
     put_int_other_units(12345678901234567890),

     put_width(-123, 8),
     put_width(-123, 16),
     put_width(-123, 24),
     put_width(-123, 32),
     put_width(-123, 40),
     put_width(-123, 64),
     put_width(-123, 72),
     put_width(-123, 80),
     put_width(-123, 1024),

     put_width(12345678901234567890, 40),
     put_width(12345678901234567890, 1000),
     put_width(-12345678901234567890, 40),
     put_width(-12345678901234567890, 1000)
    }.

put_int(X) ->
    {<<X, X:8/little, X:16/little, X:32/little, X:64/little>>,
    <<X, X:8/big, X:16/big, X:32/big, X:64/big>>}.

put_int_other_widths(X) ->
    {<<X:24/little, X:80/little, X:120/little>>,
     <<X:24/big, X:80/big, X:120/big>>}.

put_width(X,W) ->
    {<<X:W/little>>, <<X:W/big>>}.

put_int_other_units(X) ->
    {<<X, X:1/little-unit:8, X:2/little-unit:8, X:8/little-unit:16, X:1/little-unit:64>>,
     <<X, X:1/big-unit:8, X:2/big-unit:8, X:8/big-unit:16, X:1/big-unit:64>>}.
