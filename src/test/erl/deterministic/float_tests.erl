-module(float_tests).

-export([test/0]).

test() ->
    %% Test float() on small integers, large integers, bigints, and non-numbers.
    {float(0), float(0.0),
     float(123), float(65536), float(123456789), float(123456789123456789123456789), float(1.0e299),
     float(-123), float(-65536), float(-123456789), float(-123456789123456789123456789), float(-1.0e299),
     catch(float([])), catch(float(atom)), catch(float("abcd")), catch(float([a|b]))
    }.
