-module(utf_tests).
-compile(export_all).

test() ->
    Spec = [
	    <<>>,
	    ascii, <<0>>, <<"abc">>, <<127>>,
	    two_bytes, <<"\xCC\xA2">>,
	    three_bytes, <<"\xE0\xA2\x90">>,
	    four_bytes, <<"\xF0\xA2\x90\x85">>,

	    overlong_boundaries,
	    <<"\x7F">>, <<"\xC1\xBF">>,	<<"\xC2\x80">>,	% At 127
	    <<"\xDF\xBF">>, <<"\xE0\x9F\xBF">>, <<"\xE0\xA0\x80">>, % At 2047
	    <<"\xEF\xBF\xBD">>, <<"\xF0\x8F\xBF\xBD">>, <<"\xF0\x90\x80\x80">>,

	    invalid_range_boundaries,
	    <<"\xED\x9F\xBF">>, <<"\xED\xA0\x80">>, % D800-
	    <<"\xED\xBF\xBF">>, <<"\xEE\x80\x80">>, % -DFFF
	    <<"\xEF\xBF\xBD">>, <<"\xEF\xBF\xBE">>, % FFFE-
	    <<"\xEF\xBF\xBF">>, <<"\xF0\x90\x80\x80">>, % -FFFF
	    <<"\xF4\x8F\xBF\xBF">>, <<"\xF4\x90\x80\x80">>, % 110000-...

	    truncated, <<"\xC2">>, <<"\xE0\xA0">>, <<"\xF0\x90\x80">>,
	    invalid_first, <<"\x80">>,
	    invalid_continuation, <<"\xC2\xC0">>, <<"\xE0\xA0\x00">>
	   ],
    [decode_utf8(X) || X<-Spec].


decode_utf8(X) when is_atom(X) -> X;
decode_utf8(<<>>) -> [];
decode_utf8(<<C/utf8, Rest/binary>>) -> [C | decode_utf8(Rest)];
decode_utf8(<<B, Rest/binary>>) -> [{byte,B} | decode_utf8(Rest)].


main() ->
    io:format("~p\n", [test()]).
