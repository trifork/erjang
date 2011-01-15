-module(utf_tests).
-compile(export_all).

test() ->
    {test8(),
     test16(),
     test32()}.

test8() ->
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


test16() ->
    Spec = [
	    <<>>,
	    <<0,0>>, <<0,1>>, <<1,0>>, <<255,0>>, <<0,255>>,
	    two_words,
	    <<16#D812:16, 16#DC34:16>>,
	    <<16#D956:16, 16#DD78:16>>,

	    truncated, <<0>>, <<0,1,2>>,
	    <<16#D800:16>>, <<16#D800:16, 3>>, <<16#D800:16, 16#DC>>,

	    invalid_first, <<16#DC00:16, 16#DC34:16>>,
	    invalid_continuation, <<"\xC2\xC0">>, <<"\xE0\xA0\x00">>
	   ],
    [{decode_utf16_le(X), decode_utf16_be(X)} || X<-Spec].

test32() ->
    Spec = [
	    ascii,
	    <<16#0:32>>, <<16#127:32>>,
	    non_ascii,
	    <<16#1234:32, 16#5678:32>>,

	    invalid_range,
	    <<16#D7FF:32>>, <<16#D800:32>>,
	    <<16#DFFF:32>>, <<16#E000:32>>,
	    <<16#FFFE:32>>, <<16#FFFF:32>>,
	    <<16#10FFFF:32>>, <<16#110000:32>>,

	    truncated, <<0>>, <<0,1>>, <<0,1,2>>
	   ],
    [{decode_utf32_le(X), decode_utf32_be(X)} || X<-Spec].


decode_utf8(X) when is_atom(X) -> X;
decode_utf8(<<>>) -> [];
decode_utf8(<<C/utf8, Rest/binary>>) -> [C | decode_utf8(Rest)];
decode_utf8(<<B, Rest/binary>>) -> [{byte,B} | decode_utf8(Rest)].

decode_utf16_le(X) when is_atom(X) -> X;
decode_utf16_le(<<>>) -> [];
decode_utf16_le(<<C/utf16-little, Rest/binary>>) -> [C | decode_utf16_le(Rest)];
decode_utf16_le(<<B, Rest/binary>>) -> [{byte,B} | decode_utf16_le(Rest)].

decode_utf16_be(X) when is_atom(X) -> X;
decode_utf16_be(<<>>) -> [];
decode_utf16_be(<<C/utf16-big, Rest/binary>>) -> [C | decode_utf16_be(Rest)];
decode_utf16_be(<<B, Rest/binary>>) -> [{byte,B} | decode_utf16_be(Rest)].

decode_utf32_le(X) when is_atom(X) -> X;
decode_utf32_le(<<>>) -> [];
decode_utf32_le(<<C/utf32-little, Rest/binary>>) -> [C | decode_utf32_le(Rest)];
decode_utf32_le(<<B, Rest/binary>>) -> [{byte,B} | decode_utf32_le(Rest)].

decode_utf32_be(X) when is_atom(X) -> X;
decode_utf32_be(<<>>) -> [];
decode_utf32_be(<<C/utf32-big, Rest/binary>>) -> [C | decode_utf32_be(Rest)];
decode_utf32_be(<<B, Rest/binary>>) -> [{byte,B} | decode_utf32_be(Rest)].


main() ->
    io:format("~p\n", [test()]).
