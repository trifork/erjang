-module(bytematch_tests).
-compile(export_all).

test() ->
    Spec = [
	    <<>>,
	    <<0>>,
	    <<1,2>>,
	    <<10,20,30>>,
	    <<128,192,64,255>>,
	    <<"whatever">>,
	    <<"etc. etc. etc.">>
	   ],
    [bin2bytes(X) || X<-Spec].

bin2bytes(<<>>) -> [];
bin2bytes(<<X, Rest/binary>>) -> [X | bin2bytes(Rest)].

main() ->
    io:format("~p\n", [test()]).
