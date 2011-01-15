-module(facfib_tests).

-compile(export_all).

test() ->
    {[fac(X) || X <- lists_seq(1,200)],
     [tail_fac(X) || X <- lists_seq(1,200)],
     [slow_fib(X) || X <- lists_seq(1,20)],
     [fast_fib(X) || X <- lists_seq(1,200)]}.

fac(0) -> 1;
fac(N) -> N * fac(N-1).

tail_fac(N) -> tail_fac(N,1).
tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) -> tail_fac(N-1, N*Acc).

slow_fib(0) -> 0;
slow_fib(1) -> 1;
slow_fib(N) -> slow_fib(N-2) + slow_fib(N-1).

fast_fib(N) -> fast_fib(N,0,1).
fast_fib(0, A,_) -> A;
fast_fib(1, _,B) -> B;
fast_fib(N, A,B) -> fast_fib(N-1, B, A+B).

lists_seq(A,B) ->
    if A>B -> [];
       true -> [A | lists_seq(A+1,B)]
    end.
