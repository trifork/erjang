-module(fib).
-export([fibo/1, fibo2/1, fibo3/1, print_nfibos/2, main/0, runLength/2, main2/0]).

%% print fibo arg. and result, with function as parameter

print_nfibos( N, FiboFunc) -> printfibos( N, FiboFunc, 0).

printfibos( 0, FiboFunc, N) ->  %% last recursion
   Res = FiboFunc(N),
   io:format("~w ~w~n", [N, Res]) ;

printfibos( Iter, FiboFunc, N) when Iter > 0 -> 
   Res = FiboFunc(N),
   io:format("~w ~w~n", [N, Res]),
   printfibos( Iter -1, FiboFunc, N +1).

   

fibo(0) -> 0 ;
fibo(1) -> 1 ;
fibo(N) when N > 0 -> fibo(N-1) + fibo(N-2) .

   
fibo2_tr( 0, Result, _Next) -> Result ;  %% last recursion output

fibo2_tr( Iter, Result, Next) when Iter > 0 -> fibo2_tr( Iter -1, Next, Result + Next) .

fibo2( N) -> fibo2_tr( N, 0, 1) .


fibo3(N) ->
    {Fib, _} = fibo3(N, {1, 1}, {0, 1}),
    Fib.

fibo3(0, _, Pair) -> Pair;
fibo3(N, {Fib1, Fib2}, Pair) when N rem 2 == 0 ->
    SquareFib1 = Fib1*Fib1,
    fibo3(N div 2, {2*Fib1*Fib2 - SquareFib1, SquareFib1 + Fib2*Fib2}, Pair);
fibo3(N, {FibA1, FibA2}=Pair, {FibB1, FibB2}) ->
    fibo3(N-1, Pair, {FibA1*FibB2 + FibB1*(FibA2 - FibA1), FibA1*FibB1 + FibA2*FibB2}).
   
   
time(F,N) ->
   {Time,_Res} = timer:tc(fib,F,[N]),
   io:format("~w,~w,~w~n", [F,N,Time]).
   
tlength(L) -> tlength(L,0).
tlength([_|T],Acc) -> tlength(T,Acc+1);
tlength([],Acc) -> Acc.

makeList(N) when N==0 -> [];
makeList(N) -> [N,makeList(N-1)].   
   
runLength(_,N) when N==0 -> done;
runLength(L,N) -> tlength(L), runLength(L,N-1).
   
main2() ->
   List = makeList(2000),
   io:format("time=~w~n", [timer:tc(fib,runLength,[List,100000])]).

main() ->
    main_fib(),
    main_fib(),
    main_fib().    
       
main_fib() ->   
   time(fibo3,10),
   time(fibo3,100),
   time(fibo3,1000),
   time(fibo3,10000),
   time(fibo3,100000),
   time(fibo3,1000000).
   
   
