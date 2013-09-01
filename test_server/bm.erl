-module(bm).

-export([main/0]).

format({M, F, I, B}) ->
    io:format("mod: ~p fun: ~p ins: ~p~n~p~n", [M, F, I, B]).

mean(Ns) ->
    S = lists:foldl(fun (X, A) -> A + X end, 0, Ns),
    S / length(Ns).

stddev(Ns, N0) ->
    S = lists:foldl(fun (X, A) -> D=X-N0, A + (D*D) end, 0, Ns),
    math:sqrt(S / length(Ns)).

run(F, _I, berk) ->
    berk:run(F, 5);
run(F, I, no_berk) ->
    NN = lists:seq(1, 5),
    T = lists:map(
	  fun(_) ->
		  T0 = now(),				% start timer
		  [ F() || _ <- NN ], 			% call F
		  1.0e-6*timer:now_diff(now(), T0)/I	% average time per call
	  end,
	  NN),
    A = mean(T),
    {{mean, round(A)}, {stddev, round(stddev(T, A))}}.

bench(M, {I, Fns}) ->
    [format(T) || T <- [{M, F, I, run(fun() -> M:F(I) end, I, berk)} || F <- Fns]].

main() ->
    [bench(M, M:benchmarks()) ||
	M <- [bin_to_term_bm, bs_simple_bm, call_bm, freq_bm, lists_bm, bs_bm,
	      bs_sum32_bm, call_tail_bm, fun_bm, ref_bm, bs_float_bm,
	      bs_sum_bm, float_bm, lc_bm, sort_bm]].
