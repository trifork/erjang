%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2012 Richard Carlsson
%% @doc A library for benchmarking Erlang code.
%% @url https://github.com/richcarl/berk/

-module(berk).

-export([runner/1, runner/2, stats/1, gather/3]).

-export([berps/0, berps/1, calibrate/0, calibrate/1,
	 read_rc/0, write_rc/1, run/2, run_for/1, run_for/2, rndbyte/0, rndbits/1, rndpbm/0]).

-export([floats/1, ints/2]).

-export([uniform/0, normal/0]).

-export([lin/2, lin/3, lin/4, log/2, log/3, log/4, linlin/3, linlin/5,
	 linlin/7, linlog/3, linlog/5, linlog/7, loglog/3, loglog/5,
	 loglog/7]).

-define(USE_OS_TIMESTAMP, true).

-include_lib("eunit/include/eunit.hrl").

%% TODO: filtering out outliers?

-export([test/0]).

test() ->
    run(fun () -> lists:seq(1,100) end, 1).

%% ------------------------------------------------------------------------

-define(OPT_MIN_HEAP_SIZE,  min_heap_size).
-define(OPT_VERBOSE,        verbose).
-define(OPT_BEFORE,         before).
-define(OPT_AFTER,          'after').
-define(OPT_SAMPLES,        samples).
-define(OPT_TIMEOUT,        timeout).
-define(OPT_BASELINE,       baseline).
-define(OPT_VARIATION,      variation).
-define(OPT_JITTER,         jitter).
-define(OPT_RESOLUTION,     resolution).
-define(OPT_ITERATIONS,     iterations).

-define(TAG_MEAN_RUNTIME,     mean_runtime).
-define(TAG_RUNTIME_MEANDEV,  runtime_meandev).
-define(TAG_RUNTIME_STDDEV,   runtime_stddev).
-define(TAG_STDDEV,           stddev).
-define(TAG_MEANDEV,          meandev).
-define(TAG_MEDDEV,           meddev).
-define(TAG_MAXDEV,           maxdev).
-define(TAG_STDERR,           stderr).
-define(TAG_ITERATIONS,       iterations).
-define(TAG_SAMPLES,          samples).
-define(TAG_DATA,             data).
-define(TAG_MIN,              min).
-define(TAG_MAX,              max).
-define(TAG_REL_STDDEV,       rel_stddev).
-define(TAG_REL_MEANDEV,      rel_meandev).
-define(TAG_REL_MAXDEV,       rel_maxdev).
-define(TAG_REL_MEDDEV,       rel_meddev).
-define(TAG_RANGE,            range).
-define(TAG_MEAN,             mean).
-define(TAG_MEDIAN,           median).
-define(TAG_GC_COUNT,         gc_count).
-define(TAG_GC_WORDS,         gc_words).
-define(TAG_REDUCTIONS,       reductions).
-define(TAG_RUNTIME,          runtime).
-define(TAG_SERIES,           series).

-define(DEFAULT_SAMPLES, 50).
-define(DEFAULT_TIMEOUT, 3600).

-define(MIN_HEAP_SIZE, 100000).

-define(RCFILE, ".berk").

-define(HALF_AN_HOUR_IN_SECONDS, 1800).

%% Runtime clock properties
-ifdef(USE_OS_TIMESTAMP).
-define(TICKS_PER_SECOND, 1000000).
-define(get_runtime, os:timestamp()).
-define(runtime_diff(T1,T0), timer:now_diff(T1,T0)).
-else.
-define(TICKS_PER_SECOND, 1000).
-define(get_runtime, erlang:statistics(runtime)).
-define(runtime_diff(T1,T0), (element(1,T1) - element(1,T0))).
-endif.

%% Remember: DO NOT USE SHELL FUNS AS BENCHMARKS! They use erl_eval!
-type benchmark() :: fun (() -> any()).

-type proplist() :: [proplists:property()].

%% ------------------------------------------------------------------------
%% @doc Computes BERPS (Bogus Erlang Reductions Per Second) for the machine.

%% Measurement of BERPS.

%%% TODO: what about this BERPS stuff? Use it for something, and how?

berps() ->
    berps(false).

berps(Verbose) ->
    P = spawn(fun () -> ok end),  % get a pid to a dead process
    berps_wait(P),
    F = runner(fun () -> P ! [] end, []),
    gather(F, fun calibrate_ctrl/5, [{?OPT_VERBOSE, Verbose}]).

%% This sends a message N*M times to a dead process; hopefully, the compiler
%% will never be able to eliminate this as dead code.

berps_wait(P) ->
    case erlang:process_info(P) of
        undefined ->
            ok;
        _ ->
            timer:sleep(1),
            berps_wait(P)
    end.

%% %% we don't use the generic runner/2 loop, because we want this code to be a
%% %% single, independent, and easily ported/reproduced unit

%% berps_loop(N) when N > 0, N =< ((1 bsl 27) - 1) ->
%%     %% N and M must be less than 2^27, to avoid all bignum arithmetic on
%%     %% the loop counters. The value M=1000 gives a total run time that
%%     %% is in the range 1-2 seconds for N=100 000 on a 2-4 GHz machine
%%     %% (like the one used at the time of writing), and should thus be in
%%     %% the same range for N=100 on a 2-4 MHz machine (e.g., an Amiga
%%     %% 500). This gives a minimum resolution for N=1 in the 20-40
%%     %% millisecond range on a 1MHz machine (if Erlang is ever ported to
%%     %% a Vic-20), and a maximum run time of up to 1-2 seconds on a 1 THz
%%     %% machine (if such machines are ever built), for N = 100 000 000
%%     %% (the highest power of 10 less than 2^27).
%%     M = 1000,
%%     R0 = erlang:statistics(exact_reductions),
%%     T0 = erlang:statistics(runtime),
%%     P = spawn(fun () -> ok end),  % get a pid to a dead process
%%     berps_loop0(N, M, P),
%%     T1 = erlang:statistics(runtime),
%%     R1 = erlang:statistics(exact_reductions),
%%     T = element(1,T1) - element(1,T0),
%%     R = element(1,R1) - element(1,R0),
%%     B = if T > 0 -> round(R/T); true -> 0 end,
%%     {T, R, B}.

%% %% This sends a message N*M times to a dead process; hopefully, the compiler
%% %% will never be able to eliminate this as dead code.

%% berps_loop0(N, M, P) ->
%%     case erlang:process_info(P) of
%%         undefined ->
%%             berps_loop1(N, M, P);
%%         _ ->
%%             timer:sleep(1),
%%             berps_loop0(N, M, P)
%%     end.

%% berps_loop1(N, M, P) when N > 0 ->
%%     berps_loop2(M, P),
%%     berps_loop1(N - 1, M, P);
%% berps_loop1(_, _, _) ->
%%     ok.

%% berps_loop2(N, P) when N > 0 ->
%%     P ! [],
%%     berps_loop2(N - 1, P);
%% berps_loop2(_, _) ->
%%     ok.

%% ------------------------------------------------------------------------
%% @doc Computes statistics over lists of numbers. Yields a property list
%% with the following entries:
%% <ul>
%%   <li>`{mean, number()}': the arithmetic mean over all inputs</li>
%%   <li>`{meandev, number()}': mean absolute deviation from the median</li>
%%   <li>`{data, [number()]}': the input list itself</li>
%%   <li>`{max, number()}': the largest input</li>
%%   <li>`{maxdev, number()}': maximum absolute deviation from the mean</li>
%%   <li>`{meddev, number()}': median absolute deviation from the median</li>
%%   <li>`{median, number()}': the median of all inputs</li>
%%   <li>`{min, number()}': the smallest input</li>
%%   <li>`{range, number()}': the difference between the smallest and
%%        largest inputs</li>
%%   <li>`{rel_meandev, number()}': relative mean absolute deviation</li>
%%   <li>`{rel_maxdev, number()}': relative maximum absolute deviation</li>
%%   <li>`{rel_meddev, number()}': relative median absolute deviation</li>
%%   <li>`{rel_stddev, number()}': relative standard deviation</li>
%%   <li>`{stddev, number()}': standard deviation</li>
%%   <li>`{stderr, number()}': standard error of the mean (SEM)</li>
%% </ul>
%% A good measurement set should consist of at least 30 samples, and
%% preferably 50 if time allows, but more than 70 is generally overkill.

-spec stats(Ns::[number()]) -> proplist().

stats(Ns) ->
    N = length(Ns),
    Min = lists:min(Ns),
    Max = lists:max(Ns),
    M = median(Ns),
    A = mean(Ns),
    SD = stddev(Ns, A),
    MaxD = maxdev(Ns, A),
    AD = meandev(Ns, M),
    MD = meddev(Ns, M),
    [{?TAG_MIN, Min},
     {?TAG_MAX, Max},
     {?TAG_RANGE, Max-Min},
     {?TAG_MEAN, A},
     {?TAG_MEDIAN, M},
     {?TAG_MAXDEV, MaxD},  % maximum absolute deviation (from the mean)
     {?TAG_REL_MAXDEV, relative(MaxD, A)},
     {?TAG_MEDDEV, MD},  % median absolute deviation (from the median)
     {?TAG_REL_MEDDEV, relative(MD,M)},
     {?TAG_MEANDEV, AD},  % mean absolute deviation (from the median)
     {?TAG_REL_MEANDEV, relative(AD, M)},
     {?TAG_STDDEV, SD},  % standard deviation
     {?TAG_REL_STDDEV, relative(SD, A)},
     {?TAG_STDERR, SD/math:sqrt(N)},  % standard error of the mean (SEM)
     {?TAG_DATA, Ns}
    ].

relative(X, A) when A > 0 -> X/A;
relative(_, _) -> 1.0e99.

mean(Ns) ->
    S = lists:foldl(fun (X, A) -> A + X end, 0, Ns),
    S / length(Ns).

median(Ns) ->
    N = length(Ns),
    Ss = lists:sort(Ns),
    if (N rem 2) > 0 ->
	    lists:nth(1+trunc(N/2), Ss);
       true ->
	    [X1,X2] = lists:sublist(Ss, trunc(N/2),2),
	    (X1+X2)/2
    end.

stddev(Ns, N0) ->
    S = lists:foldl(fun (X, A) -> D=X-N0, A + (D*D) end, 0, Ns),
    math:sqrt(S / length(Ns)).

maxdev(Ns, N0) ->
    lists:foldl(fun (X, A) -> max(A, abs(X-N0)) end, 0, Ns).

meddev(Ns, N0) ->
    %% median absolute deviation (usually, N0 is the median of Ns)
    median([abs(N-N0) || N <- Ns]).

meandev(Ns, N0) ->
    %% mean absolute deviation
    S = lists:foldl(fun (X, A) -> A + abs(X-N0) end, 0, Ns),
    S / length(Ns).

%% ------------------------------------------------------------------------
%% @doc Creates a benchmark runner function, that runs the provided
%% benchmark for a given number of iterations. Measurements are performed
%% before the first iteration and after the last. The standard measurements
%% are: `{runtime, CPUTime}', `{reductions, Reductions}', `{gc_count,
%% Sweeps}', and `{gc_words, WordsReclamed}'.
%%
%% For additional measurements, the user can specify options `{before, fun
%% ((ets:tab()) -> any())}' and `{after, fun ((ets:tab()) -> any())}'. As
%% input, these functions get a reference to the ets table storing the
%% measurements, and their return values are discarded. Typically, the
%% before-function stores a base value for reading and the after-function
%% takes a new reading and updates the entry to hold the delta instead. They
%% should not modify the entries for the standard measurements.

%% TODO: properly document all the options (variation, jitter etc.)

%% This reports runtime (wall clock time) in seconds per iteration, minus an
%% optional baseline. It furthermore allows the number of iterations to be
%% randomly varied, and jitter and clock resolution can be simulated.

-type runner() :: fun ((integer()) -> proplist()).

-spec runner(F::benchmark()) -> runner().

runner(F) ->
    runner(F, []).

-spec runner(F::benchmark(), Options::proplist()) -> runner().

runner(F, Opts) ->
    Before = proplists:get_value(?OPT_BEFORE, Opts, fun (_) -> [] end),
    After = proplists:get_value(?OPT_AFTER, Opts, fun (_) -> [] end),
    %% The baseline is per iteration and should include the calibrated
    %% times for the measurement loop
    Baseline = proplists:get_value(?OPT_BASELINE, Opts, 0.0),
    %% A small variation in run time (around 10%) is useful for
    %% overcoming the problem of artificially precise measurements when
    %% the actual deviations are smaller than the clock resolution
    Variation = proplists:get_value(?OPT_VARIATION, Opts, 0.0),
    %% These options are used to simulate jitter and clock resolution
    Jitter = proplists:get_value(?OPT_JITTER, Opts, 0.0),
    Resolution0 = proplists:get_value(?OPT_RESOLUTION, Opts,
                                      1/?TICKS_PER_SECOND),
    Resolution = max(1, round(?TICKS_PER_SECOND * Resolution0)),
    B = fun (Tab) ->
                Before(Tab),  % user function always runs first
                %% The standard measurements are: CPU time (runtime),
                %% reductions, and GC (sweeps and words reclaimed)
                ets:insert(Tab, {garbage_collection,  % temporary entry
                                 erlang:statistics(garbage_collection)}),
                ets:insert(Tab, {?TAG_REDUCTIONS,
                                 erlang:statistics(exact_reductions)}),
                %% runtime is always the last thing here
                ets:insert(Tab, {?TAG_RUNTIME, ?get_runtime})
	 end,
    A = fun (Tab) ->
                %% runtime is always the first thing here
                T1 = ?get_runtime,
                R1 = erlang:statistics(exact_reductions),
                G1 = erlang:statistics(garbage_collection),
                After(Tab),  % user function runs as soon as possible
                [{_,T0}] = ets:lookup(Tab, ?TAG_RUNTIME),
                [{_,R0}] = ets:lookup(Tab, ?TAG_REDUCTIONS),
                [{_,G0}] = ets:lookup(Tab, garbage_collection),
                ets:insert(Tab, {?TAG_RUNTIME, ?runtime_diff(T1,T0)}),
                ets:insert(Tab, {?TAG_REDUCTIONS,
                                 element(1,R1) - element(1,R0)}),
                ets:insert(Tab, {?TAG_GC_COUNT,
                                 element(1,G1) - element(1,G0)}),
                ets:insert(Tab, {?TAG_GC_WORDS,
                                 element(2,G1) - element(2,G0)}),
                ets:delete(Tab, garbage_collection)
        end,

    fun (N0) ->
	    %% add variation to the number of iterations
            {Rand1, Rand2} = normal(),
	    N = round(N0*(1 + Variation*Rand1)),
            %% simulate jitter for this sample
            SampleJitter = 1 + Jitter*Rand2,
            TimeF = time_function(N, Resolution, SampleJitter),
            %% run the measurement in a fresh process
	    inprocess(
              fun () ->
                      Tab = ets:new(?MODULE, [private]),
                      ets:insert(Tab, {?TAG_ITERATIONS, N}),
                      loop0(N, F, B, A, Tab),
                      %% replace total runtimes with seconds per iteration
                      [{_,T0}] = ets:lookup(Tab, ?TAG_RUNTIME),
                      T1 = abs(TimeF(T0) - Baseline),
                      ets:insert(Tab, {?TAG_RUNTIME, T1}),
                      ets:insert(Tab, {?TAG_RUNTIME, T1}),
                      %% the table gets deleted when the process dies
                      ets:tab2list(Tab)
              end,
              Opts)
    end.

time_function(Iter, Resolution, Jitter) ->
    fun(T0) ->
            %% add simulated jitter
            T1 = round(T0*Jitter),
            %% truncate to desired resolution
            T2 = T1 - (T1 rem Resolution),
            %% divide by iterations and ticks per second
            T2/(Iter*?TICKS_PER_SECOND)
    end.

%% This automatically splits the given number of iterations into an inner
%% and outer loop, as necessary, to avoid bignum arithmetic in the loop
%% counters. The inner counter N must be strictly less than 2^27 to be an
%% immediate.
loop0(N0, F, Before, After, Tab) ->
    D = 1 bsl 24, % a good, safe number
    N = N0 div D,
    R = N0 rem D,
    Before(Tab),
    loop1(N, F, D, R),
    After(Tab),
    ok.

%% the number of reductions counted in this nested loop (apart from any work
%% done within F itself) should be 1+2*N for loop2 (one for each time loop2
%% is entered, and one for each call of F), and 1+N for the outer loop (one
%% for each time loop1 is entered). In total N*(1+2*D)+(1+2*R).
loop1(N, F, D, R) when N > 0 ->
    loop2(D, F),
    loop1(N - 1, F, D, R);
loop1(_N, F, _D, R) ->
    loop2(R, F).

loop2(N, F) when N > 0 ->
    F(),  % call the benchmark fun
    loop2(N - 1, F);
loop2(_, _) ->
    ok.

%% Running a fun in a separate (linked) process, but letting result and
%% exceptions propagate through as if it was executed directly. Takes
%% min_heap_size option, but no other process options.
inprocess(F, Opts) when is_function(F,0) ->
    Parent = self(),
    MinHeapSize = proplists:get_value(?OPT_MIN_HEAP_SIZE, Opts, ?MIN_HEAP_SIZE),
    Pid = spawn_opt(fun () ->
                            M = try {ok, F()}
                                catch
                                    Class:Term ->
                                        %% remove the spawn-fun from the
                                        %% trace, leaving only user code
                                        Trace0 = erlang:get_stacktrace(),
                                        Trace = lists:reverse(
                                                  tl(lists:reverse(Trace0))),
                                        {error, {Class, Term, Trace}}
                                end,
                            Parent ! {self(), M}
                    end,
                    [link, {min_heap_size, MinHeapSize}]),
    receive
	{Pid, {ok, X}} ->
	    X;
	{Pid, {error, {Class, Term, Trace}}} ->
	    erlang:raise(Class, Term, Trace)
    end.

%% return a uniformly distributed number between -1 and 1
uniform() ->
    2*random:uniform()-1.

%% return two normally distributed numbers between -1 and 1
normal() ->
    %% this uses the Marsaglia polar method
    X = uniform(),
    Y = uniform(),
    case X*X + Y*Y of
        S when S < 1.0 ->
            R = math:sqrt(-2*math:log(S)/S),
            {X*R, Y*R};
        _ ->
            normal()  % try again
    end.

%% ------------------------------------------------------------------------
%% @doc Runs a benchmark runner function, gathering statistics incrementally
%% until a controlling function signals completion.
%%
%% This executes `Runner' one or more times with different numbers of
%% iterations as input, and returns a list of the statistics for each
%% execution. Each time a new valid set of statistics for an execution gets
%% added, the given function `Control' is called to decide how to proceed.
%% If it returns `stop', the gathering stops and returns the gathered
%% statistics. If it returns `{next, N::integer()}', `Runner' will be
%% executed again using `N' iterations. If it returns `{samples,
%% S::integer()}', the gathering is resumed for the current number of
%% iterations for `Runner' but with the number of samples changed to `S'.
%%
%% This function adds some additional information to the statistics returned
%% by `Runner', for each sample set:
%% <ul>
%%  <li>`{iterations, integer()}': number of iterations given to `Runner' to
%%      produce the data</li>
%% </ul>
%%
%% Options are:
%% <ul>
%%  <li>`{samples, integer()}': initial number of executions of `Runner'
%%      for a data series using the same number of iterations as input</li>
%%  <li>`{timeout, number()}': time limit for `Runner', in seconds</li>
%%  <li>`{verbose, boolean()}': progress reports</li>
%% </ul>
%%
%% If the statistics for an execution of `Runner' is deemed as invalid, all
%% gathered statistics so far are discarded and gathering continues from a
%% blank slate. Statistics are considered invalid if runtimes are not at
%% least 20 times the clock resolution, or (unless there is only one sample)
%% if the difference between min and max is zero or the relative standard
%% deviation is greater than 50%.

%% Note that `Stats' will be [] as long as measurements are unstable
-type controller() :: fun ((Iterations::integer(),
                            Seconds::integer(),
                            Stats::proplist(),
                            Report::fun ((any()) -> any()))
                           -> stop | {next, integer()} | {samples, integer()}).

-spec gather(runner(), controller(), Options::proplist()) -> [proplist()].

gather(Runner, Control, Opts) ->
    Report = proplists:get_value(?OPT_VERBOSE, Opts, false),
    gather(Runner, Control, Opts, Report).

%% `Report' can be true, false, a pid, or a fun
gather(Runner, Control, Opts, true) ->
    %% start default verbosity listener, and shut it down afterwards
    Self = self(),
    Report = spawn_link(fun() -> listen(Self) end),
    try
	gather(Runner, Control, Opts, Report)
    after
	Report ! stop
    end;
gather(Runner, Control, Opts, false) ->
    Report = fun (_Msg) -> ok end,
    gather(Runner, Control, Opts, Report);
gather(Runner, Control, Opts, Report)
  when not is_function(Report) ->
    Report1 = fun (Msg) -> send_progress(Report, Msg) end,
    gather(Runner, Control, Opts, Report1);
gather(Runner, Control, Opts, Report) ->
    Iter = max(1, proplists:get_value(?OPT_ITERATIONS, Opts, 1)),
    Samples = max(1, proplists:get_value(?OPT_SAMPLES, Opts,
                                         ?DEFAULT_SAMPLES)),
    Timeout = proplists:get_value(?OPT_TIMEOUT, Opts, ?DEFAULT_TIMEOUT),
    T0 = ?get_runtime,  % track total time spent
    gather(Runner, Samples, Control, Report, Iter, T0, Timeout, [], []).

gather(Runner, Samples, Control, Report, Iter, T0, Timeout, Ds0, Stats0)
  when is_function(Runner), is_function(Control), is_function(Report),
       is_integer(Samples), Samples > 0, is_integer(Iter), Iter > 0,
       is_list(Ds0) ->
    %% generate samples and run statistics
    Samples0 = length(Ds0),
    Ds = if Samples0 =< Samples ->
		 Ds0 ++ repeat(Samples - Samples0, fun () -> Runner(Iter) end);
	    true ->
                 lists:sublist(Ds0, Samples)
	 end,
    Ms0 = multistats(Ds),

    %% extend the stats with information about the data series
    IterStats = proplists:get_value(?TAG_ITERATIONS, Ms0),
    MeanIter = round(proplists:get_value(?TAG_MEAN, IterStats)),
    RuntimeStats = proplists:get_value(?TAG_RUNTIME, Ms0),
    MeanRuntime = proplists:get_value(?TAG_MEAN, RuntimeStats),
    Ms = [{?TAG_SERIES, [{?TAG_SAMPLES,    Samples},
                         {?TAG_ITERATIONS, MeanIter},
                         {?TAG_RUNTIME,    Samples*MeanIter*MeanRuntime}
                        ]}
	  | Ms0],

    %% report the latest statistics (even if unstable)
    Report({stats, Ms}),

    %% get current time spent in seconds
    T1 = ?get_runtime,
    Time = ?runtime_diff(T1,T0) / ?TICKS_PER_SECOND,
    Report({time, Time}),

    %% ensure we only keep valid statistics
    Stats = case is_valid_stats(Iter, RuntimeStats) of
                true -> [Ms | Stats0];
                false ->
                    Report(unstable),
                    []  % discard all measurements so far
            end,

    MinRuntime = proplists:get_value(?TAG_MIN, RuntimeStats),

    if Iter*MinRuntime*?TICKS_PER_SECOND < 20 ->
            %% all runtimes must be at least 20 times the clock resolution,
            %% otherwise adjust number of iterations upwards and retry
            Iter1 = max(Iter+1, round((20/(MinRuntime*?TICKS_PER_SECOND))*1.1)),
	    gather(Runner, Samples, Control, Report, Iter1, T0, Timeout,
                   Ds, Stats0);
       true ->
            %% call control function
            case Control(Samples, Iter, Time, Stats, Report) of
                stop ->
                    Report(done),
                    Stats;
                {samples, Samples1} ->
                    %% adjust number of samples for the same Iter and
                    %% recompute stats
                    gather(Runner, Samples1, Control, Report, Iter, T0,
                           Timeout, Ds, Stats0);
                {next, Iter1} when is_integer(Iter1), Iter1 > 0 ->
                    %% calculate approximate time for next loop (if any) and
                    %% check for safety timeout
                    T = round(MeanRuntime*Samples*Iter1),
                    if T >= Timeout ->
                            Report(timeout),
                            Stats;
                       true ->
                            Report({wait, T}),
                            gather(Runner, Samples, Control, Report, Iter1,
                                   T0, Timeout, [], Stats)
                    end
            end
    end.

%% runs F() N times
repeat(N, F) when N > 0 ->
    [F() | repeat(N-1, F)];
repeat(_, _) ->
    [].

%% Statistics for a series are considered valid if (unless we have just a
%% single sample point) there must be a nonzero range between min and max,
%% and the relative maximum absolute deviation must be less than 35%.
is_valid_stats(_Iter, Ss) ->
    Samples = length(proplists:get_value(?TAG_DATA, Ss)),
    Min = proplists:get_value(?TAG_MIN, Ss),
    Max = proplists:get_value(?TAG_MAX, Ss),
    Dev = proplists:get_value(?TAG_REL_MAXDEV, Ss),
    (Samples < 2) orelse ((Max > Min) and (Dev < 0.35)).

%% @doc Computes statistics over lists of lists of tagged 2-tuples. All
%% lists must have the same length and the same tag order. For example, for
%% the input `[[{a,1},{b,10}], [{a,2},{b,20}]]' you get the resulting list
%% `[{a, StatsA}, {b, StatsB}]' where `StatsA' is the result of
%% `stats([1,2])' and `StatsB' is the result of `stats([10,20])'.
%% @see stats/1
multistats(Tss) ->
    [{T, stats(Ns)} || {T, Ns} <- collate(Tss)].

collate([[{Tag, X} | Ts] | Tss]) ->
    [{Tag, [X | [X1 || [{_,X1}|_] <- Tss]]}
     | collate([Ts | [Ts1 || [_|Ts1] <- Tss]])];
collate([[] | _Tss]) ->
    [];
collate([]) ->
    [].

%% progress messages

send_progress(P, M) ->
    P ! {self(), M}.

%% TODO: make this an official API for using a custom listener
listen(P) ->
    receive
	{P, unstable} -> io:put_chars(user,"unstable\n");
	{P, timeout} -> io:put_chars(user,"timeout limit exceedeed\n");
	{P, done} -> io:put_chars(user,"finished\n");
	{P, {time, T}} -> io:format(user,"time spent: ~w\n",[T]);
	{P, {stats, Ss}} -> io:format(user,"stats: ~p\n",[Ss]);
	{P, {wait, T}} when T < 5 -> ok;
	{P, {wait, T}} -> io:format(user,"please wait ~s...\n",[times(T)]);
	{P, {text, S}} -> io:format(user,"~s\n",[S]);
	stop ->
	    io:put_chars(user, "listener stopped\n"),
	    exit(normal);
	_X -> ok  % io:format(user,"** bad progress message: ~w\n",[X])
    end,
    listen(P).

times(T) when T < 70 ->
    io_lib:format("~w seconds", [T]);
times(T) ->
    S0 = io_lib:format("~w min. ~.2.0w seconds", [T div 60, T rem 60]),
    if T >= 300 ->
	    {_,{Hr,Mn,_}} = calendar:gregorian_seconds_to_datetime(
			      calendar:datetime_to_gregorian_seconds(
				erlang:localtime()) + T + 30),
	    io_lib:format("~s (until approx. ~w:~.2.0w)", [S0, Hr, Mn]);
       true ->
	    S0
    end.

%% ------------------------------------------------------------------------
%% Calibration of clock precision and baseline for the measurement loop;
%% this can take about one minute or two to run.

calibrate() ->
    calibrate(false).

calibrate(_Verbose) ->
    %% A minimal function that does nothing
    F = fun () -> [] end,
    %% Some variation (10% is typically enough) is useful for
    %% calibration, in case the clock has poor actual resolution.
    gather(runner(F, [{?OPT_VARIATION,0.1}
			     %% these are for simulating poor clocks
			     %% , {?OPT_JITTER,0.01}
			     %% , {?OPT_RESOLUTION,1/100}
			    ]),
           fun calibrate_ctrl/5,
	  [{?OPT_VERBOSE, true}]).

calibrate_ctrl(_Samples, _Iter, Time, _Stats, _Report)
  when Time > ?HALF_AN_HOUR_IN_SECONDS ->
    stop;
calibrate_ctrl(Samples, Iter, _Time, [Ms0, Ms1, Ms2 | _], Report) ->
    Ss0 = proplists:get_value(?TAG_RUNTIME, Ms0),
    Ss1 = proplists:get_value(?TAG_RUNTIME, Ms1),
    Ss2 = proplists:get_value(?TAG_RUNTIME, Ms2),

    RD = proplists:get_value(?TAG_REL_MEANDEV, Ss0),
    RS = proplists:get_value(?TAG_REL_STDDEV, Ss0),
    RM = proplists:get_value(?TAG_REL_MEDDEV, Ss0),

    RD1 = proplists:get_value(?TAG_REL_MEANDEV, Ss1),
    RS1 = proplists:get_value(?TAG_REL_STDDEV, Ss1),
    RM1 = proplists:get_value(?TAG_REL_MEDDEV, Ss1),
    CRD = relative(RD1-RD, RD1),
    CRS = relative(RS1-RS, RS1),
    CRM = relative(RM1-RM, RM1),
    Report({text, io_lib:format("~p\n", [{{"rel_meandev change",CRD},
                                          {"rel_stddev change",CRS},
                                          {"rel_meddev change",CRM}}])}),
    RD2 = proplists:get_value(?TAG_REL_MEANDEV, Ss2),
    RS2 = proplists:get_value(?TAG_REL_STDDEV, Ss2),
    RM2 = proplists:get_value(?TAG_REL_MEDDEV, Ss2),
    CRD1 = relative(RD2-RD1, RD2),
    CRS1 = relative(RS2-RS1, RS2),
    CRM1 = relative(RM2-RM1, RM2),
    ACRD = (CRD+CRD1)/2,
    ACRS = (CRS+CRS1)/2,
    ACRM = (CRM+CRM1)/2,
    Report({text, io_lib:format("~p\n", [{{"rel_meandev meanchange",ACRD},
                                          {"rel_stddev meanchange",ACRS},
                                          {"rel_meddev meanchange",ACRM}}])}),
    %% Continue until the smoothed change in relative median absolute
    %% deviation is too small and the smoothed change in relative
    %% standard deviation is also not large enough to motivate
    %% continuing, or simply until the last run took too long time
    Mean = proplists:get_value(?TAG_MEAN, Ss0),
    T1 = round(Mean*Samples*Iter),
    if ACRM < 0.1, ACRS < 0.25 ; T1 > (60*25) ->
	    stop;
       true ->
	    {next, Iter*2}
    end;
calibrate_ctrl(_Samples, Iter, _Time, _Stats, _Report) ->
    {next, Iter*2}.

%% ------------------------------------------------------------------------
%% Running a function for at least Lim seconds

run_for(F) ->
    run_for(F, 1.0).

run_for(F, Lim) ->
    gather(runner(F, [{?OPT_VARIATION,0.0}]),
           fun (Samples, Iter, Time, Stats, _Report) ->
                   run_for_ctrl(Samples, Iter, Time, Stats, Lim)
           end,
           [{?OPT_SAMPLES, 1},
            {?OPT_VERBOSE, true}]).

run_for_ctrl(_Samples, _Iter, Time, _Stats, _Lim)
  when Time > ?HALF_AN_HOUR_IN_SECONDS -> stop;
run_for_ctrl(Samples, Iter, _Time, [Ms | _], Lim) ->
    SeriesStats = proplists:get_value(?TAG_SERIES, Ms),
    SeriesRuntime = proplists:get_value(?TAG_RUNTIME, SeriesStats),
    RuntimeStats = proplists:get_value(?TAG_RUNTIME, Ms),
    MeanRuntime = proplists:get_value(?TAG_MEAN, RuntimeStats),
    %%RelStdDev = proplists:get_value(?TAG_REL_STDDEV, RuntimeStats),
    if Iter*MeanRuntime > Lim*0.99 ->
            ?debugVal(Iter*MeanRuntime),
	    stop;
       SeriesRuntime < 5.0, Samples < ?DEFAULT_SAMPLES ->
            ?debugVal({samples, max(Samples+1,
                          min(round(5.0/SeriesRuntime), ?DEFAULT_SAMPLES))});
       MeanRuntime > 0 ->
	    %% aim for target time plus 2%, but don't use too small
	    %% increments (at least 25%) and never decrement
	    ?debugVal({next, round(max(1.25*Iter, 1.02*(Lim/MeanRuntime)))});
       true ->
	    {next, Iter*2}
    end;
run_for_ctrl(_Samples, Iter, _T, _Stats, _Lim) ->
    %% unstable, so double the number of iterations
    {next, Iter*2}.

%% ------------------------------------------------------------------------
%% Running a function for a given minimum number of iterations

run(F, Iter) ->
    run(F, Iter, []).

run(F, Iter, Opts) ->
    gather(runner(F, [{?OPT_VARIATION,0.0}]),
           fun run_ctrl/5,
           [{?OPT_ITERATIONS, Iter},
            {?OPT_SAMPLES, 1}
            | Opts]).

run_ctrl(_Samples, Iter, Time, _Stats, _Report) when Time < 0.01 ->
    %% there's plenty of time, so increase iterations
    {next, round(Iter*1.62)};  % fibonacci-like progression
run_ctrl(Samples, _Iter, Time, _Stats, _Report)
  when Time < 0.2, Samples < ?DEFAULT_SAMPLES ->
    %% we've not spent that much time, so get more samples
    {samples, Samples+1};
run_ctrl(_Samples, Iter, Time, _Stats, _Report)
  when Time < 0.5 ->
    %% get another measurement series if time allows
    {next, Iter};
run_ctrl(_Samples, _Iter, _Time, _Stats, _Report) ->
    stop.

%% ------------------------------------------------------------------------
%% resource file handling

rcdir() ->
    case init:get_argument(home) of
	{ok, [[Home]]} -> Home;
	error -> "."
    end.

rcfile() ->
    filename:join(rcdir(), ?RCFILE).

read_rc() ->
    case file:consult(rcfile()) of
	{ok, Ts} ->
	    Ts;
	{error, E} ->
            throw({read_error, E})
    end.

%% sorts Ts on unique keys (keeps first occurrence) and writes to rcfile
write_rc(Ts) ->
    case file:open(rcfile(), [write]) of
	{ok, FD} ->
	    try
		lists:foreach(fun (T) -> write_term(FD, T) end,
                              lists:ukeysort(1, Ts))
	    after
		file:close(FD)
	    end;
	{error, E} ->
	    throw({write_error, E})
    end.

write_term(FD, T) ->
    case io:format(FD, "~p.\n", [T]) of
        {error, E} ->
            throw({write_error, E});
        _ ->
            ok
    end.


%% ------------------------------------------------------------------------
%% Entropy-based slow random bit generation

rndbyte() ->
    <<N:8>> = << <<B:1/integer>> || B <- rndbits(8) >>,
    N.

%% rndbyte2() ->
%%     <<N:8>> = crypto:rand_bytes(1),
%%     N.

rndbit() ->
    timer:sleep(1),
    element(3, erlang:now()) rem 2.

rndbits(N) ->
    [rndbit() || _<-lists:seq(1,N)].

%% convert to png with 'pnmtopng rnd.pbm > rnd.png'
rndpbm() ->
    W = 256,
    H = 128,
    {ok, FD} = file:open("rnd.pbm", [write]),
    io:format(FD, "P4\n~w ~w\n", [W, H]),
    _ = [io:fwrite(FD, "~s", [[rndbyte()||_<-lists:seq(1,W div 8)]])
         || _ <- lists:seq(1,H)],
    file:close(FD),
    ok.


%% ------------------------------------------------------------------------
%% Generating lists of random numbers

floats(N) when is_integer(N), N >= 0 ->
    floats(N, []).

floats(N, As) when N > 0 ->
    floats(N - 1, [random:uniform() | As]);
floats(0, As) ->
    As.

ints(N, M) when is_integer(N), N >= 0 ->
    ints(N, M, []).

ints(N, M, As) when N > 0 ->
    ints(N - 1, M, [random:uniform(M) | As]);
ints(0, _M, As) ->
    As.


%% ------------------------------------------------------------------------
%% Run a function with an input from a 1-D or 2-D interval

%% 1-D functions

lin(F, N) ->
    lin(F, 1, N).

lin(F, N, M) ->
    lin(F, N, M, 1).

lin(F, N, M, D)
  when is_function(F), is_integer(N), is_integer(M), is_integer(D),
       N =< M, D > 0 ->
    [F(N) | lin(F, N + D, M, D)];
lin(_F, _N, _M, _D) ->
    [].

log(F, N) ->
    log(F, 1, N).

log(F, N, M) ->
    %% the defult logarithm is 2
    log(F, N, M, 2).

log(F, N, M, K)
  when is_function(F), is_number(N), is_number(M), is_number(K),
N > 0, N =< M, K > 1 ->
    [F(N) | log(F, N * K, M, K)];
log(_F, _N, _M, _K) ->
    [].

%% 2-D functions

linlin(F, N1, N2) ->
    linlin(F, 1, N1, 1, N2).

linlin(F, N1, M1, N2, M2) ->
    linlin(F, N1, M1, 1, N2, M2, 1).

linlin(F, N1, M1, D1, N2, M2, D2) ->
    F1 = fun (X) -> lin(fun (Y) -> F(X,Y) end, N2, M2, D2) end,
    lin(F1, N1, M1, D1).

linlog(F, N1, N2) ->
    linlog(F, 1, N1, 1, N2).

linlog(F, N1, M1, N2, M2) ->
    linlog(F, N1, M1, 1, N2, M2, 2).

linlog(F, N1, M1, D, N2, M2, K) ->
    F1 = fun (X) -> log(fun (Y) -> F(X,Y) end, N2, M2, K) end,
    lin(F1, N1, M1, D).

loglog(F, N1, N2) ->
    loglog(F, 1, N1, 1, N2).

loglog(F, N1, M1, N2, M2) ->
    loglog(F, N1, M1, 2, N2, M2, 2).

loglog(F, N1, M1, K1, N2, M2, K2) ->
    F1 = fun (X) -> log(fun (Y) -> F(X,Y) end, N2, M2, K2) end,
    log(F1, N1, M1, K1).
