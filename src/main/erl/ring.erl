-module(ring).
%%-import(lists, [last/1]).
-export([startRing/2,main/0]).
-define(CYCLES, 10000).
-define(PROCS, 1000).

%%
last([End|[]]) -> 
    End;
last([_|Tail]) ->
    last(Tail).


%% returns first pid in the ring of size N
startRing(N,TimerPid) ->
    Pids = spawnNodes(N, [], TimerPid),
    connectNodes([last(Pids)|Pids]).

spawnNodes(0, Pids, _TimerPid) -> Pids;
spawnNodes(Remaining, Pids, TimerPid) ->
    Pid = spawn(fun() -> unconnectedNode(TimerPid) end),
    spawnNodes(Remaining-1, [Pid|Pids], TimerPid).

%% Connect F and S until there are no nodes left to connect
connectNodes([F,S|[]]) ->
    F ! {connect,S},
    S;
connectNodes([F,S|T]) ->
    F ! {connect,S},
    connectNodes([S|T]).

%% Run node unconnected until there is a connect message
unconnectedNode(TimerPid) ->
    receive
	{connect,SendTo} ->
	    nodeloop(SendTo,TimerPid)
    end.

%% When connected, run loop waiting for either a stop or {Source,Val}
%% message where Source is the Pid of the node that originated the message.
nodeloop(SendTo,TimerPid) ->
    receive
	{stop,_From} ->
	    SendTo ! {stop,self()}, 
	    % io:format("~p received stop from ~p ~n", [self(), From]),
	    done;
	start ->
	    io:format("~p Starting message, cycles=~p~n", [now(), ?CYCLES]),
	    TimerPid ! xstart,
		%	io:format("msg[~p] ~p -> ~p~n", [0, self(), SendTo]),
	    SendTo ! {self(), 0},
	    nodeloop(SendTo,TimerPid);
	{Source, Val} when Source =:= self() ->
	    logOnNth(Val+1, ?CYCLES),
	    if
		Val+1 == ?CYCLES -> 
			io:format("~p == ~p ... stop ~n", [Val+1, ?CYCLES]),
			TimerPid ! stop, 
			SendTo ! {stop,self()};
		true -> 
			% io:format("msg[~p] ~p -> ~p~n", [Val+1, self(), SendTo]),
			SendTo ! {Source, Val+1}
	    end,
	    nodeloop(SendTo,TimerPid);
	{Source,Val} ->
	    % io:format("msg[~p] ~p -> ~p~n", [Val, self(), SendTo]),
	    SendTo ! {Source, Val},
	    nodeloop(SendTo,TimerPid)
    end.

%% Log if Val is Nth value
logOnNth(Val, Nth) when Val rem Nth == 0 ->
    io:format("~p Around ring ~p times ~n", [now(), Val]);
logOnNth(_Val, _Nth) -> void.

%% Start timer listener process, return Pid of timer listener
startTimer(Main) ->
    spawn(fun() -> timerOff(Main) end).

timerOff(Main) ->
    receive
	cancel -> void;
	xstart -> timerOn(Main,now())
    end.

timerOn(Main,Start) ->
    receive
	cancel -> {timer,aborted};
	stop ->
	    End=now(),
	    Main ! [Start,End] ,
	    timerOff(Main)	    
    end.

diff(Start, End) ->
    {_,StartSeconds,StartMicros} = Start,
    {_,EndSeconds,EndMicros} = End,
    ((EndSeconds*1000000) + EndMicros) - ((StartSeconds*1000000) + StartMicros).

main()->
    main2(),
    main2(),
    main2(),
    main2(),
    main2().

main2() ->
    T = startTimer(self()),
    R = startRing(?PROCS, T),
    R ! start,
    receive [Start,End] -> io:format("Start=~p Stop=~p Elapsed=~pus~n", [Start,End,diff(Start,End)]) end.
   

