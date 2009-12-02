-module(ring).
%%-import(lists, [last/1]).
-export([startRing/2,main/0]).

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
connectNodes([F|[S|T]]) ->
    F ! {connect,S},
    if
	length(T) > 0 -> connectNodes([S|T]);
	true -> S
    end.

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
	stop ->
	    SendTo ! stop;
	start ->
	    io:format("~p Starting message~n", [now()]),
	    TimerPid ! start,
	    SendTo ! {self(), 0},
	    nodeloop(SendTo,TimerPid);
	{Source, Val} when Source == self() ->
	    logOnNth(Val+1, 10000),
	    if
		Val+1 == 1000000 -> TimerPid ! stop, SendTo ! stop;
		true -> SendTo ! {Source, Val+1}
	    end,
	    nodeloop(SendTo,TimerPid);
	{Source,Val} ->
	    SendTo ! {Source, Val},
	    nodeloop(SendTo,TimerPid)
    end.

%% Log if Val is Nth value
logOnNth(Val, Nth) when Val rem Nth == 0 ->
    io:format("~p Around ring ~p times ~n", [now(), Val]);
logOnNth(_Val, _Nth) -> void.

%% Start timer listener process, return Pid of timer listener
startTimer() ->
    spawn(fun() -> timerOff() end).

timerOff() ->
    receive
	cancel -> void;
	start -> timerOn(now())
    end.

timerOn(Start) ->
    receive
	cancel -> {timer,aborted};
	stop ->
	    End=now(),
	    io:format("Start=~p Stop=~p Elapsed=~p~n", [Start,End,diff(Start,End)]),
	    timerOff()
    end.

diff(Start, End) ->
    {_,StartSeconds,StartMicros} = Start,
    {_,EndSeconds,EndMicros} = End,
    ((EndSeconds*1000000) + EndMicros) - ((StartSeconds*1000000) + StartMicros).



main() ->
    T = startTimer(),
    R = startRing(100, T),
    R ! start,
    receive _ -> true end.
   

