-module(pdict_tests).

-export([test/0]).

test() ->
    Pid1 = spawn(fun()->ok end),
    Pid2 = spawn(fun()->ok end),
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Keys = [atom, [list], [], [unpure|list], 123.0, float(123), 90, -90, atom, [], self(), Pid1, Pid2, Ref1, Ref2],
    Values = [1, 2, 3, atom, [], [more|list], 12.3, Ref1, Pid1, 10, 11, 12, 13, 14, 15],

    R0 = [get(K) || K <- Keys],
    R1 = [put(K,V) || {K,V} <- lists_zip(Keys, Values)],
    R2 = [put(K,V) || {K,V} <- lists_zip(Keys, Values)], % Put same.
    R3 = [put(K,V) || {K,V} <- lists_zip(Keys++[x], [x]++Values)], % Put new.
    R4 = [erase(K) || K<-every_other(Keys)],
    R5 = [get(K) || K <- Keys],
    %% TODO: get/0, erase/0.

    [[portable(X,self(),Pid1,Pid2,Ref1,Ref2) || X<-L]
     || L <-[R0, R1, R2, R3, R4, R5]].

lists_zip([], []) -> [];
lists_zip([H1|T1], [H2|T2]) -> [{H1,H2} | lists_zip(T1,T2)].

every_other([]) -> [];
every_other([H]) -> [H];
every_other([H, _ | T]) -> [H | every_other(T)].

portable(Self, Self,_,_,_,_) -> self;
portable(Pid1, _,Pid1,_,_,_) -> pid1;
portable(Pid2, _,_,Pid2,_,_) -> pid2;
portable(Ref1, _,_,_,Ref1,_) -> ref1;
portable(Ref2, _,_,_,_,Ref2) -> ref2;
portable(X,    _,_,_,_,_) -> X.
