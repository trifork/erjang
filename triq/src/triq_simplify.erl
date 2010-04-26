%%
%% This file is part of Triq - Trifork QuickCheck
%%
%% Copyright (c) 2010 by Trifork
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%  
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%


-module(triq_simplify).

-include("triq_domain.hrl").

-export([simplify_value/2, simplify_internal/2]).
-import(triq_domain, [generate/2, generates/2, elem_gen/2]).


%%
%% public API looks like this
%%
simplify_value(Dom=#?DOM{simplify=SFun}, Val) ->
    SFun(Dom,Val);

simplify_value(Dom,Val) ->
    simplify_internal(Dom,Val).



%%
%% the meat of the shrinking function
%%
simplify_internal(_, []) -> 
    [];


%%
%% aggregates (tuple or list) are simplified either by 
%% simplifying one of the elements, or ... by removing
%% one of the elements
%%
simplify_internal(AggrDom,Aggr) when is_list(Aggr); is_tuple(Aggr)->
    NewAggr = 
    case random:uniform(2) of
	1 -> remove_any(Aggr);
	2 -> simplify_member(Aggr, AggrDom, random:uniform(len(Aggr)+1))
    end,

    %% if the above did not change anything; 
    %% or the change was illegal according to AggrDom ...
    case (Aggr==NewAggr) or (not generates(AggrDom,NewAggr)) of 
	true -> case random:uniform(2) of
		    1 -> simplify_value(AggrDom,Aggr);
		    2 -> Aggr
		end;
	false -> NewAggr
    end    
    ;



simplify_internal(_,0.0) -> 
    0.0;
simplify_internal(_NumDom,Num) when is_float(Num) ->
    Num/2;


simplify_internal(_,0) -> 
    0;
simplify_internal(_NumDom,Num) when is_integer(Num), Num > 0 ->
    Num-1;
simplify_internal(_NumDom,Num) when is_integer(Num), Num < 0 ->
    Num+1;

simplify_internal(Any,Any) -> Any.




%% remove any element of a list
remove_any(List) when is_list(List) ->
    Len = length(List),

    %%
    %% remove element at idx RemIdx
    %%
    RemIdx = random:uniform(Len),    
    case RemIdx of
	1 -> 
	    lists:sublist(List,2,Len-1);
	Len -> 
	    lists:sublist(List,1,Len-1);
	_ -> 
	    lists:sublist(List,RemIdx-1) ++ lists:sublist(List,RemIdx+1,Len)
    end;
remove_any(Tup) when is_tuple(Tup) ->
    list_to_tuple(remove_any(tuple_to_list(Tup))).

    
%% remove any element of a list
simplify_member(Any, _Dom, 0) -> Any;
simplify_member(List, ListDom, HowMany) when is_list(List) ->
    Len = length(List),

    %%
    %% remove element at idx RemIdx
    %%
    RemIdx = random:uniform(Len),    
    Elm = lists:nth(RemIdx, List),
    ElmDom = elem_gen(RemIdx, ListDom),
    NextList = case RemIdx of
	1 -> 
	    [simplify_value(ElmDom,Elm)] ++ lists:sublist(List,2,Len-1);
	Len -> 
	    lists:sublist(List,1,Len-1) ++ [simplify_value(ElmDom,Elm)];
	_ -> 
	    lists:sublist(List,RemIdx-1) ++ [simplify_value(ElmDom,Elm)] ++ lists:sublist(List,RemIdx+1,Len)
    end,

    simplify_member(NextList, ListDom,HowMany-1);

simplify_member(Tuple, TupleDom, HowMany) when is_tuple(Tuple) ->
    erlang:list_to_tuple(simplify_member(erlang:tuple_to_list(Tuple), TupleDom, HowMany)).
    

len(T) when is_tuple(T) -> erlang:tuple_size(T);
len(T) when is_list(T) -> length(T).

