-module(jsample).

-export([print/1,test/0]).
-import(java, [call/4, get_static/2]).

print(Term) ->
    Out = java:get_static('java.lang.System', 'out'),
    Str = binary_to_list(iolist_to_binary(io_lib:format("~p", [Term]))),
    Out:println(Str).

test() ->
    Map = 'java.util.HashMap':new(),
    Map:put(1, 'foo'),
    Map:put('x', "4"),

    [io:format("key=~p, value=~p~n", [Key,Val])
     || {Key,Val} <- Map].


    
