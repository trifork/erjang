-module(jsample).

-export([print/1]).
-import(java, [call/4, get_static/2]).

print(Term) ->
    Out = java:get_static('java.lang.System', 'out'),
    Str = binary_to_list(iolist_to_binary(io_lib:format("~p", [Term]))),
    java:call(Out, 'println', ['java.lang.String'], [Str]).



    
