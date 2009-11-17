
-module(test1).

-export([foo/1,bar/1]).

foo(X) when (X/2.0)==3.3 ->
	true;
foo(X) -> 
	2.0*X.


bar(A) ->
  case catch foo:bar(A) of
    A -> true;
    _ -> false
  end.

