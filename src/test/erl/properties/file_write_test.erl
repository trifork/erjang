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

%%
%% Equivalence tests for written/read Erlang terms.
%%

% ../../../ej -sname erj@mcilroy -pz ~/OSS/triq/ebin/
% c(file_write_test, [{i, "/home/erik/OSS/triq/include"}]), file_write_test:test().

-module(file_write_test).

-include("triq.hrl").

-export([test/0]).

%% ===========================================
%% Property test for file:write_file() + file:consult()
%% ===========================================

prop_file_write_read_identity() ->
    ?FORALL(X, any(),
	    begin
		Org = X,
		ok = file:write_file("data.tmp", io_lib:format("~p.\n", [Org])),
		{ok, [Copy]} = file:consult("data.tmp"),
		if Org /= Copy -> io:format("Diff: here=~p,\n     there=~p~n", [Org,Copy]); true -> ok end,
		Org==Copy
	    end).

%%
%% run the test
%%
test() ->
    triq:check(prop_file_write_read_identity()).

