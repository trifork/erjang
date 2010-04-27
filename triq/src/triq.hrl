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

-define(DELAY(X),fun()->X end).
-define(FORCE(X), (X)()).

%% properties
-define(FORALL(X,Gen,Property),
	{'prop:forall', Gen, ??X, fun(X)-> begin Property end end, ??Property}).
-define(IMPLIES(Pre, Property), 
	{'prop:implies', Pre, ??Pre, fun() -> Property end, ??Property}).

%% value domains
-define(LET(X,Gen1,Gen2), 
	triq_domain:glet(Gen1, fun(X)->Gen2 end}).
-define(SIZED(Size,Gen),
	triq_domain:sized(Size,Gen)).


-import(triq_domain, [list/1, tuple/1, int/0, real/0, elements/1, any/0, atom/0]).





								  
