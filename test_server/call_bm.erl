-module(call_bm).
-export([benchmarks/0]).
-export([local_call/1,external_call/1,fun_call/1,apply_fun/1,apply_mfa/1,
	 atom_apply_mfa/1,select/1]).
-export([foo/0,enc_foo/0]).

benchmarks() ->
    {400000,[local_call,external_call,fun_call,apply_fun,
	     apply_mfa,atom_apply_mfa,select]}.

-define(rep5(X), X, X, X, X, X).
-define(rep10(X), ?rep5(X), ?rep5(X)).
-define(rep20(X), ?rep10(X), ?rep10(X)).

local_call(0) ->
    ok;
local_call(Iter) ->
    ?rep20(foo()),
    local_call(Iter-1).

external_call(0) ->
    ok;
external_call(Iter) ->
    ?rep20(?MODULE:foo()),
    external_call(Iter-1).

fun_call(Iter) ->
    fun_call(Iter, fun() -> ok end).
fun_call(0, Fun) ->
    ok;
fun_call(Iter, Fun) ->
    ?rep20(Fun()),
    fun_call(Iter-1, Fun).

apply_fun(Iter) ->
    apply_fun(Iter, fun() -> ok end).
apply_fun(0, Fun) ->
    ok;
apply_fun(Iter, Fun) ->
    ?rep20(apply(Fun, [])),
    apply_fun(Iter-1, Fun).

apply_mfa(0) ->
    ok;
apply_mfa(Iter) ->
    ?rep20(apply(?MODULE, foo, [])),
    apply_mfa(Iter-1).

atom_apply_mfa(Iter) ->
    atom_apply_mfa(Iter, foo).

atom_apply_mfa(0, Type) -> ok;
atom_apply_mfa(Iter, Type) ->
%%    ?rep20(apply(?MODULE, list_to_atom(lists:concat(['f',Type])), [])),
    ?rep20(apply(?MODULE, list_to_atom("enc_" ++ atom_to_list(Type)), [])),
    atom_apply_mfa(Iter-1, Type).

select(0) -> ok;
select(Iter) ->
    ?rep20(select1(foo)),
    select(Iter-1).

foo() -> ok.
enc_foo() -> ok.
    
select1(a0) -> ?MODULE:a0();
select1(a1) -> ?MODULE:a1();
select1(a2) -> ?MODULE:a2();
select1(a3) -> ?MODULE:a3();
select1(a4) -> ?MODULE:a4();
select1(a5) -> ?MODULE:a5();
select1(a6) -> ?MODULE:a6();
select1(a7) -> ?MODULE:a7();
select1(a8) -> ?MODULE:a8();
select1(a9) -> ?MODULE:a9();
select1(a10) -> ?MODULE:a10();
select1(a11) -> ?MODULE:a11();
select1(a12) -> ?MODULE:a12();
select1(a13) -> ?MODULE:a13();
select1(a14) -> ?MODULE:a14();
select1(a15) -> ?MODULE:a15();
select1(a16) -> ?MODULE:a16();
select1(a17) -> ?MODULE:a17();
select1(a18) -> ?MODULE:a18();
select1(a19) -> ?MODULE:a19();
select1(a20) -> ?MODULE:a20();
select1(a21) -> ?MODULE:a21();
select1(a22) -> ?MODULE:a22();
select1(a23) -> ?MODULE:a23();
select1(a24) -> ?MODULE:a24();
select1(a25) -> ?MODULE:a25();
select1(a26) -> ?MODULE:a26();
select1(a27) -> ?MODULE:a27();
select1(a28) -> ?MODULE:a28();
select1(a29) -> ?MODULE:a29();
select1(a30) -> ?MODULE:a30();
select1(a31) -> ?MODULE:a31();
select1(a32) -> ?MODULE:a32();
select1(a33) -> ?MODULE:a33();
select1(a34) -> ?MODULE:a34();
select1(a35) -> ?MODULE:a35();
select1(a36) -> ?MODULE:a36();
select1(a37) -> ?MODULE:a37();
select1(a38) -> ?MODULE:a38();
select1(a39) -> ?MODULE:a39();
select1(a40) -> ?MODULE:a40();
select1(a41) -> ?MODULE:a41();
select1(a42) -> ?MODULE:a42();
select1(a43) -> ?MODULE:a43();
select1(a44) -> ?MODULE:a44();
select1(a45) -> ?MODULE:a45();
select1(a46) -> ?MODULE:a46();
select1(a47) -> ?MODULE:a47();
select1(a48) -> ?MODULE:a48();
select1(a49) -> ?MODULE:a49();
select1(foo) -> foo().
