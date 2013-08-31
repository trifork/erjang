-module(float_bm).
-export([benchmarks/0,float_add/1,float_sub/1,float_mul/1,float_div/1]).

benchmarks() ->
    {200000,[float_add,float_sub,float_mul,float_div]}.

float_add(Iter) ->
    float_add(Iter, 1.1, 3.1416).

float_add(0, A, B) -> ok;
float_add(Iter, A, B) ->
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    float_add(Iter-1, A, B).
    
float_sub(Iter) ->
    float_sub(Iter, 1.1, 3.1416).

float_sub(0, A, B) -> ok;
float_sub(Iter, A, B) ->
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    float_sub(Iter-1, A, B).

float_mul(Iter) ->
    float_mul(Iter, 1.1, 3.1416).

float_mul(0, A, B) -> ok;
float_mul(Iter, A, B) ->
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    float_mul(Iter-1, A, B).

float_div(Iter) ->
    float_div(Iter, 1.1, 3.1416).

float_div(0, A, B) -> ok;
float_div(Iter, A, B) ->
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    float_div(Iter-1, A, B).
