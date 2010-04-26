#!/bin/sh
erlc -o ebin src/*.erl
erl -pa ebin -noshell -s sample main -s erlang halt
