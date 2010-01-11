#!/bin/sh

java -server -d64 -da -cp erjang-0.1.jar -Derjpath=./src/main/erl/preloaded/ebin:src/main/erl erjang.Erj ring:main
