#!/bin/sh

java -server -cp erjang-0.2.jar -Derjpath=./src/main/erl/preloaded/ebin:src/main/erl erjang.Erj fib:main
