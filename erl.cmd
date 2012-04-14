@echo off

IF "x%OTPROOT%" == "x" SET OTPROOT="C:/Program Files/erl5.7.5"

java -cp erjang-0.2.jar ^
    erjang.Main ^
    -root %OTPROOT% ^
    %1 %2 %3 %4 %5 %6 %7 %8 %9
