@echo off

IF "x%OTPROOT%" == "x" SET OTPROOT="C:/Program Files/erl5.7.5"

java -ea -cp erjang-0.1.jar ^
    -Derj.threads=1 ^
    -Derjpath=%OTP_ERTS_EBIN% erjang.OTPMain ^
    -root %OTPROOT% ^
    -progname erl ^
    -- ^
    -home %HOME% ^
    %1 %2 %3 %4 %5 %6 %7 %8 %9
