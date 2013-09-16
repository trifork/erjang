#!/bin/sh

egrep ^ERL_NIF_API_FUNC_DECL ../x86_64-apple-darwin12.4.0//usr/include/erl_nif_api_funcs.h | perl -pe 's/ +//g' | awk -F, '{print "_" $2 }'  > needed
nm libjnif.jnilib | grep ' T _enif_' | awk '{ print $3 }' > defined

cat needed defined | sort | uniq -c | sort -n | grep ' 1 ' > missing

wc -l needed
wc -l defined
wc -l missing

