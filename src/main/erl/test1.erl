
-module(test1).

-export([foo/1,bar/2,gen_dec_postponed_decs/2]).

foo(X) -> fun(A0) -> A0+X end.
bar(Fun,A1) -> Fun(A1).


gen_dec_postponed_decs(DecObj,[{_Cname,{FirstPFN,PFNList},Term,TmpTerm,_Tag,OptOrMand}|Rest]) ->
%    asn1ct_name:new(term),
    asn1ct_name:new(tmpterm),
    asn1ct_name:new(reason),

    emit({"{",Term,", _, _} = ",nl}),
    N = case OptOrMand of
            mandatory -> 0;
            'OPTIONAL' ->
                emit_opt_or_mand_check(asn1_NOVALUE,TmpTerm),
                6;
            {'DEFAULT',Val} ->
                emit_opt_or_mand_check(Val,TmpTerm),
                6
        end,
    emit({indent(N+3),"case (catch ",DecObj,"(",{asis,FirstPFN},
%         ", ",TmpTerm,", ", {asis,Tag},", ",{asis,PFNList},")) of",nl}),
          ", ",TmpTerm,", [], ",{asis,PFNList},")) of",nl}),
    emit({indent(N+6),"{'EXIT', ",{curr,reason},"} ->",nl}),
    emit({indent(N+9),"exit({'Type not compatible with table constraint',",
          {curr,reason},"});",nl}),
    emit({indent(N+6),{curr,tmpterm}," ->",nl}),
    emit({indent(N+9),{curr,tmpterm},nl}),

    case OptOrMand of
        mandatory -> emit([indent(N+3),"end,",nl]);
        _ ->
            emit([indent(N+3),"end",nl,
                  indent(3),"end,",nl])
    end,
%    emit({indent(3),"end,",nl}),
    gen_dec_postponed_decs(DecObj,Rest).



indent(_) -> [].
emit(_) -> [].
emit_opt_or_mand_check(_,_) -> [].
