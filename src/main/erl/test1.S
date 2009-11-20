{module, test1}.  %% version = 0

{exports, [{bar,2},
           {foo,1},
           {gen_dec_postponed_decs,2},
           {module_info,0},
           {module_info,1}]}.

{attributes, []}.

{labels, 26}.


{function, foo, 1, 2}.
  {label,1}.
    {func_info,{atom,test1},{atom,foo},1}.
  {label,2}.
    {make_fun2,{f,25},0,3272456,1}.
    return.


{function, bar, 2, 4}.
  {label,3}.
    {func_info,{atom,test1},{atom,bar},2}.
  {label,4}.
    {allocate,0,2}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_fun,1}.
    {deallocate,0}.
    return.


{function, gen_dec_postponed_decs, 2, 6}.
  {label,5}.
    {func_info,{atom,test1},{atom,gen_dec_postponed_decs},2}.
  {label,6}.
    {test,is_nonempty_list,{f,5},[{x,1}]}.
    {get_list,{x,1},{x,2},{x,3}}.
    {test,is_tuple,{f,5},[{x,2}]}.
    {test,test_arity,{f,5},[{x,2},6]}.
    {get_tuple_element,{x,2},1,{x,4}}.
    {get_tuple_element,{x,2},2,{x,5}}.
    {get_tuple_element,{x,2},3,{x,6}}.
    {get_tuple_element,{x,2},5,{x,7}}.
    {test,is_tuple,{f,5},[{x,4}]}.
    {test,test_arity,{f,5},[{x,4},2]}.
    {allocate,7,8}.
    {get_tuple_element,{x,4},0,{y,1}}.
    {get_tuple_element,{x,4},1,{y,2}}.
    {move,{x,0},{y,5}}.
    {move,{atom,tmpterm},{x,0}}.
    {move,{x,5},{y,0}}.
    {move,{x,6},{y,3}}.
    {move,{x,7},{y,4}}.
    {move,{x,3},{y,6}}.
    {call_ext,1,{extfunc,asn1ct_name,new,1}}.
    {move,{atom,reason},{x,0}}.
    {call_ext,1,{extfunc,asn1ct_name,new,1}}.
    {test_heap,5,0}.
    {put_tuple,4,{x,0}}.
    {put,{literal,"{"}}.
    {put,{y,0}}.
    {put,{literal,", _, _} = "}}.
    {put,{atom,nl}}.
    {kill,{y,0}}.
    {call,1,{f,17}}.
    {test,is_tuple,{f,7},[{y,4}]}.
    {test,test_arity,{f,13},[{y,4},2]}.
    {get_tuple_element,{y,4},0,{x,0}}.
    {get_tuple_element,{y,4},1,{x,1}}.
    {test,is_eq_exact,{f,13},[{x,0},{atom,'DEFAULT'}]}.
    {move,{x,1},{x,0}}.
    {move,{y,3},{x,1}}.
    {call,2,{f,19}}.
    {move,{integer,6},{x,0}}.
    {jump,{f,10}}.
  {label,7}.
    {test,is_atom,{f,13},[{y,4}]}.
    {select_val,{y,4},
                {f,13},
                {list,[{atom,'OPTIONAL'},{f,8},{atom,mandatory},{f,9}]}}.
  {label,8}.
    {move,{y,3},{x,1}}.
    {move,{atom,asn1_NOVALUE},{x,0}}.
    {call,2,{f,19}}.
    {move,{integer,6},{x,0}}.
    {jump,{f,10}}.
  {label,9}.
    {move,{integer,0},{x,0}}.
  {label,10}.
    {gc_bif,'+',{f,0},1,[{x,0},{integer,3}],{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,0}}.
    {call,1,{f,15}}.
    {test_heap,18,1}.
    {put_tuple,2,{x,1}}.
    {put,{atom,asis}}.
    {put,{y,1}}.
    {put_tuple,2,{x,2}}.
    {put,{atom,asis}}.
    {put,{y,2}}.
    {put_tuple,11,{x,3}}.
    {put,{x,0}}.
    {put,{literal,"case (catch "}}.
    {put,{y,5}}.
    {put,{literal,"("}}.
    {put,{x,1}}.
    {put,{literal,", "}}.
    {put,{y,3}}.
    {put,{literal,", [], "}}.
    {put,{x,2}}.
    {put,{literal,")) of"}}.
    {put,{atom,nl}}.
    {kill,{y,1}}.
    {kill,{y,2}}.
    {kill,{y,3}}.
    {move,{x,3},{x,0}}.
    {call,1,{f,17}}.
    {gc_bif,'+',{f,0},0,[{y,0},{integer,6}],{x,0}}.
    {call,1,{f,15}}.
    {test_heap,6,1}.
    {put_tuple,5,{x,1}}.
    {put,{x,0}}.
    {put,{literal,"{'EXIT', "}}.
    {put,{literal,{curr,reason}}}.
    {put,{literal,"} ->"}}.
    {put,{atom,nl}}.
    {move,{x,1},{x,0}}.
    {call,1,{f,17}}.
    {gc_bif,'+',{f,0},0,[{y,0},{integer,9}],{x,0}}.
    {call,1,{f,15}}.
    {test_heap,6,1}.
    {put_tuple,5,{x,1}}.
    {put,{x,0}}.
    {put,{literal,"exit({'Type not compatible with table constraint',"}}.
    {put,{literal,{curr,reason}}}.
    {put,{literal,"});"}}.
    {put,{atom,nl}}.
    {move,{x,1},{x,0}}.
    {call,1,{f,17}}.
    {gc_bif,'+',{f,0},0,[{y,0},{integer,6}],{x,0}}.
    {call,1,{f,15}}.
    {test_heap,5,1}.
    {put_tuple,4,{x,1}}.
    {put,{x,0}}.
    {put,{literal,{curr,tmpterm}}}.
    {put,{literal," ->"}}.
    {put,{atom,nl}}.
    {move,{x,1},{x,0}}.
    {call,1,{f,17}}.
    {gc_bif,'+',{f,0},0,[{y,0},{integer,9}],{x,0}}.
    {call,1,{f,15}}.
    {test_heap,4,1}.
    {put_tuple,3,{x,1}}.
    {put,{x,0}}.
    {put,{literal,{curr,tmpterm}}}.
    {put,{atom,nl}}.
    {move,{x,1},{x,0}}.
    {call,1,{f,17}}.
    {test,is_eq_exact,{f,11},[{y,4},{atom,mandatory}]}.
    {gc_bif,'+',{f,0},0,[{y,0},{integer,3}],{x,0}}.
    {kill,{y,0}}.
    {kill,{y,4}}.
    {call,1,{f,15}}.
    {test_heap,2,1}.
    {put_list,{x,0},{literal,["end,",nl]},{x,0}}.
    {call,1,{f,17}}.
    {jump,{f,12}}.
  {label,11}.
    {gc_bif,'+',{f,0},0,[{y,0},{integer,3}],{x,0}}.
    {kill,{y,0}}.
    {kill,{y,4}}.
    {call,1,{f,15}}.
    {move,{x,0},{y,4}}.
    {move,{integer,3},{x,0}}.
    {call,1,{f,15}}.
    {test_heap,8,1}.
    {put_list,{x,0},{literal,["end,",nl]},{x,0}}.
    {put_list,{atom,nl},{x,0},{x,0}}.
    {put_list,{literal,"end"},{x,0},{x,1}}.
    {put_list,{y,4},{x,1},{x,0}}.
    {kill,{y,4}}.
    {call,1,{f,17}}.
  {label,12}.
    {move,{y,6},{x,1}}.
    {move,{y,5},{x,0}}.
    {call_last,2,{f,6},7}.
  {label,13}.
    {case_end,{y,4}}.


{function, indent, 1, 15}.
  {label,14}.
    {func_info,{atom,test1},{atom,indent},1}.
  {label,15}.
    {move,nil,{x,0}}.
    return.


{function, emit, 1, 17}.
  {label,16}.
    {func_info,{atom,test1},{atom,emit},1}.
  {label,17}.
    {move,nil,{x,0}}.
    return.


{function, emit_opt_or_mand_check, 2, 19}.
  {label,18}.
    {func_info,{atom,test1},{atom,emit_opt_or_mand_check},2}.
  {label,19}.
    {move,nil,{x,0}}.
    return.


{function, module_info, 0, 21}.
  {label,20}.
    {func_info,{atom,test1},{atom,module_info},0}.
  {label,21}.
    {move,{atom,test1},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 23}.
  {label,22}.
    {func_info,{atom,test1},{atom,module_info},1}.
  {label,23}.
    {move,{x,0},{x,1}}.
    {move,{atom,test1},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-foo/1-fun-0-', 2, 25}.
  {label,24}.
    {func_info,{atom,test1},{atom,'-foo/1-fun-0-'},2}.
  {label,25}.
    {gc_bif,'+',{f,0},2,[{x,0},{x,1}],{x,0}}.
    return.
