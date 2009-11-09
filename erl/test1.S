{module, test1}.  %% version = 0

{exports, [{foo,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 8}.


{function, foo, 1, 2}.
  {label,1}.
    {func_info,{atom,test1},{atom,foo},1}.
  {label,2}.
    {gc_bif,'/',{f,3},1,[{x,0},{float,2.0}],{x,1}}.
    {test,is_eq,{f,3},[{x,1},{float,3.3}]}.
    {move,{atom,true},{x,0}}.
    return.
  {label,3}.
    {test_heap,{alloc,[{words,0},{floats,1}]},1}.
    {fmove,{float,2.0},{fr,0}}.
    {fconv,{x,0},{fr,1}}.
    fclearerror.
    {bif,fmul,{f,0},[{fr,0},{fr,1}],{fr,1}}.
    {fcheckerror,{f,0}}.
    {fmove,{fr,1},{x,0}}.
    return.


{function, module_info, 0, 5}.
  {label,4}.
    {func_info,{atom,test1},{atom,module_info},0}.
  {label,5}.
    {move,{atom,test1},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 7}.
  {label,6}.
    {func_info,{atom,test1},{atom,module_info},1}.
  {label,7}.
    {move,{x,0},{x,1}}.
    {move,{atom,test1},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
