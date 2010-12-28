
%class Insn()
# label L: ignore

K_return:
	PRE_CALL(); return reg[0];
send:
	ERT.send(proc, reg[0], reg[1]);

remove_message:
	ERT.remove_message(proc);

int_code_end:
 	{}

##########==========     ERROR REPORTING    	  ==========##########

%class Insn()
if_end:
	return ERT.if_end();

%class AAI(a1:A, a2:A, i3:I)
func_info mod fun arity:
	{PRE_CALL(); return ERT.func_info((EAtom)GET(mod), (EAtom)GET(fun), REGS_AS_SEQ(GET(arity)));}

%class S(src:S)
badmatch src:
	/*PRE_CALL();*/ return ERT.badmatch(GET(src));

case_end src:
	/*PRE_CALL();*/ return ERT.case_end(GET(src));

try_case_end src:
	/*PRE_CALL();*/ return ERT.try_case_end(GET(src));

##########==========        ALLOCATION      	  ==========##########

%class I(i1:I)
allocate slots:
	STACK_ALLOC(GET(slots));

deallocate slots:
	STACK_DEALLOC(GET(slots));

%class II(i1:I, i2:I)
allocate_zero slots _live:
	STACK_ALLOC(GET(slots));

%class IWI(i1:I, i2:W, i3:I)
allocate_heap      stacksize _heapsize _live:
	STACK_ALLOC(GET(stacksize));
allocate_heap_zero stacksize _heapsize _live:
	STACK_ALLOC(GET(stacksize));


%class WI(al:W, i2:I)
test_heap alloc_size _live:
	{}

%class D(dest:D)
init dest:
	SET(dest, null);

%class II(i1:I i2:I)
trim amount remaining:
	STACK_DEALLOC(GET(amount));

##########==========  MOVE/CONSTRUCT/DECONSTRUCT  ==========##########

%class SD(src:S dest:D)
move src dst:
	SET(dst, GET(src));
#	src.equals(dst) => {}

%class SSD(src1:S src2:S dest:D)
put_list h t dst:
	SET(dst, ERT.cons(GET(h), GET(t)));

%class SDD(src:S dest1:D dest2:D)
get_list src h t:
        {ECons cons = GET(src).testNonEmptyList(); SET(h, cons.head()); SET(t, cons.tail());}

%class SID(src:S i:I dest:D)
get_tuple_element src pos dst:
	SET(dst, ((ETuple)GET(src)).elm(1+GET(pos)));

%class ID(i1:I dest:D)
put_tuple size dst: encoder_side_effect(tuple_pos=0;)
	SET(dst, curtuple = ETuple.make(GET(size)));

%class S(src:S)
put src: encode(++tuple_pos)(index)
	curtuple.set(GET(index), GET(src));

%class SDI(src:S dest:D i:I)
set_tuple_element src dest index:
        ((ETuple)GET(dest)).set(GET(index)+1, GET(src));



##########==========  TESTS & CONTROL FLOW	  ==========##########
%class L(label:L)
jump lbl:
	GOTO(lbl);

%class LD(label:L, dest:D)
is_integer lbl arg:
	if (GET(arg).testInteger() 	   == null) GOTO(lbl);
is_float lbl arg:
	if (GET(arg).testFloat()   	   == null) GOTO(lbl);
is_number lbl arg:
	if (GET(arg).testNumber()  	   == null) GOTO(lbl);
is_atom lbl arg:
	if (GET(arg).testAtom()    	   == null) GOTO(lbl);
is_pid lbl arg:
	if (GET(arg).testPID() 		   == null) GOTO(lbl);
is_reference lbl arg:
	if (GET(arg).testReference() 	   == null) GOTO(lbl);
is_port lbl arg:
	if (GET(arg).testPort() 	   == null) GOTO(lbl);
is_nil lbl arg:
	if (GET(arg).testNil() 		   == null) GOTO(lbl);
is_binary lbl arg:
	if (GET(arg).testBinary() 	   == null) GOTO(lbl);
is_list lbl arg:
	if (GET(arg).testCons() 	   == null) GOTO(lbl);
is_nonempty_list lbl arg:
	if (GET(arg).testNonEmptyList()    == null) GOTO(lbl);
is_tuple lbl arg:
	if (GET(arg).testTuple() 	   == null) GOTO(lbl);
is_function lbl arg:
	if (GET(arg).testFunction() 	   == null) GOTO(lbl);
is_boolean lbl arg:
	if (GET(arg).testBoolean() 	   == null) GOTO(lbl);
is_bitstr lbl arg:
	if (GET(arg).testBitString() 	   == null) GOTO(lbl);

%class LDI(label:L, dest:D, i:I)
test_arity lbl arg arity:
	if (GET(arg).testTuple() == null || ((ETuple)GET(arg)).arity() != GET(arity)) GOTO(lbl);

%class LSS(label:L, src1:S, src2:S)
is_eq_exact lbl a b:
	if (! GET(a).equalsExactly(GET(b))) GOTO(lbl);

is_ne_exact lbl a b:
	if (GET(a).equalsExactly(GET(b))) GOTO(lbl);

is_eq lbl a b:
	if (GET(a).erlangCompareTo(GET(b)) != 0) GOTO(lbl);

is_ne lbl a b:
	if (GET(a).erlangCompareTo(GET(b)) == 0) GOTO(lbl);
#	src1.equals(src2) => GOTO(lbl);
##	(src1 instanceof Literal && src2 instanceof Literal && !src1.equals(src2)) => {}

is_lt lbl a b:
	if (GET(a).erlangCompareTo(GET(b)) >= 0) GOTO(lbl);

is_ge lbl a b:
	if (GET(a).erlangCompareTo(GET(b)) < 0) GOTO(lbl);

%class LDS(label:L, dest:D, src:S)
is_function2 lbl subject arity:
	if (GET(subject).testFunction2(GET(arity).asInt()) == null) GOTO(lbl);

%class Select(src:S jumpTable:JV defaultLabel:L)
select_val src table lbl:
	TABLEJUMP(table, GET(src), GET_PC(lbl));

%class Select(src:S jumpTable:JA defaultLabel:L)
select_tuple_arity src table lbl:
	{ETuple tuple_val = GET(src).testTuple(); if (tuple_val == null) GOTO(lbl); else {int arity=tuple_val.arity(); TABLEJUMP(table, arity, GET_PC(lbl));}}

##########==========       FUNCTION CALLS   	  ==========##########

%class IL(i1:I label:L)
call_only keep lbl:
	GOTO(lbl);

call keep lbl:
	PRE_CALL(); reg[0] = LOCAL_CALL(GET(keep), GET_PC(lbl)); POST_CALL();

%class IE(i1:I ext_fun:E)
call_ext _ extfun:
	PRE_CALL(); reg[0] = GET(extfun).invoke(proc, reg, 0, GET(extfun).arity()); POST_CALL();

call_ext_only _ extfun:
	PRE_CALL(); return GET(extfun).invoke(proc, reg, 0, GET(extfun).arity());


%class IEI(i1:I ext_fun:E i3:I)
call_ext_last arity extfun dealloc:
	STACK_DEALLOC(GET(dealloc)); PRE_CALL(); return GET(extfun).invoke(proc, reg, 0, GET(extfun).arity());

%class I(i1:I)
apply arity:
	PRE_CALL(); int ary=GET(arity); reg[0] = ERT.resolve_fun(reg[ary], reg[ary+1], ary).invoke(proc, reg, 0, ary); POST_CALL();

call_fun arity:
	PRE_CALL(); int ary=GET(arity); reg[0] = ((EFun)reg[ary]).invoke(proc, reg, 0, ary); POST_CALL();

%class II(i1:I i2:I)
apply_last arity dealloc:
	STACK_DEALLOC(GET(dealloc)); int ary = GET(arity); EFun fun = ERT.resolve_fun(reg[ary], reg[ary+1], ary); PRE_CALL(); return fun.invoke(proc, REGS_AS_ARRAY(ary));

%class ILI(i1:I label:L i3:I)
call_last keep lbl dealloc:
	STACK_DEALLOC(GET(dealloc)); GOTO(lbl);

%class F(anon_fun.total_arity:I, anon_fun.free_vars:I, anon_fun.label:IL)
make_fun2 total_arity free_vars label:
	reg[0] = MAKE_CLOSURE(REGS_AS_ARRAY(GET(free_vars)), GET(total_arity)-GET(free_vars), GET_PC(label));

##########==========             BIFS       	  ==========##########

#%class Bif(ext_fun:E args[0]:S args[1]:S dest:D label:L)

%class Bif(ext_fun:E dest:D label:L0)
bif0 bif dest onFail:
	{EObject tmp = GET(bif).invoke(proc, new EObject[]{}); if (IS_GUARD(bif) && tmp==null) GOTO(onFail); SET(dest, tmp);}

%class Bif(ext_fun:EG args[0]:S dest:D label:L)
bif1 bif arg1 dest onFail:
	{EObject tmp = GET(bif).invoke(proc, new EObject[]{GET(arg1)}); if (IS_GUARD(bif) && tmp==null) GOTO(onFail); SET(dest, tmp);}

%class Bif(ext_fun:EG args[0]:S args[1]:S dest:D label:L)
bif2 bif arg1 arg2 dest onFail:
	{EObject tmp = GET(bif).invoke(proc, new EObject[]{GET(arg1), GET(arg2)}); if (IS_GUARD(bif) && tmp==null) GOTO(onFail); SET(dest, tmp);}

%class GcBif(ext_fun:EG args[0]:S dest:D label:L)

gc_bif1 bif arg1 dest onFail:
	{EObject tmp = GET(bif).invoke(proc, new EObject[]{GET(arg1)}); if (tmp==null) GOTO(onFail); SET(dest, tmp);}

%class GcBif(ext_fun:EG args[0]:S args[1]:S dest:D label:L)

gc_bif2 bif arg1 arg2 dest onFail:
	{EObject tmp = GET(bif).invoke(proc, new EObject[]{GET(arg1), GET(arg2)}); if (tmp==null) GOTO(onFail); SET(dest, tmp);}
# TODO: Streamline these calls - e.g. cast to EFun2 instead of creating array


##########==========           RECEIVE            ==========##########
%class LD(label:L, dest:D)
loop_rec label dest:
	EObject tmp = ERT.loop_rec(proc); if (tmp==null) GOTO(label); else SET(dest, tmp);

%class L(label:L)
wait label:
	ERT.wait(proc); GOTO(label);

loop_rec_end label:
	ERT.loop_rec_end(proc); GOTO(label);

%class LS(label:L src:S)
wait_timeout label millis:
	if (ERT.wait_timeout(proc, GET(millis))) GOTO(label);

%class Insn()
timeout:
	ERT.timeout(proc);

%class L(label:L)
#NOPs for now:
recv_mark lbl:
	{}
recv_set lbl:
	{}


##########==========       MATCHING OF BINARIES    ==========##########
%class LDIID(label:L dest:D i3:I i4:I dest5:D)
bs_start_match2 failLabel src _ slots dest:
	EObject tmp = EBinMatchState.bs_start_match2(GET(src), GET(slots)); if (tmp==null) GOTO(failLabel); else SET(dest, tmp);

bs_get_utf8 failLabel src _ flags dest:
	int chr = ((EBinMatchState)(GET(src))).bs_get_utf8(GET(flags)); if (chr < 0) GOTO(failLabel); else SET(dest, ERT.box(chr));
bs_get_utf16 failLabel src _ flags dest:
	int chr = ((EBinMatchState)(GET(src))).bs_get_utf16(GET(flags)); if (chr < 0) GOTO(failLabel); else SET(dest, ERT.box(chr));
bs_get_utf32 failLabel src _ flags dest:
	int chr = ((EBinMatchState)(GET(src))).bs_get_utf32(GET(flags)); if (chr < 0) GOTO(failLabel); else SET(dest, ERT.box(chr));


%class LDBi(label:L dest:D bin:c)
bs_match_string failLabel src string:
	if (((EBinMatchState)(GET(src))).bs_match_string((EBitString)GET(string)) == null) GOTO(failLabel);


%class LDISIID(label:L dest:D i3:I src4:S i5:I i6:I dest7:D)
bs_get_integer2 failLabel src _keep bits unit flags dest:
	EObject tmp = ((EBinMatchState)(GET(src))).bs_get_integer2(((ESmall)GET(bits)).intValue(), GET(unit), GET(flags)); if (tmp == null) GOTO(failLabel); else SET(dest, tmp);

bs_get_float2 failLabel src _keep bits unit flags dest:
	EObject tmp = ((EBinMatchState)(GET(src))).bs_get_float2(((ESmall)GET(bits)).intValue(), GET(unit), GET(flags)); if (tmp == null) GOTO(failLabel); else SET(dest, tmp);

bs_get_binary2 failLabel ms _keep bits unit flags dest:
	EObject tmp = ((EBinMatchState)(GET(ms))).bs_get_binary2(GET(bits), GET(flags)); if (tmp==null) GOTO(failLabel); else SET(dest, tmp);

%class LDI(label:L, dest:D, i:I)
bs_test_tail2 failLabel ms bits_left:
	if (! ((EBinMatchState)(GET(ms))).bs_test_tail2(GET(bits_left))) GOTO(failLabel);

bs_test_unit failLabel ms unit:
	if (! ((EBinMatchState)(GET(ms))).bs_test_unit(GET(unit))) GOTO(failLabel);

%class LDII(label:L dest:D i3:I i4:I)
bs_skip_utf8 failLabel ms _dummy flags:
	if (! ((EBinMatchState)(GET(ms))).bs_skip_utf8(GET(flags))) GOTO(failLabel);
bs_skip_utf16 failLabel ms _dummy flags:
	if (! ((EBinMatchState)(GET(ms))).bs_skip_utf16(GET(flags))) GOTO(failLabel);
bs_skip_utf32 failLabel ms _dummy flags:
	if (! ((EBinMatchState)(GET(ms))).bs_skip_utf32(GET(flags))) GOTO(failLabel);

%class LDSII(label:L dest:D src3:S i4:I i5:I)
bs_skip_bits2 failLabel ms bits unit flags:
	EObject tmp = ((EBinMatchState)(GET(ms))).bs_skip_bits2((GET(bits)), GET(unit), GET(flags)); if (tmp==null) GOTO(failLabel);

%class LSD(label:L src:S dest:D)
bs_utf8_size failLabel value dest:
	SET(dest, EBitStringBuilder.bs_utf8_size(GET(value))); // Label unused??
bs_utf16_size failLabel value dest:
	SET(dest, EBitStringBuilder.bs_utf16_size(GET(value))); // Label unused??


%class DI(dest:D i2:I)
bs_save2 ms pos:
	EObject ms = GET(ms); int pos = GET(pos); if (pos==-1) EBinMatchState.bs_save2_start(ms); else EBinMatchState.bs_save2(ms, pos);

bs_restore2 ms pos:
	EObject ms = GET(ms); int pos = GET(pos); if (pos==-1) EBinMatchState.bs_restore2_start(ms); else EBinMatchState.bs_restore2(ms, pos);

##########==========    CONSTRUCTION OF BINARIES  ==========##########

%class Insn()
bs_init_writable:
	bit_string_builder = EBitStringBuilder.bs_init_writable(reg[0]); reg[0] = bit_string_builder.bitstring();

%class LSIIID(label:L src2:S i3:I i4:I i5:I dest:D)
bs_init2 onFail size _unit _keep flags dest:
	bit_string_builder = ERT.bs_init(((ESmall)GET(size)).intValue(), GET(flags)); SET(dest, bit_string_builder.bitstring());
# TODO: use unit (is it i3 or i4, btw?)
bs_init_bits onFail size _unit _keep flags dest:
	bit_string_builder = ERT.bs_initBits(((ESmall)GET(size)).intValue(), GET(flags)); SET(dest, bit_string_builder.bitstring());

%class By(bin:c)
bs_put_string value:
	bit_string_builder.put_string((EString)GET(value));

%class LSIIS(label:L src2:S i3:I i4:I src5:S)
bs_put_integer onFail size unit flags value:
	bit_string_builder.put_integer(GET(value), GET(unit) * ((ESmall)GET(size)).intValue(), GET(flags));

bs_put_binary onFail size unit flags value:
	EInteger size = GET(size).testInteger(); int actualSize = (size==null)? -1 : size.intValue() * GET(unit); bit_string_builder.put_bitstring(GET(value), actualSize, GET(flags));

bs_put_float onFail size unit flags value:
	bit_string_builder.put_float(GET(value), GET(unit) * ((ESmall)GET(size)).intValue(), GET(flags));

%class LIS(label:L i2:I src:S)
bs_put_utf8 onFail flags value:
	bit_string_builder.put_utf8(GET(value), GET(flags));
bs_put_utf16 onFail flags value:
	bit_string_builder.put_utf16(GET(value), GET(flags));
bs_put_utf32 onFail flags value:
	bit_string_builder.put_utf32(GET(value), GET(flags));

%class LSSID(label:L0 src1:S src2:S i3:I dest:D)
bs_add onFail x y yunit dest:
	try {int xval = ERT.unboxToInt(GET(x)), yval = ERT.unboxToInt(GET(y)); SET(dest, ERT.box(xval + yval * GET(yunit)));} catch (Exception e) {GOTO(onFail);}

%class BSAppend(label:L src2:S i3:I i4:I i5:I src6:S i7:I dest8:D)
bs_append onFail extra_size dummy3 dummy4 unit src flags dest:
	bit_string_builder = EBitStringBuilder.bs_append(GET(src), ERT.unboxToInt(GET(extra_size)), GET(unit), GET(flags)); SET(dest, bit_string_builder.bitstring());

%class BSPrivateAppend(label:L src2:S i3:I src4:S i5:I dest:D)
bs_private_append onFail extra_size unit src flags dest:
	bit_string_builder = EBitStringBuilder.bs_append(GET(src), ERT.unboxToInt(GET(extra_size)), GET(unit), GET(flags)); SET(dest, bit_string_builder.bitstring());

%class D(dest:D)
bs_context_to_binary srcdest:
	SET(srcdest, EBinMatchState.bs_context_to_binary(GET(srcdest)));

##########==========      EXCEPTION HANDLING	  ==========##########

%class YL(y:y, label:L)
K_catch y lbl:
	SET(y, exh); exh = MAKE_EXH_LINK(GET_PC(lbl), false);

K_try y lbl:
	SET(y, exh); exh = MAKE_EXH_LINK(GET_PC(lbl), true);

%class Y(y:y)
catch_end y:
	RESTORE_EXH(GET(y));

try_end y:
	RESTORE_EXH(GET(y));

try_case y:
	RESTORE_EXH(GET(y)); {/* Exception deconstruction done by TryExceptionHandler. */}

%class SS(src1:S src2:S)
raise value trace:
	reg[0] = ERT.raise(GET(value), GET(trace));

##########==========       FLOATING-POINT    	  ==========##########
# TODO: Room for improvement in this section - by not boxing the fregs.

%class Insn()
fclearerror:
	{}

%class L(label:L)
#fcheckerror: TODO - actually do something?
fcheckerror lbl:
	{}

%class SD(src:SF dest:DF)
fmove src dst:
	SET(dst, ErlBif.float$n((EObject)GET(src)));

fconv src dst:
	SET(dst, ErlBif.float$n((EObject)GET(src)));


%class LSSD(label:L, src1:f, src2:f, dest:f)
fadd lbl a b dst:
	SET(dst, ERT.box(ErlBif.fadd(GET(a).value, GET(b).value)));

fsub lbl a b dst:
	SET(dst, ERT.box(ErlBif.fsub(GET(a).value, GET(b).value)));

fmul lbl a b dst:
	SET(dst, ERT.box(ErlBif.fmul(GET(a).value, GET(b).value)));

fdiv lbl a b dst:
	SET(dst, ERT.box(ErlBif.fdiv(GET(a).value, GET(b).value)));

