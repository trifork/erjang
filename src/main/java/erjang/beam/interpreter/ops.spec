
%class Insn()
# label L: ignore

K_return:
	if (true) return reg[0];
send:
	//TODO
remove_message:
	//TODO
timeout:
	//TODO
if_end:
	//TODO
int_code_end:
	{}
fclearerror:
	//TODO
bs_init_writable:
	//TODO

##########==========     ERROR REPORTING    	  ==========##########

%class AAI(a1:A, a2:A, i3:I)
func_info mod fun arity:
	if (true) {System.err.println("INT| func_info: "+GET(mod)+":"+GET(fun)+"/"+GET(arity)); return ERT.func_info((EAtom)GET(mod), (EAtom)GET(fun), xregsSeq(reg, GET(arity)));}

##########==========        ALLOCATION      	  ==========##########

%class I(i1:I)
allocate slots:
	stack = ensureCapacity(stack, sp+GET(slots), sp); sp += GET(slots);

deallocate slots:
	sp -= GET(slots);

%class II(i1:I, i2:I)
allocate_zero slots _live:
	stack = ensureCapacity(stack, sp+GET(slots), sp); sp += GET(slots);

%class WI(al:W, i2:I)
test_heap alloc_size _live:
	{}

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
	SET(dst, ((ETuple)GET(src)).elm(GET(pos)));

%class ID(i1:I dest:D)
put_tuple size dst: encoder_side_effect(tuple_pos=0;)
	SET(dst, curtuple = ETuple.make(GET(size)));

%class S(src:S)
put src: encode(++tuple_pos)(index)
	curtuple.set(GET(index), GET(src));


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
	if (GET(arg).testTuple() == null || ((ETuple)GET(arg)).arity() == GET(arity)) GOTO(lbl);

%class LSS(label:L, src1:S, src2:S)
is_eq_exact lbl a b:
	if (! GET(a).equalsExactly(GET(b))) GOTO(lbl);

is_ne_exact lbl a b:
	if (GET(a).equalsExactly(GET(b))) GOTO(lbl);

is_eq lbl a b:
	if (GET(a).compareTo(GET(b)) != 0) GOTO(lbl);

is_ne lbl a b:
	if (GET(a).compareTo(GET(b)) == 0) GOTO(lbl);
#	src1.equals(src2) => GOTO(lbl);
##	(src1 instanceof Literal && src2 instanceof Literal && !src1.equals(src2)) => {}

is_lt lbl a b:
	if (GET(a).compareTo(GET(b)) < 0) GOTO(lbl);

is_ge lbl a b:
	if (GET(a).compareTo(GET(b)) >= 0) GOTO(lbl);

%class Select(src:S jumpTable:J defaultLabel:L)
select_val src table lbl:
	TABLEJUMP(table, GET(src), GET_PC(lbl));

##########==========       FUNCTION CALLS   	  ==========##########

%class IL(i1:I label:L)
call_only keep lbl:
	GOTO(lbl);

call keep lbl:
	proc.stack=stack; proc.sp=sp; reg[0] = invoke(proc, reg, GET(keep), GET_PC(lbl));

%class IE(i1:I ext_fun:E)
call_ext_only _ extfun:
	proc.stack=stack; proc.sp=sp; stack=null; reg[0] = GET(extfun).invoke(proc, xregsArray(reg, GET(extfun).arity())); stack=proc.stack;

call_ext _ extfun:
	proc.stack=stack; proc.sp=sp; stack=null; reg[0] = GET(extfun).invoke(proc, xregsArray(reg, GET(extfun).arity())); stack=proc.stack;

##########==========             BIFS       	  ==========##########

%class GcBif(ext_fun:E args[0]:S args[1]:S dest:D label:L)

gc_bif2 bif arg1 arg2 dest onFail:
	{System.err.println("INTP| invoking bif "+GET(bif)+" with "+GET(arg1)+","+GET(arg2)); EObject tmp = GET(bif).invoke(proc, new EObject[]{GET(arg1), GET(arg2)}); if (tmp==null) GOTO(onFail); SET(dest, tmp);}
# TODO: Streamline these calls - e.g. cast to EFun2 instead of creating array


##########==========      EXCEPTION HANDLING	  ==========##########

#%class YL(y:y, label:L)
#K_catch y lbl:
#	SET(y, makeExceptionHandler(sp,exh));
#exh = GET(lbl);

##########==========       FLOATING-POINT    	  ==========##########
