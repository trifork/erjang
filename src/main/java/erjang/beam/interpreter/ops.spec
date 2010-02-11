
# %class Insn()

# label L: ignore

%class L(lbl:L)
jump lbl:
	GOTO(lbl);

%class SD(src:S dst:D)
move src dst:
	SET(dst, GET(src));
# 
# %class ID(i1:I dest:D)
# put_tuple size dst:
# 	SET(dst, curtuple = ETuple.make(size)); tuplepos=0;
# 
# %class S(src:S)
# put src: encode(++tuplepos)(index)
# 	SET(curtuple).set(index, GET(src));
# 
# 
%class LSS(label:L src1:S src2:S)
is_eq lbl src1 src2:
	if (GET(src1).equals(GET(src2))) GOTO(lbl);
# 
# is_ne lbl src1 src2:
# 	if (! (GET(src1).equals(GET(src2)))) GOTO(lbl);
# 
# 
# %class AAI(a1:A a2:A i1:I)
# func_info mod fun arity:
# 	ERT.func_info((EAtom)GET(mod), (EAtom)GET(fun), arity);
# 
# 
# %class IL(i1:I lbl:L)
# call_only keep lbl:
# 	GOTO(lbl);
