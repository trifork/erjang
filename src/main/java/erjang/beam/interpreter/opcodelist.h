// Assumes definitions of:
// TEMPLATE(opcode, code)
// LIT(nr), XREG(nr), YREG(nr)
// NEXTVAL

#define FORALL_SRC(F)	 	F(XREG) F(YREG) F(LIT)
#define FORALL_SRC2(F,x) 	F(x,XREG) F(x,YREG) F(x,LIT)
#define FORALL_DST2(F,x)	F(x,XREG) F(x,YREG)

//---------- Format 'S' ----------
#define PUT(Src)	TEMPLATE(put_##Src, EObject tmp = Src(NEXTVAL); tupleVal.set(tuplePos++, tmp);)
#define BADMATCH(Src)	TEMPLATE(badmatch_##Src, EObject tmp = Src(NEXTVAL); return ERT.badmatch(tmp);)
#define CASE_END(Src)	TEMPLATE(case_end_##Src, EObject tmp = Src(NEXTVAL); /*ToDo*/)
#define TEY_CASE_END(Src) TEMPLATE(try_case_end_##Src, EObject tmp = Src(NEXTVAL); /*ToDo*/)

//---------- Format 'SD' ----------
#define MOVE(Src,Dst)	TEMPLATE(move_##Src##_##Dst, EObject tmp = Src(NEXTVAL); Dst(NEXTVAL) = tmp;)

#define FORALL_OPS(F,x) \
        FORALL_SRC(PUT) \
        FORALL_SRC(BADMATCH) \
	FORALL_SRC2(FORALL_DST2, MOVE)
