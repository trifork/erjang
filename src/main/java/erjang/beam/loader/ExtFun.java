package erjang.beam.loader;

import erjang.EAtom;
import erjang.ETuple;
import erjang.ESmall;

import static erjang.beam.CodeAtoms.EXTFUNC_ATOM;

public class ExtFun {
	int mod, fun, arity;

	public ExtFun(int mod, int fun, int arity) {
		this.mod=mod; this.fun=fun; this.arity=arity;
	}

	public String toString() {
		return mod+":"+fun+"/"+arity;
	}
	public ETuple toSymbolic(CodeTables ct) {
		return ETuple.make(EXTFUNC_ATOM,
				   ct.atom(mod),
				   ct.atom(fun),
				   new ESmall(arity));
	}
}
