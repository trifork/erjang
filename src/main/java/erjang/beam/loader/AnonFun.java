package erjang.beam.loader;

import erjang.EAtom;
import erjang.ETuple;
import erjang.ESmall;

public class AnonFun {
	int fun;
	int total_arity, free_vars;
	int label;
	int occur_nr;
	int uniq;

	public AnonFun(int fun, int total_arity, int free_vars, int label, int occur_nr, int uniq) {
		this.fun=fun;
		this.total_arity=total_arity;
		this.free_vars=free_vars;
		this.label=label;
		this.occur_nr=occur_nr;
		this.uniq=uniq;
	}

	public String toString() {
		return fun+"/"+total_arity;
	}
	public ETuple toSymbolic(CodeTables ct) {
		return ETuple.make(ct.moduleName(),
				   ct.atom(fun),
				   new ESmall(total_arity));
	}
}
