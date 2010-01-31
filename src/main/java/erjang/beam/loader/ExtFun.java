package erjang.beam.loader;

import erjang.EAtom;

public  class ExtFun {
	EAtom mod, fun;
	int arity;

	public ExtFun(EAtom mod, EAtom fun, int arity) {
		this.mod=mod; this.fun=fun; this.arity=arity;
	}

	public String toString() {
		return mod+":"+fun+"/"+arity;
	}
}
