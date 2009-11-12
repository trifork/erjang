package erjang.jbeam.ops;

import erjang.EAtom;
import erjang.EInteger;
import erjang.ETerm;
import erjang.ETuple;

public class ExternalFunction {

	public final EAtom mod;
	public final EAtom name;
	public final int arity;

	public ExternalFunction(EAtom mod, EAtom name, int arity) {
		this.mod = mod;
		this.name = name;
		this.arity = arity;
	}
	
	public ETuple getID() {
		return ETuple.make(new ETerm[] {mod, name, new EInteger(arity)});
	}

}
