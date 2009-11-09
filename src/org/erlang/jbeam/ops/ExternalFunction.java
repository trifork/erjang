package org.erlang.jbeam.ops;

import org.erlang.EAtom;
import org.erlang.EInteger;
import org.erlang.ETerm;
import org.erlang.ETuple;

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
