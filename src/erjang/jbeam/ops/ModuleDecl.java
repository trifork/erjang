package erjang.jbeam.ops;

import erjang.EAtom;

public class ModuleDecl extends Stmt {

	private final EAtom atom;

	public ModuleDecl(EAtom atom) {
		this.atom = atom;
	}

	public String getName() {
		return atom.getName();
	}

}
