package erjang.jbeam.ops;


import com.sun.xml.internal.ws.org.objectweb.asm.Opcodes;

import erjang.EAtom;

public class Call extends Insn {

	private final int nargs;
	private final EAtom mod;
	private final EAtom fun;
	private final boolean tail;

	public Call(int nargs, EAtom mod, EAtom fun, int ari, boolean tail) {
		this.nargs = nargs;
		this.mod = mod;
		this.fun = fun;
		this.tail = tail;
	}

	@Override
	void emit(FunctionAdapter ma) {

		for (int i = 0; i < nargs; i++) {
			ma.push_xreg(i);
		}

		ma.visitMethodInsn(INVOKESTATIC, ma.getModuleClassName(mod), ma
				.getFunctionName(fun, nargs, tail), ma.getFunctionDesc(nargs));
		if (tail) {
			ma.visitInsn(Opcodes.ARETURN);
		} else {
			ma.store_xreg(0);
		}
	}

}
