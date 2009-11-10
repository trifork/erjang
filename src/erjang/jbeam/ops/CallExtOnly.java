package org.erlang.jbeam.ops;

import org.erlang.EFun;
import org.erlang.ETerm;
import org.objectweb.asm.Type;

public class CallExtOnly extends Insn {

	private static final Type EFUN_TYPE = Type.getType(EFun.class);
	private static final Type ETERM_TYPE = Type.getType(ETerm.class);
	private final int args;
	private final ExternalFunction fun;
	private final boolean tail;

	public CallExtOnly(int args, ExternalFunction fun, boolean tail) {
		this.args = args;
		this.fun = fun;
		this.tail = tail;
	}

	@Override
	void emit(FunctionAdapter ma) {

		// deref external
		String sig = ma.deref_external_fun(fun);
		
		// push arguments
		for (int i = 0; i < args; i++) {
			ma.push_xreg(i);
		}
		
		if (tail) {
			ma.visitMethodInsn(INVOKEINTERFACE, EFUN_TYPE.getInternalName(), "invoke_tail", sig);
			ma.visitInsn(ARETURN);
		} else {
			ma.visitMethodInsn(INVOKEINTERFACE, EFUN_TYPE.getInternalName(), "invoke", sig);
			ma.store_xreg(0);
		}

	}

}
