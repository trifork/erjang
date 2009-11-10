package org.erlang.jbeam.ops;

import org.erlang.EBIF;
import org.erlang.EAtom;
import org.objectweb.asm.Label;
import org.objectweb.asm.Type;

public class Test extends Insn {

	private final EAtom how;
	private final Label failto;
	private final Object[] args;

	private static final Type BIF_TYPE = Type.getType(EBIF.class);
	
	public Test(EAtom how, Label failto, Object[] args) {
		this.how = how;
		this.failto = failto;
		this.args = args;
	}

	@Override
	void emit(FunctionAdapter ma) {
		
		StringBuffer sig = new StringBuffer("(");
		
		for (int i = 0; i < args.length; i++) {
			Type arg = emit_push(ma, args[i]);
			sig.append(arg.getDescriptor());
		}
		
		sig.append(")Z");
		
		String desc = sig.toString();
		
		ma.visitMethodInsn(INVOKESTATIC, BIF_TYPE.getInternalName(), how.getName(), desc);
		
		ma.visitJumpInsn(IFEQ, failto);

	}

}
