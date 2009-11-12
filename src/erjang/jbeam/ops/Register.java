package erjang.jbeam.ops;

import org.objectweb.asm.Type;


public abstract class Register extends Expr {

	protected int reg;
	private Type type;
	
	public Register(int reg) {
		this.reg = reg;
		this.type = Type.getType(Object.class);
	}

	abstract int assigned(FunctionAdapter fa);
	
	public void emit_store(FunctionAdapter ma) {
		ma.visitVarInsn(ASTORE, assigned(ma));
	}

	@Override
	Type emit_push(FunctionAdapter ma) {
		ma.visitVarInsn(ALOAD, assigned(ma));
		return type;
	}

}
