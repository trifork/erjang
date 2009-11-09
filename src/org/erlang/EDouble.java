package org.erlang;

import org.erlang.jbeam.ops.CodeAdapter;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class EDouble extends ENumber {

	private static final Type EDOUBLE_TYPE = Type.getType(EDouble.class);
	final double value;
	
	public EDouble(double value) {
		this.value = value;
	}

	@Override
	public int intValue() {
		return (int) value;
	}

	public static ETerm parseDouble(String string) {
		return new EDouble(Double.parseDouble(string));
	}
	

	@Override
	public EDouble asb() {
		return new EDouble (Math.abs(value));
	}

	@Override
	public String toString() {
		return String.valueOf(value);
	}
	

	@Override
	public org.objectweb.asm.Type emit_const(CodeAdapter fa) {

		Type type = EDOUBLE_TYPE;
		
		fa.visitTypeInsn(Opcodes.NEW, type.getInternalName());
		fa.visitInsn(Opcodes.DUP);
		fa.visitLdcInsn(new Double(value));
		fa.visitMethodInsn(Opcodes.INVOKESPECIAL, type.getInternalName(), "<init>", "(" + type.getDescriptor() + ")V");
		
		return type;		
	}


}
