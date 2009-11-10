package org.erlang;

import org.erlang.jbeam.ops.CodeAdapter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class EInteger extends ENumber {

	private static final Type EINTEGER_TYPE = Type.getType(EInteger.class);
	final int value;
	
	public EInteger(int value)
	{
		this.value = value;
	}
	
	@Override
	public int hashCode() {
		return value;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof EInteger) {
			EInteger o = (EInteger) obj;
			return o.value == value;
		}
		return false;
	}
	
	public int intValue() {
		return value;
	}

	@Override
	public String toString() {
		return String.valueOf(value);
	}
	
	@Override
	public org.objectweb.asm.Type emit_const(CodeAdapter fa) {

		Type type = EINTEGER_TYPE;
		
		fa.visitTypeInsn(Opcodes.NEW, type.getInternalName());
		fa.visitInsn(Opcodes.DUP);
		fa.visitLdcInsn(new Integer(value));
		fa.visitMethodInsn(Opcodes.INVOKESPECIAL, type.getInternalName(), "<init>", "(" + type.getDescriptor() + ")V");
		
		return type;
	}

	@Override
	public EInteger asb() {
		return new EInteger (Math.abs(value));
	}
}
