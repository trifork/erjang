package org.erlang;

import java.math.BigInteger;

import org.erlang.jbeam.ops.CodeAdapter;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class EBig extends ENumber {

	private final BigInteger value;

	public EBig(BigInteger value) {
		this.value = value;
	}

	@Override
	public int intValue() {
		return value.intValue();
	}

	@Override
	public String toString() {
		return value.toString();
	}

	public static EBig fromString(String value) {
		return new EBig(new BigInteger(value));
	}

	private static final Type EBIG_TYPE = Type.getType(EBig.class);
	private static final Type STRING_TYPE = Type.getType(String.class);

	@Override
	public org.objectweb.asm.Type emit_const(CodeAdapter fa) {

		Type type = EBIG_TYPE;

		fa.visitLdcInsn(value.toString());
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, type.getInternalName(),
				"fromString", "(" + STRING_TYPE.getDescriptor() + ")"
						+ type.getDescriptor());

		return type;
	}

	@Override
	public EBig asb() {
		return new EBig(value.abs());
	}
}
