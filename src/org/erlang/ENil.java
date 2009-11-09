package org.erlang;

import org.erlang.jbeam.ops.CodeAdapter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class ENil extends ESeq {

	private static final Type ECONS_TYPE = Type.getType(ECons.class);
	private static final Type ENIL_TYPE = Type.getType(ENil.class);

	public ENil() {
		super();
	}

	@Override
	public ETerm head() {
		throw new UnsupportedOperationException();
	}

	@Override
	public EList tail() {
		throw new UnsupportedOperationException();
	}

	@Override
	public String toString() {
		return "[]";
	}

	@Override
	public EList cons(EObject h) {
		return new EList(h, this);
	}

	@Override
	public Type emit_const(CodeAdapter fa) {
		fa.visitFieldInsn(Opcodes.GETSTATIC, ECONS_TYPE.getInternalName(), "EMPTY", ENIL_TYPE.getDescriptor());
		return ENIL_TYPE;
	}
}
