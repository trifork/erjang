package org.erlang.jbeam.ops;

import org.objectweb.asm.Opcodes;

public class PutInsn extends Insn {

	private final int idx;
	private final Object value;

	public PutInsn(int idx, Object value) {
		this.idx = idx;
		this.value = value;
	}

	// touple
	void emit(FunctionAdapter ma)
	{
		ma.visitInsn(DUP);
		emit_push(ma, value);
		ma.visitFieldInsn(PUTFIELD, "org/erlang/Touple"+idx, "v"+idx, "Ljava/lang/Object;");
		
	}

}
