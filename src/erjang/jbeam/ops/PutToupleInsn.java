package erjang.jbeam.ops;

import org.objectweb.asm.Opcodes;

public class PutToupleInsn extends Insn {

	private final int size;
	private final Register dst;

	public PutToupleInsn(int size, Register dst) {
		this.size = size;
		this.dst = dst;
	}
	
	void emit(FunctionAdapter ma)
	{
		String type = "org/erlang/Touple"+size;
		ma.visitTypeInsn(Opcodes.NEW, "org/erlang/Tuple"+size);
		ma.visitInsn(Opcodes.DUP);
		ma.visitMethodInsn(Opcodes.INVOKESPECIAL, type, "<init>", "()V");
		dst.emit_store(ma);
	}
	

}
