package erjang.jbeam.ops;

public class Return extends Insn {

	@Override
	void emit(FunctionAdapter ma) {
		ma.push_xreg(0);
		ma.visitInsn(ARETURN);
	}

}
