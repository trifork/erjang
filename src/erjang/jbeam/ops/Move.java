package erjang.jbeam.ops;


public class Move extends Insn {

	private final Object from;
	private final Register to;

	public Move(Object from, Register to) {
		this.from = from;
		this.to = to;
	}

	@Override
	void emit(FunctionAdapter ma) {
		emit_push(ma, from);
		to.emit_store(ma);
	}
}
