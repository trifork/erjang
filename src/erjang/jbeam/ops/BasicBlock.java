package erjang.jbeam.ops;

import java.util.ArrayList;
import java.util.List;

public class BasicBlock {

	final int label;
	
	List<Insn> insns = new ArrayList<Insn>();

	private final FunctionDecl fun;

	public BasicBlock(FunctionDecl fun, int label) {
		this.fun = fun;
		this.label = label;
		fun.add(this);
	}

	public void emit(FunctionAdapter fa) {

		fa.visitLabel(fa.get_label(label));
		
		for (Insn i : insns) {
			i.emit(fa);
		}
		
	}

	public void append(Insn insn) {
		insns.add(insn);
	}

}
