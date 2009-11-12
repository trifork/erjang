package erjang.beam;

import erjang.EAtom;
import erjang.EInteger;
import erjang.EList;
import erjang.EObject;
import erjang.ESeq;
import erjang.ETerm;
import erjang.ETuple;
import erjang.ETuple0;

public class BeamFile {

	static public class LabeledBlockVisitor {

		public void visitInsn(BeamOpcode opcode, ETuple et) {
			
		}

		public void visitEnd() {
			
		}

	}

	private static final EAtom BEAM_FILE = EAtom.intern("beam_file");
	private static final EAtom MODULE = EAtom.intern("module");
	private static final EAtom EXPORTS = EAtom.intern("exports");
	private static final EAtom ATTRIBUTES = EAtom.intern("attributes");
	private static final EAtom COMP_INFO = EAtom.intern("comp_info");
	private static final EAtom CODE = EAtom.intern("code");

	private EAtom module;
	private ESeq exports;
	private ESeq attributes;
	private ESeq comp_info;
	private ESeq code;

	public BeamFile(ETuple data) {
		assert (data.nth(1) == BEAM_FILE);

		module = data.nth(2).asAtom();
		exports = data.nth(3).asSeq();
		attributes = data.nth(4).asSeq();
		comp_info = data.nth(5).asSeq();
		code = data.nth(6).asSeq();
	}

	public static class ModuleVisitor {

		protected void visitModule(EAtom name) {
		}

		/** list of {Fun,Arity,Entry} */
		protected void visitExport(EAtom fun, int arity, int entry) {
		}

		/** list of {Atom,Value} */
		protected void visitAttribute(EAtom att, EObject eObject) {
		}

		/** Visit function */
		protected FunctionVisitor visitFunction(EAtom name, int arity, int startLabel) {
			return new FunctionVisitor();
		}

		protected void visitEnd() {
		}

	}

	public static class FunctionVisitor {
		public void visitEnd() {
		}

		public LabeledBlockVisitor visitLabeledBlock(int label) {
			return new LabeledBlockVisitor();
		}
	}

	void accept(ModuleVisitor v) {
		v.visitModule(module);

		visit_exports(v);

		visit_attributes(v);

		for (ESeq exp = (ESeq) code; exp != ESeq.EMPTY; exp = exp.tail()) {
			ETuple fun = (ETuple) exp.head();

			visit_function(v, fun);
		}

		v.visitEnd();

	}

	private void visit_function(ModuleVisitor v, ETuple fun) {

		EAtom name = (EAtom) fun.nth(2);
		int ary = fun.nth(3).asInt();
		int entry = fun.nth(4).asInt();
		EList insns = (EList) fun.nth(5);

		FunctionVisitor fv = v.visitFunction(name, ary, entry);

		visit_insns(insns, fv);

		fv.visitEnd();
	}

	private void visit_insns(EList insns, FunctionVisitor fv) {
		
		LabeledBlockVisitor bbv = null;
		
		for (ESeq insn = (ESeq) insns; insn != ESeq.EMPTY; insn = insn.tail()) {

			EObject i = insn.head();
			
			if (i instanceof EAtom) {
				BeamOpcode opcode = BeamOpcode.get((EAtom) i);
				bbv.visitInsn(opcode, ETuple.make(new EObject[] {i}));
				
			} else if (i instanceof ETuple) {
				ETuple et = (ETuple) i;
				BeamOpcode opcode = BeamOpcode.get((EAtom) et.nth(1));
				
				if (opcode == BeamOpcode.label) {
					EInteger label = (EInteger) et.nth(2);
					
					if (bbv != null) {
						bbv.visitEnd();
					}
					
					bbv = fv.visitLabeledBlock(label.asInt());
				} else {
					bbv.visitInsn(opcode, et);
				}
			} else {
				throw new IllegalArgumentException();
			}
		}
		
		if (bbv != null) {
			bbv.visitEnd();
		}
	}

	private void visit_attributes(ModuleVisitor v) {
		for (ESeq exp = (ESeq) attributes; exp != ESeq.EMPTY; exp = exp
				.tail()) {
			ETuple one = (ETuple) exp.head();
			v.visitAttribute((EAtom) one.nth(1), one.nth(2));
		}
	}

	private void visit_exports(ModuleVisitor v) {
		for (ESeq exp = (ESeq) exports; exp != ESeq.EMPTY; exp = exp
				.tail()) {
			ETuple one = (ETuple) exp.head();
			v.visitExport((EAtom) one.nth(1), one.nth(2).asInt(), one.nth(3)
					.asInt());
		}
	}

}
