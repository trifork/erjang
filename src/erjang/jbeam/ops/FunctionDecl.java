package org.erlang.jbeam.ops;

import java.util.ArrayList;
import java.util.List;

import org.erlang.EAtom;
import org.erlang.ETerm;
import org.erlang.Tail;
import org.erlang.jbeam.BEAMFile;
import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodAdapter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class FunctionDecl extends Stmt implements Opcodes {

	private static final Type TAIL_TYPE = Type.getType(Tail.class);

	private static final String[] EXCEPTIONS = new String[] { Type.getType(
			Exception.class).getInternalName() };

	private static final Type ETERM_TYPE = Type.getType(ETerm.class);

	private final EAtom name;
	final int arity;
	private final int start;

	final List<BasicBlock> bbs = new ArrayList<BasicBlock>();

	private final BEAMFile beam;

	private boolean is_tail_recursive;

	public FunctionDecl(BEAMFile beam, EAtom name, int arity, int start) {
		this.beam = beam;
		this.name = name;
		this.arity = arity;
		this.start = start;
	}

	public void emit(BEAMFile beam, ClassAdapter ca) {

		MethodVisitor mv = ca.visitMethod(ACC_STATIC, getFunctionName(name,
				arity, is_tail_recursive), Type.getMethodDescriptor(ETERM_TYPE, argTypes()),
				null, EXCEPTIONS);

		FunctionAdapter fa = new FunctionAdapter(beam, this, mv, ca);

		fa.visitCode();

		fa.assign_initial_registers();

		fa.visitJumpInsn(GOTO, beam.get_label(start));

		for (BasicBlock bb : bbs) {
			bb.emit(fa);
		}

		fa.visitEnd();
	}

	private Type[] argTypes() {
		Type[] result = new Type[arity + 1];
		for (int i = 0; i < arity; i++) {
			result[i] = ETERM_TYPE;
		}
		result[arity] = TAIL_TYPE;
		return result;
	}

	public void finish() {
		// TODO Auto-generated method stub

	}

	public void add(BasicBlock basicBlock) {
		bbs.add(basicBlock);
	}

	public Label get_label(int label) {
		return beam.get_label(label);
	}

	public String getModuleClassName(EAtom mod) {
		return beam.getModuleClassName(mod.getName());
	}

	public String getFunctionName(EAtom fname, int nargs, boolean isTailRecursive) {
		return fname.getName() + "$" + nargs + (isTailRecursive? "_tail" : "");
	}

	public String getSelfClassName() {
		return beam.getInternalClassName();
	}

	public String registerConst(ETerm value) {
		return beam.registerConst(value);
	}
	public String deref_external_fun( CodeAdapter ca, ExternalFunction fun) {
		return beam.deref_external_fun(ca, fun);
	}

	public void set_is_tail_recursive(boolean tailp) {
		this.is_tail_recursive = tailp;
	}
}
