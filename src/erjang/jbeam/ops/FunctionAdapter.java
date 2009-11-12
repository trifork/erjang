package erjang.jbeam.ops;

import java.util.HashMap;
import java.util.Map;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import erjang.EAtom;
import erjang.ETerm;
import erjang.Tail;
import erjang.jbeam.BEAMFile;

public class FunctionAdapter extends CodeAdapter {

	private static final Type TAIL_TYPE = Type.getType(Tail.class);
	private static final Type ETERM_TYPE = Type.getType(ETerm.class);
	private final FunctionDecl fun;
	private ClassAdapter classAdapter;

	public FunctionAdapter(BEAMFile beam, FunctionDecl fun, MethodVisitor mv, ClassAdapter ca) {
		super(beam, mv);
		this.fun = fun;
		this.classAdapter = ca;
	}

	Map<Integer, Integer> xregs = new HashMap<Integer, Integer>();
	Map<Integer, Integer> yregs = new HashMap<Integer, Integer>();
	int allocated = 0;
	int tail_reg = 0;
	
	void assign_initial_registers() {
		for (int i = 0; i < fun.arity; i++) {
			xregs.put(i, allocated++);
		}
		
		tail_reg = allocated++;
	}
	
	int get_xreg(int i) {
		Integer val = xregs.get(i);
		if (val == null) {
			xregs.put(i, val = allocated++);
		}
		return val;
	}
	
	int get_yreg(int i) {
		Integer val = yregs.get(i);
		if (val == null) {
			yregs.put(i, val = allocated++);
		}
		return val;
	}

	public Label get_label(int label) {
		return fun.get_label(label);
	}

	public void push_xreg(int i) {
		int var = get_xreg(i);
		this.visitVarInsn(Opcodes.ALOAD, var);
	}

	public void store_xreg(int i) {
		this.visitVarInsn(Opcodes.ASTORE, get_xreg(i));
	}

	public String getModuleClassName(EAtom mod) {
		return fun.getModuleClassName(mod);
	}

	public String getFunctionName(EAtom fname, int nargs, boolean isTail) {
		return fun.getFunctionName(fname, nargs, isTail);
	}

	public String getFunctionDesc(int nargs) {
		StringBuffer sb = new StringBuffer("(");
		for (int i = 0; i < nargs; i++) {
			sb.append(ETERM_TYPE.getDescriptor());
		}
		sb.append(TAIL_TYPE.getDescriptor());
		sb.append(")");
		sb.append(ETERM_TYPE.getDescriptor());
		
		return sb.toString();
	}

	public void push_tailreg() {
		this.visitVarInsn(Opcodes.ALOAD, tail_reg);
	}

	public String getSelfClassName() {
		return fun.getSelfClassName();
	}

	public String registerConst(ETerm value) {
		return fun.registerConst(value);
	}

	public String deref_external_fun(ExternalFunction fun2) {
		return this.fun.deref_external_fun(this, fun2);
	}



}
