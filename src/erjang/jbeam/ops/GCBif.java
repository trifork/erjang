package erjang.jbeam.ops;

import org.objectweb.asm.Label;
import org.objectweb.asm.Type;

import erjang.EAtom;
import erjang.ETerm;
import erjang.modules.erl_bif;

public class GCBif extends Insn {

	private static final Type EXCEPTION_TYPE = Type.getType(Exception.class);

	private static final Type BIF_TYPE = Type.getType(erl_bif.class);

	private static final Type ETERM_TYPE = Type.getType(ETerm.class);
	
	
	private final EAtom name;
	private final Label fail;
	private final int hmm;
	private final Object[] args;
	private final Register dest;

	public GCBif(EAtom name, Label fail, int hmm, Object[] args, Register dest) {
		this.name = name;
		this.fail = fail;
		this.hmm = hmm;
		this.args = args;
		this.dest = dest;
	}

	@Override
	void emit(FunctionAdapter ma) {

		Label before = new Label();
		ma.visitLabel(before);
		
		StringBuffer sig = new StringBuffer("(");
		
		for (int i = 0; i < args.length; i++) {
			Type arg = emit_push(ma, args[i]);
			sig.append(arg.getDescriptor());
		}
		
		sig.append(")");
		sig.append(ETERM_TYPE.getDescriptor());
		
		String desc = sig.toString();
		
		ma.visitMethodInsn(INVOKESTATIC, BIF_TYPE.getInternalName(), getBifName(name), desc);
		
		Label after = new Label();
		ma.visitLabel(after);
		
		dest.emit_store(ma);
		
		if (fail != null) {
		   ma.visitTryCatchBlock(before, after, fail, EXCEPTION_TYPE.getClassName());
		}
	}

	private String getBifName(EAtom name) {
		String nam = name.getName();
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < nam.length(); i++) {
			char ch = nam.charAt(i);
			if (Character.isJavaIdentifierPart(ch)) {
				sb.append(ch);
		 	} else {
		 		sb.append('$');
		 		sb.append(Integer.toHexString(ch));
		 	}
		}
		return sb.toString();
	}

}
