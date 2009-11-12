package erjang.jbeam.ops;

import org.objectweb.asm.Type;
import org.objectweb.asm.commons.Method;

import erjang.EAtom;
import erjang.ERT;


public class FuncInfo extends Insn {

	private static final Type ERT_TYPE = Type.getType(ERT.class);
	private static final Method BAD_MATCH;

	static {
		try {
			BAD_MATCH = Method.getMethod(ERT.class.getMethod("bad_match",
					new Class[] { EAtom.class, EAtom.class, int.class }));
		} catch (Exception e) {
			throw new Error(e);
		}
	}

	private final EAtom module;
	private final EAtom fun;
	private final int arity;

	public FuncInfo(EAtom module, EAtom fun, int arity) {
		this.module = module;
		this.fun = fun;
		this.arity = arity;
	}

	@Override
	void emit(FunctionAdapter ma) {

		emit_push(ma, module);
		emit_push(ma, fun);
		ma.visitLdcInsn(new Integer(arity));
		
		ma.visitMethodInsn(INVOKESTATIC, ERT_TYPE.getInternalName(), "no_match", 
				BAD_MATCH.getDescriptor());

		// make jvm happy
		ma.visitInsn(ARETURN);
		
	}
}
