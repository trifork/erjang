package org.erlang.jbeam.ops;

import org.erlang.ETerm;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public abstract class Insn extends Stmt implements Opcodes {

	Type emit_push(FunctionAdapter ma, Object value) {
		if (value instanceof Expr) {
			return ((Expr) value).emit_push(ma);

		} else if ((value instanceof String) 
				|| (value instanceof Float)
				|| (value instanceof Integer) 
				|| (value instanceof Double)
				|| (value instanceof Long) 
				|| value == null) {
			ma.visitLdcInsn(value);
			
			if (value == null)
				return Type.getType(Object.class);

			if (value instanceof String)
				return Type.getType(String.class);
			
			if (value instanceof Integer)
				return Type.INT_TYPE;
			
			if (value instanceof Float)
				return Type.FLOAT_TYPE;
			
			if (value instanceof Double)
				return Type.DOUBLE_TYPE;
			
			if (value instanceof Long)
				return Type.LONG_TYPE;
			
			throw new Error("should not happen");
			
		} else if (value instanceof ETerm) {

			String field_name = ma.registerConst((ETerm)value);
			Type type = Type.getType(value.getClass());
			ma.visitFieldInsn(GETSTATIC, ma.getSelfClassName(), field_name,
					type.getDescriptor());
			return type;
			
		} else {
			
			throw new Error("unknown constant type");
		}
	}

	abstract void emit(FunctionAdapter ma);

}
