package org.erlang;

import org.erlang.jbeam.ops.CodeAdapter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class EPair extends ECons {
	public final EObject head;
	public final EObject tail;
	
	public EPair(EObject h, EObject t) {
		
		assert (h != null);
		assert (t != null);
		
		this.head = h;
		this.tail = t;
	}


	public EObject head() {
		return head;
	}

	public ETerm tail() {
		return (ECons)tail;
	}

	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer("[");
		
		EObject val = this;
		while (val instanceof ECons && val != EMPTY) {
			ECons ep = (ECons) val;
			if (val != this) { sb.append(","); }
			sb.append(ep.head());
			val = ep.tail();
		}
		
		if (val != EMPTY) {
			sb.append('|');
			sb.append(val);
		}
		
		sb.append("]");
		return sb.toString();
	}

	static Type EPAIR_TYPE = Type.getType(EPair.class);
	static Type ETERM_TYPE = Type.getType(ETerm.class);
	static String CONSTRUCTOR_DESC = "(" + ETERM_TYPE.getDescriptor() + ETERM_TYPE.getDescriptor() + ")V";
	
	@Override
	public Type emit_const(CodeAdapter fa) {
		Type type = EPAIR_TYPE;
		
		fa.visitTypeInsn(Opcodes.NEW, type.getInternalName());
		fa.visitInsn(Opcodes.DUP);

		fa.emit_const(head);
		fa.emit_const(tail);

		fa.visitMethodInsn(Opcodes.INVOKESPECIAL, type.getInternalName(), "<init>", CONSTRUCTOR_DESC);
		
		return type;
	}
}
