package org.erlang;

import org.erlang.jbeam.ops.CodeAdapter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class EList extends ESeq {
	
	private final EObject head;
	private final ESeq tail;
	
	public EList(EObject h, ESeq tail) {
		if (tail == null) tail = EMPTY;
		
		this.head = h;
		this.tail = tail;
	}
	
	// only for ENil!
	protected EList() {
		head = tail = null;
	}

	@Override
	public EList cons(EObject h) {
		// TODO Auto-generated method stub
		return new EList(h, this);
	}

	@Override
	public EObject head() {
		return head;
	}

	@Override
	public ESeq tail() {
		// TODO Auto-generated method stub
		return tail;
	}
	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer("[");
		
		ESeq val = this;
		while (val != ECons.EMPTY) {
			if (val != this) { sb.append(","); }
			sb.append(val.head());
			val = val.tail();
		}
		
		sb.append("]");
		return sb.toString();
	}
	

	static Type ELIST_TYPE = Type.getType(EList.class);
	static Type ETERM_TYPE = Type.getType(ETerm.class);
	static String CONSTRUCTOR_DESC = "(" + ETERM_TYPE.getDescriptor() + ELIST_TYPE.getDescriptor() + ")V";
	
	@Override
	public Type emit_const(CodeAdapter fa) {
		Type type = ELIST_TYPE;
		
		fa.visitTypeInsn(Opcodes.NEW, type.getInternalName());
		fa.visitInsn(Opcodes.DUP);

		fa.emit_const(head);
		fa.emit_const(tail);

		fa.visitMethodInsn(Opcodes.INVOKESPECIAL, type.getInternalName(), "<init>", CONSTRUCTOR_DESC);
		
		return type;
	}
}
