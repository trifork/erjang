/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2009 by Trifork
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *  
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/

package erjang;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import erjang.jbeam.ops.CodeAdapter;

public class EPair extends ECons {
	public final EObject head;
	public final EObject tail;
	
	public EPair(EObject h, EObject t) {
		
		assert (h != null);
		assert (t != null);
		
		this.head = h;
		this.tail = t;
	}

	public ECons testNonEmptyList() {
		return this;
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
		while (val instanceof ECons && val != NIL) {
			ECons ep = (ECons) val;
			if (val != this) { sb.append(","); }
			sb.append(ep.head());
			val = ep.tail();
		}
		
		if (val != NIL) {
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
