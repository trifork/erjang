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

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import erjang.m.erlang.erlang$bifs;

public class EList extends ESeq {
	
	private final EObject head;
	private final ESeq tail;
	
	public EList(EObject h, ESeq tail) {
		if (tail == null) tail = NIL;
		
		this.head = h;
		this.tail = tail;
	}
	
	public ECons testNonEmptyList() {
		return this;
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
		while (val != ECons.NIL) {
			if (val != this) { sb.append(","); }
			sb.append(val.head());
			val = val.tail();
		}
		
		sb.append("]");
		return sb.toString();
	}
	

	static Type ELIST_TYPE = Type.getType(EList.class);
	static Type ETERM_TYPE = Type.getType(EObject.class);
	static String CONSTRUCTOR_DESC = "(" + ETERM_TYPE.getDescriptor() + ELIST_TYPE.getDescriptor() + ")V";
	
	@Override
	public Type emit_const(MethodVisitor fa) {
		Type type = ELIST_TYPE;
		
		fa.visitTypeInsn(Opcodes.NEW, type.getInternalName());
		fa.visitInsn(Opcodes.DUP);

		((EObject)head).emit_const(fa);
		((EObject)tail).emit_const(fa);

		fa.visitMethodInsn(Opcodes.INVOKESPECIAL, type.getInternalName(), "<init>", CONSTRUCTOR_DESC);
		
		return type;
	}
	

	
}
