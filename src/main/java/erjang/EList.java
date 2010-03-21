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

import java.io.IOException;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public final class EList extends ESeq {
	
	private final EObject head;
	private final ESeq tail;
	
	public EList(EObject h, ESeq tail) {
		if (tail == null) tail = ERT.NIL;
		
		if (h == null)
			throw new NullPointerException();
		
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
		return new EList(h, this);
	}

	@Override
	public EObject head() {
		return head;
	}

	@Override
	public ESeq tail() {
		return tail;
	}
	
	@Override
	public String toString() {
		
		try {
			// TODO: make this faster, we generate too many exceptions
			// on account of this piece of code!
			ESeq str = EString.make(this);
			return str.toString();
		} catch (ErlangException e) {
			// ignor e//
		}
		
		StringBuffer sb = new StringBuffer("[");

		assert (this instanceof EList);
		
		ESeq val = this;
		while ((val.testNil()) == null) {
			if (val != this) { sb.append(","); }
			sb.append(val.head());
			val = val.tail();
		}
		
		sb.append("]");
		return sb.toString();
	}
	

	static Type ELIST_TYPE = Type.getType(EList.class);
	static Type ESEQ_TYPE = Type.getType(ESeq.class);
	static Type ETERM_TYPE = Type.getType(EObject.class);
	static String CONSTRUCTOR_DESC = "(" + ETERM_TYPE.getDescriptor() + ESEQ_TYPE.getDescriptor() + ")V";
	
	@Override
	public Type emit_const(MethodVisitor fa) {
		Type type = ELIST_TYPE;
		
		fa.visitTypeInsn(Opcodes.NEW, type.getInternalName());
		fa.visitInsn(Opcodes.DUP);

		((EObject)head).emit_const(fa);
		((EObject)tail).emit_const(fa);

		fa.visitMethodInsn(Opcodes.INVOKESPECIAL,
				type.getInternalName(), "<init>", CONSTRUCTOR_DESC);
		
		return type;
	}

	/**
	 * @param messages
	 * @return
	 */
	public static ESeq make(Object... messages) {
		ESeq result = ERT.NIL;
		for (int i = messages.length-1; i >= 0; i--) {
			result = result.cons((EObject)messages[i]);
		}
		return result;
	}

	public static ESeq make(int... messages) {
		ESeq result = ERT.NIL;
		for (int i = messages.length-1; i >= 0; i--) {
			result = result.cons(ERT.box( messages[i] ));
		}
		return result;
	}

	public static EObject read(EInputStream buf) throws IOException {
        final int arity = buf.read_list_head();
        EObject[] elems;
        EObject tail;
		if (arity > 0) {
            elems = new EObject[arity];
            for (int i = 0; i < arity; i++) {
                elems[i] = buf.read_any();
            }
            /* discard the terminating nil (empty list) or read tail */
            if (buf.peek1() == EExternal.nilTag) {
                buf.read_nil();
                tail = ERT.NIL;
            } else {
            	tail = buf.read_any();
            }
            
            EObject res = tail;
            for (int i = arity-1; i >= 0; i--) {
            	res = res.cons(elems[i]);
            }
            
            return res;
        } else {
        	return ERT.NIL;
        }
	}

	@Override
	public void encode(EOutputStream eos) {
		int len = this.length();
		eos.write_list_head(len);
		ESeq curr = this;
		while (!curr.isNil()) {
			eos.write_any(curr.head());
			curr = curr.tail();
		}
		eos.write_nil();
	}

}
