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

import java.nio.ByteBuffer;
import java.util.List;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class ENil extends ESeq {

	private static final Type ENIL_TYPE = Type.getType(ENil.class);
	private static final Type ERT_TYPE = Type.getType(ERT.class);
	
	private static ByteBuffer EMPTY_BB = ByteBuffer.allocate(0);

	public ENil() {
		super();
	}

	public boolean isNil() {
		return true;
	}


	@Override
	public EObject head() {
		throw new UnsupportedOperationException();
	}

	@Override
	public EList tail() {
		throw new UnsupportedOperationException();
	}
	
	public boolean collectIOList(List<ByteBuffer> out) {
		return true;
	}

	@Override
	public void collectCharList(CharCollector out) {
	}

	@Override
	public String toString() {
		return "[]";
	}

	@Override
	public EList cons(EObject h) {
		return new EList(h, this);
	}

	@Override
	public Type emit_const(MethodVisitor fa) {
		fa.visitFieldInsn(Opcodes.GETSTATIC, ERT_TYPE.getInternalName(), "NIL", ENIL_TYPE.getDescriptor());
		return ENIL_TYPE;
	}
	
	public ENil testNil() {
		return this;
	}

	/* (non-Javadoc)
	 * @see erjang.EObject#compare_same(erjang.EObject)
	 */
	@Override
	int compare_same(EObject rhs) {
		if (rhs.testNil() != null) return 0;
		return -1;
	}
}
