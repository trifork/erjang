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

public class ENil extends ESeq {

	private static final Type ECONS_TYPE = Type.getType(ECons.class);
	private static final Type ENIL_TYPE = Type.getType(ENil.class);

	public ENil() {
		super();
	}

	@Override
	public ETerm head() {
		throw new UnsupportedOperationException();
	}

	@Override
	public EList tail() {
		throw new UnsupportedOperationException();
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
	public Type emit_const(CodeAdapter fa) {
		fa.visitFieldInsn(Opcodes.GETSTATIC, ECONS_TYPE.getInternalName(), "EMPTY", ENIL_TYPE.getDescriptor());
		return ENIL_TYPE;
	}
}
