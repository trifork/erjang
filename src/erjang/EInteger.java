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

public class EInteger extends ENumber {

	private static final Type EINTEGER_TYPE = Type.getType(EInteger.class);
	public final int value;
	
	public EInteger(int value)
	{
		this.value = value;
	}
	
	public EInteger asInteger() {
		return this;
	}

	@Override
	public int hashCode() {
		return value;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof EInteger) {
			EInteger o = (EInteger) obj;
			return o.value == value;
		}
		return false;
	}
	
	public int intValue() {
		return value;
	}

	@Override
	public String toString() {
		return String.valueOf(value);
	}
	
	@Override
	public org.objectweb.asm.Type emit_const(CodeAdapter fa) {

		Type type = EINTEGER_TYPE;
		
		fa.visitTypeInsn(Opcodes.NEW, type.getInternalName());
		fa.visitInsn(Opcodes.DUP);
		fa.visitLdcInsn(new Integer(value));
		fa.visitMethodInsn(Opcodes.INVOKESPECIAL, type.getInternalName(), "<init>", "(" + type.getDescriptor() + ")V");
		
		return type;
	}

	@Override
	public EInteger asb() {
		return new EInteger (Math.abs(value));
	}
}
