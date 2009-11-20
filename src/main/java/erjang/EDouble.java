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

public class EDouble extends ENumber {

	private static final Type EDOUBLE_TYPE = Type.getType(EDouble.class);
	public final double value;
	
	public EDouble(double value) {
		this.value = value;
	}

	public EDouble testFloat() {
		return this;
	}

	@Override
	public int intValue() {
		return (int) value;
	}

	public static EObject parseDouble(String string) {
		return new EDouble(Double.parseDouble(string));
	}
	

	@Override
	public EDouble asb() {
		return new EDouble (Math.abs(value));
	}

	@Override
	public String toString() {
		return String.valueOf(value);
	}
	

	@Override
	public org.objectweb.asm.Type emit_const(MethodVisitor fa) {

		Type type = EDOUBLE_TYPE;
		
		fa.visitTypeInsn(Opcodes.NEW, type.getInternalName());
		fa.visitInsn(Opcodes.DUP);
		fa.visitLdcInsn(new Double(value));
		fa.visitMethodInsn(Opcodes.INVOKESPECIAL, type.getInternalName(), "<init>", "(D)V");
		
		return type;		
	}

	/**
	 * @return
	 */
	public EString to_list() {
		return new EString(String.valueOf(value));
	}


}
