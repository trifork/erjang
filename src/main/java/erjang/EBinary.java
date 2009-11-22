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

import java.nio.charset.Charset;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import com.sun.org.apache.bcel.internal.generic.INVOKESTATIC;

public class EBinary extends EObject {

	@Override
	int cmp_order() {
		return 8;
	}
	
	@Override
	int compare_same(EObject rhs) {
		EBinary other = (EBinary) rhs;
		int min = Math.min(data.length, other.data.length);
		for (int i = 0; i < min; i++) {
			int b1 = 0xff & data[i];
			int b2 = 0xff & other.data[i];
			if (b1 < b2) return -1;
			if (b1 > b2) return 1;
		}
		
		if (data.length < other.data.length) return -1;
		if (data.length > other.data.length) return 1;

		return 0;
	}
	
	private static final Type EBINARY_TYPE = Type.getType(EBinary.class);
	private static final String EBINARY_NAME = EBINARY_TYPE.getInternalName();
	private final byte[] data;

	public EBinary(byte[] data) {
		this.data = data;
	}

	@Override
	public Type emit_const(MethodVisitor fa) {
		char[] chs = new char[data.length];
		for (int i = 0; i < data.length; i++) { chs[i] = (char) (0xff & (int)data[i]); }
		String str = new String(chs);
		
		fa.visitLdcInsn(str);
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, EBINARY_NAME, "fromString", "(Ljava/lang/String;)L" + EBINARY_NAME + ";");

		return EBINARY_TYPE;
	}

	public EBinary testBinary() {
		return this;
	}

	/**
	 * @return
	 */
	public byte[] getByteArray() {
		return data;
	}

	/**
	 * @return
	 */
	public int byte_size() {
		return data.length;
	}

	/**
	 * @return
	 */
	public int bit_size() {
		return byte_size() * 8;
	}
}
