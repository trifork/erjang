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

import java.math.BigInteger;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class EBig extends ENumber {

	private final BigInteger value;

	public EBig(BigInteger value) {
		this.value = value;
	}

	public EBig(long res) {
		this.value = BigInteger.valueOf(res);
	}

	@Override
	public int intValue() {
		return value.intValue();
	}

	@Override
	public String toString() {
		return value.toString();
	}

	public static EBig fromString(String value) {
		return new EBig(new BigInteger(value));
	}

	private static final Type EBIG_TYPE = Type.getType(EBig.class);
	private static final Type STRING_TYPE = Type.getType(String.class);

	@Override
	public org.objectweb.asm.Type emit_const(MethodVisitor fa) {

		Type type = EBIG_TYPE;

		fa.visitLdcInsn(value.toString());
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, type.getInternalName(),
				"fromString", "(" + STRING_TYPE.getDescriptor() + ")"
						+ type.getDescriptor());

		return type;
	}

	@Override
	public EBig asb() {
		return new EBig(value.abs());
	}
}
