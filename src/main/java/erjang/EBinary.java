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

import java.io.File;
import java.math.BigInteger;
import java.util.zip.Adler32;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class EBinary extends EBitString {

	EBinary(byte[] data, int byteOff, int byteLength) {
		super(data, byteOff * 8, byteLength * 8);
	}

	/**
	 * @param binaryValue
	 */
	public EBinary(byte[] bytes) {
		this(bytes, 0, bytes.length);
	}

	private static final Type EBINARY_TYPE = Type.getType(EBinary.class);
	private static final String EBINARY_NAME = EBINARY_TYPE.getInternalName();

	@Override
	public Type emit_const(MethodVisitor fa) {
		char[] chs = new char[bits / 8];
		for (int i = 0; i < bits / 8; i++) {
			chs[i] = (char) (0xff & octetAt(i));
		}
		String str = new String(chs);

		fa.visitLdcInsn(str);
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, EBINARY_NAME, "fromString",
				"(Ljava/lang/String;)L" + EBINARY_NAME + ";");

		return EBINARY_TYPE;
	}

	public static EBinary fromString(String str) {
		int size = str.length()*8;
		byte[] data = new byte[size/8];
		for (int i = 0; i < str.length(); i++) {
			data[i] = (byte) str.charAt(i);
		}
		return new EBinary(data, 0, size);
	}
	

	
	public EBinary testBinary() {
		return this;
	}

	public void updateAdler32(Adler32 a) {
		a.update(data, bitOff / 8, bits / 8);
	}

	static final int MOD_ADLER = 65521;

	long adler32() {
		return adler32(1);
	}

	long adler32(long adler) {
		long a = (adler & 0xffff), b = (adler >> 16);

		/* Loop over each byte of data, in order */
		for (int index = bitOff / 8; index < bits / 8; ++index) {
			a = (a + (0xff & data[index])) % MOD_ADLER;
			b = (b + a) % MOD_ADLER;
		}

		return (b << 16) | a;
	}

	static long adler32(long adler, int byte_value) {
		long a = (adler & 0xffff), b = (adler >> 16);

		a = (a + (byte_value & 0xff)) % MOD_ADLER;
		b = (b + a) % MOD_ADLER;

		return (b << 16) | a;
	}

	/**
	 * @return
	 */
	public byte[] getByteArray() {
		int octets = bitCount() / 8;
		byte[] res = new byte[octets];
		if ((bitOff % 8) == 0) {
			System.arraycopy(data, bitOff / 8, res, 0, octets);
		} else {
			for (int i = 0; i < octets; i++) {
				res[i] = (byte) octetAt(i);
			}
		}
		return res;
	}

	/**
	 * @return
	 */
	public int byteSize() {
		return bitCount() / 8;
	}

}
