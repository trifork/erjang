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
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.util.zip.CRC32;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class EBinary extends EBitString {

	public static final EBinary EMPTY = new EBinary(new byte[0]);

	public EBinary(byte[] data, int byteOff, int byteLength) {
		super(data, byteOff, byteLength, 0);
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
		char[] chs = new char[byteSize()];
		for (int i = 0; i < byteSize(); i++) {
			chs[i] = (char) (0xff & octetAt(i));
		}
		String str = new String(chs);

		fa.visitLdcInsn(str);
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, EBINARY_NAME, "fromString",
				"(Ljava/lang/String;)L" + EBINARY_NAME + ";");

		return EBINARY_TYPE;
	}

	public static EBinary fromString(String str) {
		int size = str.length();
		byte[] data = new byte[size];
		for (int i = 0; i < str.length(); i++) {
			data[i] = (byte) str.charAt(i);
		}
		return new EBinary(data, 0, size);
	}

	public EBinary testBinary() {
		return this;
	}

	/**
	 * @return
	 */
	public byte[] getByteArray() {
		int octets = byteSize();
		byte[] res = new byte[octets];
		System.arraycopy(data, byteOffset(), res, 0, octets);
		return res;
	}

	public long crc() {
		CRC32 crc = new CRC32();

		int octets = byteSize();
		crc.update(data, byteOffset(), octets);
		return crc.getValue();
	}

	/**
	 * @param barr
	 * @throws IOException
	 */
	public void appendTo(OutputStream barr) {
		try {
			barr.write(data, byteOffset(), byteSize());
		} catch (IOException e) {
			throw new Error(e);
		}
	}

	/**
	 * @param barr
	 * @throws IOException 
	 * @throws IOException
	 */
	public void writeTo(OutputStream o) throws IOException {
		o.write(data, byteOffset(), byteSize());
	}

	/**
	 * @param data
	 * @return
	 */
	public static EBinary make(ByteBuffer data) {
		if (data == null || data.remaining() == 0)
			return EMPTY;
		return new EBinary(data.array(), data.arrayOffset() + data.position(),
				data.remaining());
	}

	/**
	 * @return
	 */
	public ByteBuffer toByteBuffer() {
		return ByteBuffer.wrap(this.data, byteOffset(), byteSize());
	}

	/**
	 * @param eInputStream
	 * @return
	 */
	public static EBinary read(EInputStream eInputStream) throws IOException {
	    byte[] data = eInputStream.read_binary();
	    return new EBinary(data); //TODO: use a make().
	}

	/**
	 * @return
	 */
	public EInputStream getInputStream() {
		int octets = byteSize();
		return new EInputStream(data, byteOffset(), octets,
				EInputStream.DECODE_INT_LISTS_AS_STRINGS);
	}

	/**
	 * @return true if this binary is all 7-bit bytes
	 */
	public boolean is_7bit() {
		for (int i = 0; i < byteSize(); i++) {
			byte b = data[byteOffset() + i];
			if ((b & 0x80) != 0) 
				return false;
		}
		return true;
	}

	@Override
	public void encode(EOutputStream eos) {
		eos.write_binary(getByteArray());
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("<<\"");
		for (int i = 0; i < byteSize(); i++) {
			char b = (char) (0xff & byteAt(i*8));
			if (b >= 32 && b < 127)
				EString.appendChar(sb, b);
			else
				return super.toString();
		}
		
		sb.append("\">>");
		return sb.toString();
	}

	public static EBinary make(byte[] res) {
		return new EBinary(res);
	}

	public EBinary sub_binary(int off, int len) {
		return new EBinary(data, this.byteOffset() + off, len);
	}
}
