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

/**
 * 
 */
public class EBitString extends EObject {

	public static final Type EBITSTRING_TYPE = Type.getType(EBitString.class);
	public static final String EBITSTRING_NAME = EBITSTRING_TYPE.getInternalName();
	
	protected final byte[] data;
	protected final int bits;
	protected final int bitOff;

	public EBitString(byte[] data) {
		this(data.clone(), 0, data.length * 8);
	}

	@Override
	int cmp_order() {
		return CMP_ORDER_BITSTRING;
	}


	@Override
	public Type emit_const(MethodVisitor fa) {
		char[] chs = new char[bits / 8 + 1];
		
		for (int pos = 0; pos < bits; pos += 8) {
			
			int rest = Math.min(8, bits-pos);
			
			int oc = 0xff & intBitsAt(pos, rest);

			if (rest < 8) {
				oc <<= (8-rest);
			}
			
			chs[pos/8] = (char)oc;
		}
		
		String str = new String(chs);

		fa.visitLdcInsn(str);
		fa.visitLdcInsn(new Integer(bits % 8));
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, EBITSTRING_NAME, "fromString",
				"(Ljava/lang/String;I)L" + EBITSTRING_NAME + ";");

		return EBITSTRING_TYPE;
	}

	public static EBitString fromString(String str, int extra) {
		int size = str.length()*8;
		byte[] data = new byte[size/8];
		for (int i = 0; i < str.length(); i++) {
			data[i] = (byte) str.charAt(i);
		}
		return new EBitString(data, 0, extra == 0 ? size : size-8+extra);
	}
	
	@Override
	public boolean equalsExactly(EObject rhs) {
		if (rhs.cmp_order() != CMP_ORDER_BITSTRING) return false;
		
		EBitString ebs = (EBitString)rhs;
		
		if (bitCount() != ebs.bitCount()) return false;

		int bc1 = bitCount();
		int bc2 = ebs.bitCount();
		int limit = Math.min(bc1, bc2);
		
		for (int pos = 0; pos < limit; pos += 8) {
			
			int rest = Math.min(8, limit-pos);
			
			int oc1 = 0xff & intBitsAt(pos, rest);
			int oc2 = 0xff & ebs.intBitsAt(pos, rest);

			if (oc1 != oc2) return false;
		}
		
		return true;
	}

	@Override
	int compare_same(EObject rhs) {
		EBitString ebs = (EBitString)rhs;
		
		int bc1 = bitCount();
		int bc2 = ebs.bitCount();
		int limit = Math.min(bc1, bc2);
		
		for (int pos = 0; pos < limit; pos += 8) {
			
			int rest = Math.min(8, limit-pos);
			
			int oc1 = 0xff & intBitsAt(pos, rest);
			int oc2 = 0xff & ebs.intBitsAt(pos, rest);
			
			if (oc1 == oc2) continue;
			
			if (oc1 < oc2) return -1;
			if (oc1 > oc2) return 1;
		}

		if (bc1 == bc2)
			return 0;
		
		if (bc1 < bc2)
			return -1;
		else
			return 1;
	}

	public EBitString(byte[] data, int offset, int bits) {
		this.data = data;
		this.bitOff = offset;
		this.bits = bits;
	}

	public boolean isBinary() {
		return (bits & 0x07) == 0;
	}

	public int octetAt(int idx) {
		return intBitsAt(idx * 8, 8);
	}

	public int bitCount() {
		return bits;
	}

	public EBitString substring(int bitOff) {
		if (bitOff < 0 || bitOff > bitCount()) {
			throw new IllegalArgumentException("offset out of range");
		}
		return new EBitString(data, bitOff + bitOff, bitCount() - bitOff);
	}

	public EBitString substring(int bitOff, int len) {
		if (bitOff < 0 || bitOff + len > bitCount()) {
			throw new IllegalArgumentException("offset out of range");
		}
		return new EBitString(data, bitOff + bitOff, len);
	}

	public int bitAt(int bitPos) {
		if (bitPos < 0 || bitPos >= bitCount()) {
			throw new IllegalArgumentException("bit index out of range");
		}

		bitPos += bitOff;
		int data_byte = (int) data[bitPos >>> 3];
		int shift = 7 - (bitPos & 0x07);
		int bit = 0x01 & (data_byte >> shift);
		return bit;
	}

	public int intBitsAt(int bitPos, int bitLength) {

		if (bitPos + bitLength > this.bits) {
			throw new IllegalArgumentException(
					"reading beyond end of BitString");
		}

		if (bitLength < 0 || bitLength > 32)
			throw new IllegalArgumentException(
					"this method can only get 32 bits");

		bitPos += bitOff;

		int res = 0;

		// first, get the right-most bits from data[bitPos/8]
		if ((bitPos & 0x07) != 0) {

			// how many bits from this byte?
			int len = 8 - (bitPos & 0x07);

			// the byte
			int val = 0x0ff & (int) data[bitPos >> 3];
			res = val & ((1 << len) - 1);

			if (bitLength < len) {
				res >>>= (len - bitLength);
				return res;
			}

			bitLength -= len;
			bitPos += len;

		}

		assert ((bitPos & 0x07) == 0);

		// we're getting bytes
		int pos = bitPos >> 3;
		while (bitLength > 7) {
			res <<= 8;
			res |= 0x0ff & (int) data[pos++];

			bitPos += 8;
			bitLength -= 8;
		}

		assert (bitLength < 8);

		// finally, get the left-most bits from data[bitPos/8]
		if (bitLength != 0) {

			// how many bits from this byte?
			int len = bitLength;

			// the byte
			int val = 0x0ff & (int) data[bitPos >> 3];
			res = val >> (8 - len);

			bitLength -= len;
			bitPos += len;
		}

		assert (bitLength == 0);

		return res;

	}

	public byte byteAt(int bitPos) {
		return (byte) intBitsAt(bitPos, 8);
	}

	public double doubleAt(int bitPos) {
		return Double.longBitsToDouble(longBitsAt(bitPos, 64));
	}

	public double floatAt(int bitPos) {
		return Float.intBitsToFloat(intBitsAt(bitPos, 32));
	}

	public long longBitsAt(int bitPos, int bitLength) {

		if (bitPos + bitLength > this.bits) {
			throw new IllegalArgumentException(
					"reading beyond end of BitString");
		}

		if (bitLength < 0 || bitLength > 64)
			throw new IllegalArgumentException(
					"this method can only get 64 bits");

		long res = 0;

		bitPos += bitOff;

		// first, get the right-most bits from data[bitPos/8]
		if ((bitPos & 0x07) != 0) {

			// how many bits from this byte?
			int len = 8 - (bitPos & 0x07);

			// the byte
			int val = 0x0ff & (int) data[bitPos >> 3];
			res = val & ((1 << len) - 1);

			// are we looking for less that len bits?
			if (bitLength < len) {
				res >>>= (len - bitLength);
				return res;
			}

			bitLength -= len;
			bitPos += len;
		}

		assert ((bitPos & 0x07) == 0);

		// we're getting bytes
		int pos = bitPos >> 3;
		while (bitLength > 7) {
			res <<= 8;
			res |= 0x0ff & (int) data[pos++];

			bitPos += 8;
			bitLength -= 8;
		}

		assert (bitLength < 8);
		assert ((bitPos & 0x07) == 0);

		// finally, get the left-most bits from data[bitPos/8]
		if (bitLength != 0) {

			// how many bits from this byte?
			int len = bitLength;

			// the byte
			int val = 0x0ff & (int) data[bitPos >> 3];
			res = val >> (8 - len);

			bitLength -= len;
			bitPos += len;
		}

		assert (bitLength == 0);

		return res;

	}

	@Override
	public String toString() {

		foo: if (bits % 8 == 0) {
			StringBuilder sb = new StringBuilder("<<\"");

			int i = 0;
			for (; i < bits / 8; i += 8) {
				char ch = (char) (0xff & octetAt(i));
				if (ch < ' ' || ch > '~')
					break foo;
				
				sb.append((char)i);
			}

			sb.append("\">>");
			return sb.toString();
		}

		StringBuilder sb = new StringBuilder("<<");
		int i = 0;
		for (; i < bits - 8; i += 8) {
			sb.append(0xff & intBitsAt(i, 8));
			sb.append(',');
		}

		int lastBitLength = bits - i;
		sb.append(0xff & intBitsAt(i, lastBitLength));
		if (lastBitLength != 8) {
			sb.append(':').append(lastBitLength);
		}
		sb.append(">>");
		return sb.toString();
	}

}
