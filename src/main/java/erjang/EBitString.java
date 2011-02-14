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
import java.nio.ByteBuffer;
import java.util.List;
import java.util.Set;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import erjang.m.ets.EMatchContext;
import erjang.m.ets.EPattern;
import erjang.m.ets.ETermPattern;

/**
 * 
 */
public class EBitString extends EObject {

	public static final Type EBITSTRING_TYPE = Type.getType(EBitString.class);
	public static final String EBITSTRING_NAME = EBITSTRING_TYPE
			.getInternalName();

	protected final byte[] data;
	private final int data_offset;
	private final int byte_size;
	protected final int extra_bits;
	//private final long bits;
	//protected final long bitOff = 0;

	protected EBitString(byte[] data) {
		this(data.clone(), 0, data.length, 0);
	}
	
	@Override
	public int hashCode() {
		int h = 0;
		for (int i = 0; i < byte_size; i++) {
			h = h * 31 + data[data_offset + i];
		}
		if (extra_bits != 0) {
			int val = data[data_offset + byte_size] >>> (8-extra_bits);
			h = h * 31 + val;
		}
		return h;
	}

	/**
	 * @return the bits
	 */
	public long bitSize() {
		return byteSize() * 8L + extra_bits;
	}

	/** 
	 * @return number of bytes needed to hold this bit string (byteSize + (extrabits?1:0))
	 * */
	protected int dataByteSize() {
		return byteSize() + (extra_bits > 0 ? 1 : 0);
	}
	
	public EBitString testBitString() {
		return this;
	}

	/** returns true if this bitstring matches the give <code>matcher</code>. */
	public boolean match(ETermPattern matcher, EMatchContext r) {
		return matcher.match(this, r);
	}

	public EBinary testBinary() {
		if (isBinary()) {
			assert false : "EBitString is Binary and testBinary is not overridden.";
			// this should never happen
			return new EBinary(toByteArray());
		}
		return null;
	}

	@Override
	int cmp_order() {
		return CMP_ORDER_BITSTRING;
	}

	@Override
	public Type emit_const(MethodVisitor fa) {
		char[] chs = new char[dataByteSize()];

		for (int byteIdx = 0; byteIdx < byteSize(); byteIdx += 1) {
			chs[byteIdx] = (char)(data[byteIdx] & 0xff);
		}
		
		if (extra_bits != 0) {
			int rest = intBitsAt(byteSize()*8, extra_bits);
			rest <<= (8-extra_bits);
			chs[byteSize()] = (char)rest;
		}
		

		String str = new String(chs);

		fa.visitLdcInsn(str);
		fa.visitLdcInsn(new Integer(extra_bits));
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, EBITSTRING_NAME, "make",
				"(Ljava/lang/String;I)L" + EBITSTRING_NAME + ";");

		return EBITSTRING_TYPE;
	}

	/** called by generated code to emit a constant BitString.
	 * If extra > 0, then the last char of str contains the extra bits */
	public static EBitString make(String str, int extra) {
		int in_len = str.length();
		byte[] data = new byte[in_len];
		for (int i = 0; i < in_len; i++) {
			data[i] = (byte) str.charAt(i);
		}
		
		int data_len = in_len - (extra>0 ? 1 : 0);
		
		return EBitString.make(data, 0, data_len, extra);
	}

	@Override
	public boolean equalsExactly(EObject rhs) {
		if (rhs.cmp_order() != CMP_ORDER_BITSTRING)
			return false;

		EBitString ebs = (EBitString) rhs;

		if (byteSize() != ebs.byteSize())
			return false;
		
		if (extra_bits != ebs.extra_bits)
			return false;
		
		int byteOffset = byteOffset();
		int ebsByteOffset = ebs.byteOffset();

		for (int i = 0; i < byte_size; i++) {
			if (data[byteOffset + i] != ebs.data[ebsByteOffset + i])
				return false;
		}
		
		if (extra_bits != 0) {
			int myExtraBits = intBitsAt(8L*byte_size, extra_bits);
			int hisExtraBits = ebs.intBitsAt(8L*byte_size, extra_bits);
			return myExtraBits == hisExtraBits;
		} else {
			return true;
		}
	}

	@Override
	int compare_same(EObject rhs) {
		EBitString ebs = (EBitString) rhs;

		long bc1 = bitSize();
		long bc2 = ebs.bitSize();
		long limit = Math.min(bc1, bc2);

		for (int pos = 0; pos < limit; pos += 8) {

			int rest = limit-pos>8 ? 8 : (int)(limit-pos);

			int oc1 = 0xff & intBitsAt(pos, rest);
			int oc2 = 0xff & ebs.intBitsAt(pos, rest);

			if (oc1 == oc2)
				continue;

			if (oc1 < oc2)
				return -1;
			if (oc1 > oc2)
				return 1;
		}

		if (bc1 == bc2)
			return 0;

		if (bc1 < bc2)
			return -1;
		else
			return 1;
	}

	public static EBitString make(byte[] data, int byteOff, int byteLength, int extra_bits) {
		if (extra_bits == 0) {
			return new EBinary(data, byteOff, byteLength);
		} else {
			return new EBitString(data, byteOff, byteLength, extra_bits);
		}
	}

	public static EBitString makeByteOffsetTail(EBitString org, int byteOff) {
	    return EBitString.make(org.data, org.data_offset+byteOff, org.byte_size-byteOff, org.extra_bits);
	}

	protected EBitString(byte[] data, int byte_off, int byte_len, int extra_bits) {
		this.data = data;
		this.data_offset = byte_off;
		this.byte_size = byte_len;
		this.extra_bits = extra_bits;

		if (data.length < byte_off + byte_len + (extra_bits>0?1:0)) {
			throw new IllegalArgumentException();
		}
		if (extra_bits<0 || extra_bits>=8) {
			throw new IllegalArgumentException();
		}
	}

	public boolean isBinary() {
		return extra_bits == 0;
	}

	public int octetAt(int byteIndex) {
		return data[byteOffset() + byteIndex] & 0xff;
	}

	public int uint16_at(int byteIndex, int flags) {
	    int b1 = data[byteOffset() + byteIndex]     & 0xff;
	    int b2 = data[byteOffset() + byteIndex + 1] & 0xff;
	    if ((flags & EBinMatchState.BSF_LITTLE) > 0)
			return b1 + (b2 << 8);
	    else
			return (b1 << 8) + b2;
	}

	public int int32_at(int byteIndex, int flags) {
	    int b1 = data[byteOffset() + byteIndex]     & 0xff;
	    int b2 = data[byteOffset() + byteIndex + 1] & 0xff;
	    int b3 = data[byteOffset() + byteIndex + 2] & 0xff;
	    int b4 = data[byteOffset() + byteIndex + 3] & 0xff;
	    if ((flags & EBinMatchState.BSF_LITTLE) > 0)
			return  b1        + (b2 <<  8) + (b3 << 16) + (b4 << 24);
	    else
			return (b1 << 24) + (b2 << 16) + (b3 <<  8) +  b4;
	}

	public EBitString substring(long bitOff) {
		return substring(bitOff, bitSize() - bitOff);
	}
	
	public EBitString substring(long bitOff, long bit_len) {
		if (bitOff < 0 || bitOff + bit_len > bitSize()) {
			throw new IllegalArgumentException("offset out of range");
		}

		bitOff += byteOffset() * 8;

		int out_full_bytes = (int) (bit_len/8);
		int extra = (int) (bit_len%8);
		if (0 == (bitOff % 8)) {
			return EBitString.make(data,
					(int)(bitOff/8),
					out_full_bytes, extra);
		}
		
		int out_bytes = (int) (out_full_bytes + (extra==0 ? 0 : 1));		
		byte[] res = new byte[out_bytes];
		for (int i = 0; i < out_full_bytes; i++) {
			res[i] = (byte) intBitsAt(bitOff + i*8, 8);
		}
		if (extra != 0) {
			res[(int) out_full_bytes] =
				(byte) (intBitsAt(bitOff + bit_len - extra, extra) << (8-extra));
		}
		return EBitString.make(res, 0, (int) out_full_bytes, extra);
	}

	public int bitAt(long bitPos) {
		if (bitPos < 0 || bitPos >= bitSize()) {
			throw new IllegalArgumentException("bit index out of range");
		}

		bitPos += byteOffset() * 8;
		int data_byte = (int) data[(int) (bitPos >>> 3)];
		int shift = 7 - (int)(bitPos & 0x07);
		int bit = 0x01 & (data_byte >> shift);
		return bit;
	}

	public int intBitsAt(long bitPos, int bitLength) {

		if (bitPos + bitLength > this.bitSize()) {
			throw new IllegalArgumentException(
					"reading beyond end of BitString");
		}

		if (bitLength < 0 || bitLength > 32)
			throw new IllegalArgumentException(
					"this method can only get 32 bits");

		bitPos += byteOffset() * 8;

		int res = 0;

		// first, get the right-most bits from data[bitPos/8]
		if ((bitPos & 0x07) != 0) {

			// how many bits from this byte?
			int len = 8 - (int)(bitPos & 0x07);

			// the byte
			int val = 0x0ff & (int) data[(int) (bitPos >> 3)];
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
		int pos = (int) (bitPos >> 3);
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
			int val = 0x0ff & (int) data[pos];
			res <<= bitLength;
			res |= val >> (8 - len);

			bitLength -= len;
			bitPos += len;
		}

		assert (bitLength == 0);

		return res;
	}


	public int intLittleEndianBitsAt(long bitPos, int bitLength) {
		if (bitPos + bitLength > this.bitSize()) {
			throw new IllegalArgumentException(
					"reading beyond end of BitString");
		}

		if (bitLength < 0 || bitLength > 32)
			throw new IllegalArgumentException(
					"this method can only get 32 bits");

		bitPos += byteOffset() * 8;

		int res = 0;
		int res_offset = 0;

		// first, get the left-most bits from data[bitPos/8]
		if ((bitPos & 0x07) != 0) {

			// how many bits from this byte?
			int len = 8 - (int)(bitPos & 0x07);

			// the byte
			int val = data[(int) (bitPos >> 3)] & 0x0ff;
			res = val & ((1 << len) - 1);

			if (bitLength < len) {
				res >>>= (len - bitLength);
				return res;
			}

			res_offset += len;
			bitLength -= len;
			bitPos += len;
		}

		assert ((bitPos & 0x07) == 0);

		// we're getting bytes
		int pos = (int) (bitPos >> 3);
		while (bitLength > 7) {
			res |= (data[pos++] & 0x0ff) << res_offset;

			res_offset += 8;
			bitPos += 8;
			bitLength -= 8;
		}

		assert (bitLength < 8);

		// finally, get the left-most bits from data[bitPos/8]
		if (bitLength != 0) {

			// how many bits from this byte?
			int len = bitLength;

			// the byte
			int val = 0x0ff & (int) data[(int) (bitPos >> 3)];
			res |= (val >> (8 - len)) << res_offset;

			res_offset += len;
			bitLength -= len;
			bitPos += len;
		}

		assert (bitLength == 0);

		return res;
	}

	/** Sign extend value of size bits.
	 * Assumes that bits above sign are zero.
	 * @see http://www-graphics.stanford.edu/~seander/bithacks.html#VariableSignExtend
	 * */
	static int signExtend(int val, int bits) {
		// Simpler (but slower?):
		// int nonbits = 32-bits;
		// return (val << nonbits) >> nonbits;
		if (bits==32) return val;
		int r;      // resulting sign-extended number
		int m = 1 << (bits - 1); // mask can be pre-computed if b is fixed

		// val = val & ((1 << bits) - 1);  // (Skip this if bits in x above position b are already zero.)
		r = (val ^ m) - m;

		return r;
	}
	
	static long signExtend(long val, int bits) {
		// Simpler (but slower?):
		// int nonbits = 64-bits;
		// return (val << nonbits) >> nonbits;
		if (bits==64) return val;
		long r;      // resulting sign-extended number
		long m = 1L << (bits - 1); // mask can be pre-computed if b is fixed

		// val = val & ((1 << bits) - 1);  // (Skip this if bits in x above position b are already zero.)
		r = (val ^ m) - m;

		return r;
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

	public long longBitsAt(long bitPos, int bitLength) {

		if (bitPos + bitLength > this.bitSize()) {
			throw new IllegalArgumentException(
					"reading beyond end of BitString");
		}

		if (bitLength < 0 || bitLength > 64)
			throw new IllegalArgumentException(
					"this method can only get 64 bits");

		long res = 0;

		bitPos += byteOffset() * 8;

		// first, get the right-most bits from data[bitPos/8]
		if ((bitPos & 0x07) != 0) {

			// how many bits from this byte?
			int len = 8 - (int)(bitPos & 0x07);

			// the byte
			int val = 0x0ff & (int) data[(int) (bitPos >> 3)];
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
		int pos = (int) (bitPos >> 3);
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
			int val = 0x0ff & (int) data[(int) (bitPos >> 3)];
			res <<= bitLength;
			res |= val >> (8 - len);

			bitLength -= len;
			bitPos += len;
		}

		assert (bitLength == 0);

		return res;

	}

	public long longLittleEndianBitsAt(long bitPos, int bitLength) {
		if (bitPos + bitLength > this.bitSize()) {
			throw new IllegalArgumentException(
					"reading beyond end of BitString");
		}

		if (bitLength < 0 || bitLength > 64)
			throw new IllegalArgumentException(
					"this method can only get 64 bits");

		long res = 0;
		int res_offset = 0;

		bitPos += byteOffset() * 8;

		// first, get the right-most bits from data[bitPos/8]
		if ((bitPos & 0x07) != 0) {

			// how many bits from this byte?
			int len = 8 - (int)(bitPos & 0x07);

			// the byte
			int val =  data[(int) (bitPos >> 3)] & 0x0ff;
			res = val & ((1 << len) - 1);

			// are we looking for less that len bits?
			if (bitLength < len) {
				res >>>= (len - bitLength);
				return res;
			}

			res_offset += len;
			bitLength -= len;
			bitPos += len;
		}

		assert ((bitPos & 0x07) == 0);

		// we're getting bytes
		int pos = (int) (bitPos >> 3);
		while (bitLength >= 8) {
			res |= ((long)(data[pos++] & 0x0ff)) << res_offset;

			res_offset += 8;
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
			int val = data[pos] & 0x0ff;
			res |= (long)(val >> (8 - len)) << res_offset;

			res_offset += len;
			bitLength -= len;
			bitPos += len;
		}

		assert (bitLength == 0);

		return res;
	}

	@Override
	public String toString() {

		foo: if (extra_bits == 0) {
			StringBuilder sb = new StringBuilder("<<\"");

			for (int i = 0; i < bitSize(); i += 8) {
				char ch = (char) (0xff & intBitsAt(i, 8));
				if (ch < ' ' || ch > '~')
					break foo;

				sb.append((char) ch);
			}

			sb.append("\">>");
			return sb.toString();
		}

		StringBuilder sb = new StringBuilder("<<");
		int i = 0;
		long max = Math.min(bitSize()-8, 20*8);
		for (; i < max; i += 8) {
			sb.append(0xff & intBitsAt(i, 8));
			sb.append(',');
		}
		if (max != bitSize()-8) { sb.append("...,"); i = (int) (bitSize()-8); }

		int lastBitLength = (int) (bitSize() - i);
		sb.append(0xff & intBitsAt(i, lastBitLength));
		if (lastBitLength != 8) {
			sb.append(':').append(lastBitLength);
		}
		sb.append(">>");
		return sb.toString();
	}

	/**
	 * @return
	 */
	public byte[] toByteArray() {
		byte[] result = new byte[dataByteSize()];
		System.arraycopy(data, byteOffset(), result, 0, dataByteSize());
		return result;
	}

	/**
	 * @return true if successful
	 */
	@Override
	public boolean collectIOList(List<ByteBuffer> out) {
		if (extra_bits != 0) 
			return false;
		
		if (byteSize() > 0) {
			out.add(ByteBuffer.wrap(data, byteOffset(), byteSize()));
		}
		
		return true;
	}

	/**
	 * @param eInputStream
	 * @return
	 */
	public static EBitString read(EInputStream eInputStream) throws IOException {
		int[] pad_bits = new int[1];
	    byte[] data = eInputStream.read_bitstr(pad_bits);
		int extra_bits = 8 - pad_bits[0];
		if (extra_bits==8) {
			return new EBinary(data); //TODO: use a make().
		} else {
			int len = data.length-1;
			return make(data, 0, data.length-1, extra_bits);

		}
	}

	protected int byteOffset() {
		return data_offset;
	}

	public int byteSize() {
		return byte_size;
	}

	public int totalByteSize() {
		return byte_size + (extra_bits>0 ? 1 : 0);
	}

	@Override
	public void collectCharList(CharCollector out)
		throws CharCollector.CollectingException,
		CharCollector.InvalidElementException,
		IOException
	{
		if (extra_bits != 0)
			throw new CharCollector.InvalidElementException();

		try {
			out.addBinary(data, data_offset, byte_size);
		} catch (CharCollector.PartialDecodingException e) {
			int n = e.inputPos;
			throw new CharCollector.CollectingException(EBitString.make(data, data_offset+n, byte_size-n, extra_bits));
		}
	}

	@Override
	public void encode(EOutputStream eos) {
		// TODO: Avoid copying.
		eos.write_bitstr(toByteArray(), extra_bits==0 ? 0 : 8-extra_bits);
	}

	@Override
	public ETermPattern compileMatch(Set<Integer> out) {
		return EPattern.compilePattern(this, out);
	}


}
