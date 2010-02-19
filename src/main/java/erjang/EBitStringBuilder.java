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

/**
 * 
 */
public class EBitStringBuilder {

	EBitString bs;
	int bpos;
	int extraBits;
	byte[] data;
	
	/**
	 * @param size
	 * @param flags
	 */
	public EBitStringBuilder(int size, int flags) {
		if (flags != 0) throw new NotImplemented("flags="+flags);
		data = new byte[size];
		bs = new EBitString(data, 0, size, 0);
	}

	public EBitStringBuilder(int size, int extra, int flags) {
		if (flags != 0) throw new NotImplemented("flags="+flags);
		data = new byte[size+1];
		bs = new EBitString(data, 0, size, extra);
	}

	/** return bitstring under construction */
	public EBitString bitstring() {
		return bs;
	}

	public void put_float(EObject value, int size, int flags) {
		if (extraBits != 0)
			throw new NotImplemented();

		switch (size) {
		case 32: {
			ENumber en = value.testNumber();
			if (en==null) { throw ERT.badarg(value); }
			float val = (float)en.doubleValue();
			put_int32(Float.floatToIntBits(val), flags);
			return;
		}
		case 64: {
			ENumber en = value.testNumber();
			if (en==null) { throw ERT.badarg(value); }
			double val = en.doubleValue();
			put_int64(Double.doubleToLongBits(val), flags);
			return;
		}
		} // switch

		throw new NotImplemented("val="+value+";size="+size+";flags="+flags);
	}

	public void put_integer(EObject value, int flags) {
		throw new NotImplemented("val="+value+";flags="+flags);
	}

	public void put_integer(EObject value, int size, int flags) {
		if (extraBits != 0)
			throw new NotImplemented("Unaligned");

		boolean litteEndian = (flags & EBinMatchState.BSF_LITTLE) > 0;

		EInteger ei = value.testInteger();
		if (ei==null) throw ERT.badarg(value);

		if (size == 8) { // Common case optimization
			data[bpos++] = (byte)ei.intValue();
			return;
		}

		ESmall sm = value.testSmall();

		if (size % 8 == 0) {
			int nBytes = size/8;

			// We process the bytes little-endian:
			int pos, delta;
			if (litteEndian) {
				pos = bpos;
				delta = 1;
			} else{
				pos = bpos + nBytes-1;
				delta = -1;
			}
			bpos += nBytes;

			if (sm!=null) { // ESmall case
				int val = sm.intValue();
				while (nBytes-- > 0) {
					data[pos] = (byte)val;
					pos += delta;
					val >>= 8;
				}
		    } else { // Larger integer case
				BigInteger big_int = ei.bigintValue();

				byte[] bytes = big_int.toByteArray();
				int src_pos = bytes.length;
				while (--src_pos >= 0 && nBytes-- > 0) {
					data[pos] = bytes[src_pos];
					pos += delta;
				}
				if (nBytes > 0) {
					byte sign_byte = (byte)(big_int.signum() < 0 ? -1 : 0);
					while (nBytes-- > 0) {
						data[pos] = sign_byte;
						pos += delta;
					}
				}
			}
		}
	}

	protected void put_int64(long val, int flags) {
		if ((flags & EBinMatchState.BSF_LITTLE) > 0) {
			put_int32_little((int)val);
			put_int32_little((int)(val>>32));
		} else {
			put_int32_big((int)(val>>32));
			put_int32_big((int)val);
		}
	}

	protected void put_int32(int val, int flags) {
		if ((flags & EBinMatchState.BSF_LITTLE) > 0) {
			put_int32_little(val);
		} else {
			put_int32_big(val);
		}
	}

	protected void put_int32_little(int val) {
		byte b1, b2, b3, b4;
		b1 = (byte)val; val >>= 8;
		b2 = (byte)val; val >>= 8;
		b3 = (byte)val; val >>= 8;
		b4 = (byte)val;
		data[bpos++] = b1;
		data[bpos++] = b2;
		data[bpos++] = b3;
		data[bpos++] = b4;
	}

	protected void put_int32_big(int val) {
		byte b1, b2, b3, b4;
		b4 = (byte)val; val >>= 8;
		b3 = (byte)val; val >>= 8;
		b2 = (byte)val; val >>= 8;
		b1 = (byte)val;
		data[bpos++] = b1;
		data[bpos++] = b2;
		data[bpos++] = b3;
		data[bpos++] = b4;
	}

	public void put_string(EString str) {
		if (extraBits != 0)
			throw new NotImplemented();
		System.arraycopy(str.data, str.off, data, bpos, str.length());
		bpos += str.length();
	}
	
	public static EBitStringBuilder bs_append(EObject str_or_builder, int extra_size, int flags)
	{
		EBitString ebs = str_or_builder.testBitString();
		if (ebs == null) throw new NotImplemented();
		
		long bitSize = ebs.bitSize() + extra_size;
		int size = (int) (bitSize/8);
		int extra = (int) (bitSize % 8);
		
		EBitStringBuilder result = new EBitStringBuilder(size, extra, flags);
		System.arraycopy(ebs.data, ebs.byteOffset(), result.data, 0, ebs.dataByteSize());
		result.bpos = (int) ebs.bitSize();
		return result;
	}

	public void put_bitstring(EObject str, EAtom how, int flags) {
		EBitString ebs = str.testBitString();
		if (ebs == null) throw new InternalError("bad code gen, arg is "+str.getClass());
		
		throw new NotImplemented();
	}
}
