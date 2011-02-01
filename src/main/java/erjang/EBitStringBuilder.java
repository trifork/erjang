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

	public static final int PB_IS_WRITABLE = 1;
	public static final int PB_ACTIVE_WRITER = 2;
	
	EBitString bs;
	int byte_pos;
	int extra_bits;
	byte[] data;
	byte flags;
	
	/**
	 * @param byte_size
	 * @param flags
	 */
	public EBitStringBuilder(int byte_size, int flags) {
		this.flags = (byte) flags;
		data = new byte[byte_size];
		bs = EBitString.make(data, 0, byte_size, 0);
	}

	public static 
	EBitStringBuilder bs_init_writable(EObject size) {
		int bin_size = 1024;
		ESmall sz = size.testSmall();
		if (sz != null && sz.value >= 0) {
			bin_size = sz.value;
		}
			
		EBitStringBuilder res = new EBitStringBuilder(bin_size, PB_IS_WRITABLE|PB_ACTIVE_WRITER);
		
		return res;
	}
	
	public EBitStringBuilder(int byte_size, int extra_bits, int flags) {
		if (flags != 0) throw new NotImplemented("flags="+flags);
		data = new byte[byte_size+(extra_bits>0?1:0)];
		bs = EBitString.make(data, 0, byte_size, extra_bits);
	}

	/** return bitstring under construction */
	public EBitString bitstring() {
		return bs;
	}

	public void put_float(EObject value, int bit_size, int flags) {
		if (extra_bits != 0)
			throw new NotImplemented();

		switch (bit_size) {
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

		throw new NotImplemented("val="+value+";size="+bit_size+";flags="+flags);
	}

	public void put_integer(EObject value, int flags) {
		throw new NotImplemented("val="+value+";flags="+flags);
	}

	public void put_integer(EObject value, int bit_size, int flags) {
		boolean litteEndian = (flags & EBinMatchState.BSF_LITTLE) > 0;

		EInteger ei = value.testInteger();
		if (ei==null) throw ERT.badarg(value);

		if (bit_size == 8 && extra_bits==0) { // Common case optimization
			data[byte_pos++] = (byte)ei.intValue();
			return;
		}

		ESmall sm = value.testSmall();

		if (extra_bits == 0 && (bit_size % 8) == 0) {
			int nBytes = bit_size/8;

			// We process the bytes little-endian:
			int pos, delta;
			if (litteEndian) {
				pos = byte_pos;
				delta = 1;
			} else{
				pos = byte_pos + nBytes-1;
				delta = -1;
			}
			byte_pos += nBytes;

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
			
			return;
		}
		
		if (bit_size <= 32 && (bit_size % 8) != 0) {
			int val = sm.value;
			if (litteEndian) 
				throw new NotImplemented();
			
			int bits_left_in_current_byte = 8-extra_bits;
			int msb_bits = Math.min(bits_left_in_current_byte, bit_size);
			int lsb_bits = bit_size-msb_bits;
			
			while (lsb_bits + msb_bits > 0) {
				int mask = ((1 << msb_bits) - 1);
				int putval = ((val >>> lsb_bits) & mask) << (8-msb_bits);
				int getval = data[byte_pos];
				
				assert ((putval & getval) == 0);
				
				data[byte_pos] = (byte) (putval | getval);

				extra_bits = (extra_bits + msb_bits) % 8;
				if (extra_bits == 0) {
					byte_pos += 1;
				}
								
				lsb_bits -= msb_bits;
				msb_bits = Math.min(8, lsb_bits);
			}
			
			return;
		}
		
		EBig bi = value.testBig();
		if (bit_size <= 64 && (bit_size % 8) != 0) {
			long val = bi == null 
				? sm.longValue()
				: bi.longValue();
			if (litteEndian) 
				throw new NotImplemented();
			
			int bits_left_in_current_byte = 
				(extra_bits == 0 
						? 8 
						: 8-extra_bits);

			int msb_bits = Math.min(bits_left_in_current_byte, bit_size);
			int lsb_bits = bit_size-msb_bits;
			
			while(lsb_bits + msb_bits > 0) {
				int mask = ((1 << msb_bits) - 1);
				int putval = (int) ((val >>> lsb_bits) & mask);
				int getval = data[byte_pos] & ~mask;
				
				assert ((putval & getval) == 0);
				
				data[byte_pos] = (byte) (putval | getval);

				extra_bits = (extra_bits + msb_bits) % 8;
				if (extra_bits == 0) {
					byte_pos += 1;
				}
								
				lsb_bits -= msb_bits;
				msb_bits = Math.min(8, lsb_bits);
			};
			
			return;
		}
		
		throw new NotImplemented("put_integer value="+ value +"; bit_size="+  bit_size + "; flags=" + flags);
		
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
		put_byte(b1);
		put_byte(b2);
		put_byte(b3);
		put_byte(b4);
	}

	protected void put_int32_big(int val) {
		byte b1, b2, b3, b4;
		b4 = (byte)val; val >>= 8;
		b3 = (byte)val; val >>= 8;
		b2 = (byte)val; val >>= 8;
		b1 = (byte)val;
		put_byte(b1);
		put_byte(b2);
		put_byte(b3);
		put_byte(b4);
	}
	
	private void put_byte(byte val) {
		
		if (extra_bits == 0) {
			data[byte_pos++] = val;
			return;
			
		} else {
			
			// | bits1 : bits2 |

			int bits1 = extra_bits;
			int bits2 = 8-bits1;

			data[byte_pos] |= (byte)((0xff & val) >> bits1);
			data[byte_pos+1] = (byte) ((val & ((1<<bits1)-1)) << bits2);
			
			byte_pos += 1;
		}

}

	public void put_string(EString str) {
		if (extra_bits != 0)
		{
			for (int i = 0; i < str.length(); i++) {
				put_byte(str.data[str.off+i]);
			}
			return;
		}
		System.arraycopy(str.data, str.off, data, byte_pos, str.length());
		byte_pos += str.length();
	}
	
	/** grow a bitstring by extra_size bits, and return a string builder with position at end of original bitstring */
	public static EBitStringBuilder bs_private_append(EObject str_or_builder, int extra_size, int unit, int flags)
	{
		EBitString ebs = str_or_builder.testBitString();
		if (ebs == null) throw new NotImplemented();
		
		long bitSize = ebs.bitSize() + extra_size;
		int size = (int) (bitSize/8);
		int extra = (int) (bitSize % 8);
		
		EBitStringBuilder result = new EBitStringBuilder(size, extra, flags);
		System.arraycopy(ebs.data, ebs.byteOffset(), result.data, 0, ebs.dataByteSize());
		
		result.byte_pos = ebs.byteSize();
		result.extra_bits = ebs.extra_bits;
		
		return result;
	}


	/** grow a bitstring by extra_size bits, and return a string builder with position at end of original bitstring */
	public static EBitStringBuilder bs_append(EObject str_or_builder, int extra_size, int unit, int flags)
	{
		EBitString ebs = str_or_builder.testBitString();
		if (ebs == null) throw new NotImplemented();
		
		long bitSize = ebs.bitSize() + extra_size;
		int size = (int) (bitSize/8);
		int extra = (int) (bitSize % 8);
		
		EBitStringBuilder result = new EBitStringBuilder(size, extra, flags);
		System.arraycopy(ebs.data, ebs.byteOffset(), result.data, 0, ebs.dataByteSize());
		
		result.byte_pos = ebs.byteSize();
		result.extra_bits = ebs.extra_bits;
		
		return result;
	}

	/** Append a bit string.
	 *  @param size The output size in bits; -1 means the entire bitstring.
	 */
	public void put_bitstring(EObject str, int size, int flags) {
		EBitString ebs = str.testBitString();
		if (ebs == null) throw new InternalError("bad code gen, arg is "+str.getClass());
		
		if (extra_bits != 0)
			throw new NotImplemented();
		
		if (size != -1 && size != ebs.bitSize()) {
			throw new NotImplemented();
		}
		
		System.arraycopy(ebs.data, ebs.byteOffset(), data, byte_pos, ebs.totalByteSize());
		byte_pos += ebs.byteSize();
		extra_bits += ebs.extra_bits; // TODO on extension
	}
	
	public void put_utf8(EObject value, int flags) {
		throw new NotImplemented("val="+value+";flags="+flags);
	}

	public void put_utf16(EObject value, int flags) {
		throw new NotImplemented("val="+value+";flags="+flags);
	}

	public void put_utf32(EObject value, int flags) {
		throw new NotImplemented("val="+value+";flags="+flags);
	}

	// compute size of utf8 char
	static public ESmall bs_utf8_size(EObject value) {
		throw new NotImplemented("val="+value);
	}
	
	// compute size of utf16 char
	static public ESmall bs_utf16_size(EObject value) {
		throw new NotImplemented("val="+value);
	}
}
