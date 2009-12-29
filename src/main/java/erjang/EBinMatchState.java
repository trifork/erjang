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

public class EBinMatchState {

	/** Field is guaranteed to be byte-aligned. */
	public static final int BSF_ALIGNED = 1;	
	
	/** Field is little-endian (otherwise big-endian). */
	public static final int BSF_LITTLE = 2;		
	
	/** Field is signed (otherwise unsigned). */
	public static final int BSF_SIGNED = 4;		
	
	/** Size in bs_init is exact. */
	public static final int BSF_EXACT = 8;		

	/** Native endian. */
	public static final int BSF_NATIVE = 16;		

	public static final EAtom ATOM_ALL = EAtom.intern("all");

	public final EBitString bin;
	long bit_pos;

	public long bitsLeft() {
		return bin.bitSize() - bit_pos;
	}

	public EBitString binary() {
		return bin;
	}

	public static EBinMatchState bs_start_match2(EObject obj) {
		EBitString bs;
		if ((bs = obj.testBitString()) == null)
			return null;
		return new EBinMatchState(bs);
	}

	public EBinMatchState(EBitString binary) {
		this.bin = binary;
		this.bit_pos = 0;
	}

	public EBitString bs_get_binary2(EObject spec, int flags) {

		if (spec == ATOM_ALL) {

			EBitString result = bin.substring(bit_pos, bitsLeft());
			bit_pos += result.bitSize();
			return result;

		}

		throw new Error("unknown spec: " + spec);

		// return null;
	}

	public EInteger bs_get_integer2(int size, int flags) {

		if (flags == 0 || flags == BSF_SIGNED) {
			// ok 
		} else {
			throw new Error("unhandled flags: " + flags);
		}

		if (size == 0) {
			return ESmall.ZERO;
		}

		if (size < 0 || bin.bitSize() > (bit_pos + size)) {
			// no match
			return null;
		}

		if (size <= 32) {
			int value = bin.intBitsAt(bit_pos, size);
			if ((flags & BSF_SIGNED) == BSF_SIGNED) {
				value = EBitString.signExtend(value, size);
			}
			ESmall res = new ESmall(value);
			bit_pos += size;
			return res;
		}

		if (size <= 64) {
			long value = bin.longBitsAt(bit_pos, size);
			if ((flags & BSF_SIGNED) == BSF_SIGNED) {
				value = EBitString.signExtend(value, size);
			}
			EInteger res = ERT.box(value);
			bit_pos += size;
			return res;
		}

		boolean signed = ((flags & BSF_SIGNED) == BSF_SIGNED);
		byte[] data;
		int extra_in_front = (size%8);
		if (extra_in_front != 0) {
			int bytes_needed = size/8 + (extra_in_front==0 ? 0 : 1);
			data = new byte[bytes_needed];
			int out_offset = 0;

			data[0] = (byte) bin.intBitsAt(bit_pos, extra_in_front);
			out_offset = 1;
			bit_pos += extra_in_front;
			
			if (signed) {
				// in this case, sign extend the extra bits to 8 bits
				data[0] = (byte) ( 0xff & EBitString.signExtend(data[0], extra_in_front) );
			}

			for (int i = 0; i < size/8; i++) {
				data[out_offset+i] = (byte) bin.intBitsAt(bit_pos, 8);
				bit_pos += 8;
			}
			
		} else {

			// if signed, the MSB of data[0] is the sign.
			// if unsigned, but a 0-byte in front to make it unsiged
			
			int bytes_needed = size/8 + (signed ? 0 : 1);
			data = new byte[bytes_needed];
			int out_offset = signed ? 0 : 1;

			for (int i = 0; i < size/8; i++) {
				data[out_offset+i] = (byte) bin.intBitsAt(bit_pos, 8);
				bit_pos += 8;
			}
			
		}
		
		
		BigInteger bi = new BigInteger(data);
		return ERT.box(bi);
	}

	public EBitString bs_match_string(int bits, EBitString ebs) {
		long size = ebs.bitSize();

		// do we have bits enough in the input
		if (size > bitsLeft()) {
			return null;
		}

		long limit = size;

		for (int pos = 0; pos < limit; pos += 8) {

			int rest = (int) Math.min(8, limit - pos);

			int oc1 = 0xff & bin.intBitsAt(pos, rest);
			int oc2 = 0xff & ebs.intBitsAt(pos, rest);

			if (oc1 != oc2)
				return null;
		}

		return ebs;

	}

	public EObject bs_skip_bits2(EObject count_o, int bits, int flags) {
		EInteger count;
		if ((count = count_o.testInteger()) == null) {
			// throw badarg?
			return null;
		}
		
		int bitsWanted = bits * count.intValue();

		if (bitsLeft() < bitsWanted) {
			return null;
		}

		bit_pos += bitsWanted;

		return ERT.TRUE;
	}
	
	public EObject bs_test_unit(int size) {
		throw new NotImplemented();
	}

	/** yields TRUE if we are at the end */
	public EObject bs_test_tail2() {
		if (bit_pos == bin.bitSize())
			return ERT.TRUE;
		return null;
	}
}
