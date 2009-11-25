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

public class EBinMatchState {

	public final EBitString bin;
	int bit_pos;

	public EBitString binary() {
		return bin;
	}
	
	public EBinMatchState(EBitString binary) {
		this.bin = binary;
		this.bit_pos = 0;
	}

	public EInteger bs_get_integer2(int size) {
		if (size == 0) {
			return ESmall.ZERO;
		}

		if (size < 0 || bin.bitCount() >= (bit_pos + size)) {
			return null;
		}

		if (size <= 32) {
			ESmall res = new ESmall(bin.intBitsAt(bit_pos, size));
			bit_pos += size;
			return res;
		}

		if (size <= 64) {
			EInteger res = ERT.box(bin.longBitsAt(bit_pos, size));
			bit_pos += size;
			return res;
		}

		throw new NotImplemented();
	}

	public EBinary bs_match_string(int bits, EBinary ebs) {
		int size = ebs.bitCount();

		// do we have bits enough in the input
		if (size > (bin.bitCount() - bit_pos)) {
			return null;
		}

		int limit = ebs.bitCount();

		for (int pos = 0; pos < limit; pos += 8) {

			int rest = Math.min(8, limit - pos);

			int oc1 = 0xff & bin.intBitsAt(pos, rest);
			int oc2 = 0xff & ebs.intBitsAt(pos, rest);

			if (oc1 != oc2)
				return null;
		}

		return ebs;

	}
	
	EAtom bs_skip_bits2(EInteger count, int bits, int flags) {
		bit_pos += (bits * count.intValue());

		if (bit_pos <= bin.bitCount())
			return ERT.TRUE;
		else
			return null;
	}
}
