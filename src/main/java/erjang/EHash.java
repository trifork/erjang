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

/**
 * Utility functions and constants for erlang hashing.
 * 
 * This class should replicate the hashing logic in beam/util.c
 * 
 */
public class EHash {

	static final int BINARY_DEF = 0x0;
	static final int LIST_DEF = 0x1;
	static final int NIL_DEF = 0x2;
	static final int TUPLE_DEF = 0x3;
	static final int PID_DEF = 0x4;
	static final int EXTERNAL_PID_DEF = 0x5;
	static final int PORT_DEF = 0x6;
	static final int EXTERNAL_PORT_DEF = 0x7;
	static final int EXPORT_DEF = 0x8;
	static final int FUN_DEF = 0x9;
	static final int REF_DEF = 0xa;
	static final int EXTERNAL_REF_DEF = 0xb;
	static final int ATOM_DEF = 0xc;
	static final int FLOAT_DEF = 0xd;
	static final int BIG_DEF = 0xe;
	static final int SMALL_DEF = 0xf;

	/* some prime numbers just above 2 ^ 28 */

	static final int FUNNY_NUMBER1 = 268440163;
	static final int FUNNY_NUMBER2 = 268439161;
	static final int FUNNY_NUMBER3 = 268435459;
	static final int FUNNY_NUMBER4 = 268436141;
	static final int FUNNY_NUMBER5 = 268438633;
	static final int FUNNY_NUMBER6 = 268437017;
	static final int FUNNY_NUMBER7 = 268438039;
	static final int FUNNY_NUMBER8 = 268437511;
	static final int FUNNY_NUMBER9 = 268439627;
	static final int FUNNY_NUMBER10 = 268440479;
	static final int FUNNY_NUMBER11 = 268440577;
	static final int FUNNY_NUMBER12 = 268440581;

	static final int HCONST = 0x9e3779b9;

	/** from util.c:934 */
	int block_hash(byte[] k, int length, int initval) {
		int a, b, c;
		int len;

		/* Set up the internal state */
		len = length;
		a = b = HCONST;
		c = initval; /* the previous hash value */

		int p = 0;
		while (len >= 12) {
			a += (uint(k[p + 0]) + uint(k[p + 1]) << 8 + uint(k[p + 2]) << 16 + uint(k[p + 3]) << 24);
			b += (uint(k[p + 4]) + uint(k[p + 5]) << 8 + uint(k[p + 6]) << 16 + uint(k[p + 7]) << 24);
			c += (uint(k[p + 8]) + uint(k[p + 9]) << 8 + uint(k[p + 10]) << 16 + uint(k[p + 11]) << 24);

			// inlined MIX because we don't have macros
			a -= b;
			a -= c;
			a ^= (c >>> 13);
			b -= c;
			b -= a;
			b ^= (a << 8);
			c -= a;
			c -= b;
			c ^= (b >>> 13);
			a -= b;
			a -= c;
			a ^= (c >>> 12);
			b -= c;
			b -= a;
			b ^= (a << 16);
			c -= a;
			c -= b;
			c ^= (b >>> 5);
			a -= b;
			a -= c;
			a ^= (c >>> 3);
			b -= c;
			b -= a;
			b ^= (a << 10);
			c -= a;
			c -= b;
			c ^= (b >>> 15);

			p += 12;
			len -= 12;
		}

		c += length;
		switch (len) /* all the case statements fall through */
		{
		case 11:
			c += uint(k[p + 10]) << 24;
		case 10:
			c += uint(k[p + 9]) << 16;
		case 9:
			c += uint(k[p + 8]) << 8;
			/* the first byte of c is reserved for the length */
		case 8:
			b += uint(k[p + 7]) << 24;
		case 7:
			b += uint(k[p + 6]) << 16;
		case 6:
			b += uint(k[p + 5]) << 8;
		case 5:
			b += uint(k[4]);
		case 4:
			a += uint(k[p + 3]) << 24;
		case 3:
			a += uint(k[p + 2]) << 16;
		case 2:
			a += uint(k[p + 1]) << 8;
		case 1:
			a += uint(k[p + 0]);
			/* case 0: nothing left to add */
		}

		c = mix(a, b, c);

		return c;
	}

	static final int uint(byte b) {
		return ((int) b) & 0xff;
	}

	static final int mix(int a, int b, int c) {
		a -= b;
		a -= c;
		a ^= (c >>> 13);
		b -= c;
		b -= a;
		b ^= (a << 8);
		c -= a;
		c -= b;
		c ^= (b >>> 13);
		a -= b;
		a -= c;
		a ^= (c >>> 12);
		b -= c;
		b -= a;
		b ^= (a << 16);
		c -= a;
		c -= b;
		c ^= (b >>> 5);
		a -= b;
		a -= c;
		a ^= (c >>> 3);
		b -= c;
		b -= a;
		b ^= (a << 10);
		c -= a;
		c -= b;
		c ^= (b >>> 15);
		return c;
	}

	/* (HCONST * {2, ..., 14}) mod 2^32 */
	static final int HCONST_2 = 0x3c6ef372;
	static final int HCONST_3 = 0xdaa66d2b;
	static final int HCONST_4 = 0x78dde6e4;
	static final int HCONST_5 = 0x1715609d;
	static final int HCONST_6 = 0xb54cda56;
	static final int HCONST_7 = 0x5384540f;
	static final int HCONST_8 = 0xf1bbcdc8;
	static final int HCONST_9 = 0x8ff34781;
	static final int HCONST_10 = 0x2e2ac13a;
	static final int HCONST_11 = 0xcc623af3;
	static final int HCONST_12 = 0x6a99b4ac;
	static final int HCONST_13 = 0x08d12e65;
	static final int HCONST_14 = 0xa708a81e;
	static final int HCONST_15 = 0x454021d7;

	static final int HCONST_NIL = (int) 3468870702L;

	static final int UINT32_HASH_2(int expr1, int expr2, int aconst, int hash) {
		int a, b;
		a = aconst + expr1;
		b = aconst + expr2;
		return mix(a, b, hash);
	}

	static final int UINT32_HASH(int expr, int aconst, int hash) {
		return UINT32_HASH_2(expr, 0, aconst, hash);
	}

	static final int SINT32_HASH(int expr, int aconst, int hash) {
		if (expr < 0) {
			return UINT32_HASH(-expr, aconst, hash);
		} else {
			return UINT32_HASH(expr, aconst, hash);
		}
	}

	public static int hash2(EAtom val, int hash) {
		if (hash == 0) {
			return val.hash;
		} else {
			return UINT32_HASH(val.hash, HCONST_3, hash);
		}
	}

	public static int hash2(ESmall val, int hash) {
		return SINT32_HASH(val.value, HCONST, hash);
	}

	public static int hash2(EString val, int hash) {
		int c = 0;
		int sh = 0;
		for (int p = 0; p < val.length(); p++) {
			sh = (sh << 8) + val.charAt(p);
			if (c == 3) {
				hash = UINT32_HASH(sh, HCONST_4, hash);
				c = sh = 0;
			} else {
				c++;
			}
		}
		if (c > 0) {
			hash = UINT32_HASH(sh, HCONST_4, hash);
		}
		return hash;
	}

	public static int hash2(ERef ref, int hash) {
		/* Only 15 bits are hashed. */
		return UINT32_HASH(ref.internal_ref_numbers()[0], HCONST_7, hash);
	}

	public static int hash2(EInternalPort port, int hash) {
		/* Only 15 bits are hashed. */
		return UINT32_HASH(port.internal_port_number(), HCONST_6, hash);
	}

	public static int hash2(EInternalPID pid, int hash) {
		/* Only 15 bits are hashed. */
		return UINT32_HASH(pid.internal_pid_number(), HCONST_5, hash);
	}

	public static int hash2(ENil nil, int hash) {
		if (hash == 0) {
			return HCONST_NIL;
		} else {
			return UINT32_HASH(NIL_DEF, HCONST_2, hash);
		}
	}

}
