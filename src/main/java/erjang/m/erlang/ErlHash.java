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


package erjang.m.erlang;

import erjang.BIF;
import erjang.EInteger;
import erjang.EObject;
import erjang.ERT;
import erjang.ESmall;

/**
 * Erlang hash functions.  !MUST MATCH ERT implementations
 */
public class ErlHash {

	@BIF
	static public ESmall hash(EObject a1, EObject a2)
	{
		ESmall range;
		if ((range=a2.testSmall()) == null || range.value <= 0) throw ERT.badarg(a1,a2);
		
		int hash = make_broken_hash(a1);
		
		return ERT.box(1 + (hash % range.value));
	}

	/**
	 * @param value
	 * @return
	 */
	private static int make_broken_hash(EObject value) {
		// TODO: implement broken_hash (see utils.c in OTP dist)
		return value.hashCode();
	}
	
	static EInteger I2POW32 = ERT.box(0x100000000L);
	
	@BIF
	public static EObject phash(EObject a1, EObject a2)
	{
		long hash, final_hash, range;
		
		if (I2POW32.equalsExactly(a2)) {
			range = 0;
		} else {
			EInteger ival;
			if ((ival=a2.testInteger()) == null 
				|| (range=ival.intValue())==0 
				|| ((range & ~0xffffffffL) != 0)) throw ERT.badarg(a1,a2);
			
			range = Math.abs(range) & 0xffffffff;
		}
		
		hash = make_hash(a1) & 0x7fffffff;
		
		if (range != 0) {
			final_hash = 1 + (hash % range);
		} else {
			final_hash = hash+1;
		}
		
		return ERT.box(final_hash);		
	}
	
	
	/**
	 * @param a1
	 * @return
	 */
	private static int make_hash(EObject value) {
		// TODO: implement hash2 (see utils.c in OTP dist)
		return value.hashCode();
	}

	@BIF
	public static ESmall phash2(EObject value)
	{
		int hash = make_hash2(value) & 0xffffffff;
		return ERT.box(hash & ((1 << 27) - 1));
	}

	/**
	 * @param value
	 * @return
	 */
	private static int make_hash2(EObject value) {
		// TODO: implement hash2 (see utils.c in OTP dist)
		return value.hashCode();
	}

	@BIF
	public static EObject phash2(EObject a1, EObject a2)
	{
		long hash, final_hash, range;
		
		if (I2POW32.equals(a2)) {
			range = 0;
		} else {
			EInteger ival;
			if ((ival=a2.testInteger()) == null 
				|| (range=ival.intValue())==0 
				|| ((range & ~0xffffffffL) != 0)) throw ERT.badarg(a1,a2);
			
			range = Math.abs(range) & 0xffffffff;
		}
		
		hash = make_hash2(a1) & 0xffffffff;
		
		if (range != 0) {
			final_hash = 1 + (hash % range);
		} else {
			final_hash = hash+1;
		}
		
		return ERT.box(final_hash);		
	}
	

	
}
