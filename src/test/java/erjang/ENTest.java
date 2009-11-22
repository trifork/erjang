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

import junit.framework.TestCase;

/**
 * 
 */
public class ENTest extends TestCase {

	

	public void testAdd() {
		tryAdd(1, 1);
		tryAdd(Integer.MIN_VALUE, 0);
		tryAdd(Integer.MIN_VALUE, -1);
		tryAdd(Integer.MIN_VALUE, Integer.MIN_VALUE);
		tryAdd(Integer.MAX_VALUE, Integer.MAX_VALUE);
		tryAdd(Integer.MAX_VALUE, 0);
		tryAdd(Integer.MAX_VALUE, -1);
		tryAdd(Integer.MAX_VALUE, 1);
	}
	

	public void testMult() {
		tryMult(1, 1);
		tryMult(Integer.MIN_VALUE, 0);
		tryMult(Integer.MIN_VALUE, -1);
		tryMult(Integer.MIN_VALUE, Integer.MIN_VALUE);
		tryMult(Integer.MAX_VALUE, Integer.MAX_VALUE);
		tryMult(Integer.MAX_VALUE, 0);
		tryMult(Integer.MAX_VALUE, -1);
		tryMult(Integer.MAX_VALUE, 1);
	}
	
	void tryAdd(int i1, int i2) {
		assertEquals(slow_add(i1, i2), EN.add(i1, i2));
	}
	
	void tryMult(int i1, int i2) {
		assertEquals(slow_mul(i1, i2), EN.mult(i1, i2));
	}
	
	ENumber slow_add(int i1, int i2) {
		
		long l = (long)i1 + (long)i2;
		if (l >= Integer.MIN_VALUE && l <= Integer.MAX_VALUE) 
			return new ESmall((int)l);
		else
			return new EBig(l);
	}

	static BigInteger INT_MIN = BigInteger.valueOf(Integer.MIN_VALUE);
	static BigInteger INT_MAX = BigInteger.valueOf(Integer.MAX_VALUE);
	
	ENumber slow_mul(int i1, int i2) {
		BigInteger b1 = BigInteger.valueOf(i1);
		BigInteger b2 = BigInteger.valueOf(i2);
		BigInteger res = b1.multiply(b2);
		
		if (res.compareTo(INT_MIN) < 0) 
			return new EBig(res);
		
		if (res.compareTo(INT_MAX) > 0)
			return new EBig(res);

		assertEquals(res, BigInteger.valueOf(res.intValue()));
		
		return new ESmall(res.intValue());
	}
}
