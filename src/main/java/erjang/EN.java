package erjang;

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

/**
 * 
 */
public class EN {

	static EDouble box(double d) {
		return new EDouble(d);
	}
	
	static EInteger box(long l) { 
		
		long offset_from_int_min  = l-(long)Integer.MIN_VALUE;
		long unsigned_offset = offset_from_int_min & Long.MAX_VALUE;
		
		if (unsigned_offset >= 0x100000000L) {
			return new EBig(l);
		} else {
			return new ESmall((int) l);
		}
		
	}
	
	static ENumber add(int i1, int i2) {
		long l = (long)i1 + (long)i2;
		
		return box(l);		
	}
	
	static ENumber mult(int i1, int i2) {
		long l = (long)i1 * (long)i2;
		
		return box(l);		
	}
	
	
	
}
