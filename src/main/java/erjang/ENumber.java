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
import java.util.Set;

import erjang.m.ets.EMatchContext;
import erjang.m.ets.EPattern;
import erjang.m.ets.ETermPattern;

public abstract class ENumber extends EObject {

	@Override
	int cmp_order() {
		return CMP_ORDER_NUMBER;
	}
	
	public boolean match(ETermPattern matcher, EMatchContext r) {
		return matcher.match(this, r);
	}

	@Override
	public ETermPattern compileMatch(Set<Integer> out) {
		return EPattern.compilePattern(this, out);
	}


	public abstract double doubleValue();
	public abstract int intValue();

	public static ENumber parseInt(String str) {
        try {     
        	int val = java.lang.Integer.parseInt(str);
        	return new ESmall(val);
         } catch (NumberFormatException e) {
        	 BigInteger val = new java.math.BigInteger(str);
        	 return new EBig(val);
         }
	}

	public ENumber testNumber() {
		return this;
	}

	public abstract ENumber abs();

	public abstract EInteger asInteger();

	public EDouble divide(double rhs) { 
		return ERT.box(doubleValue() / rhs);
	}


}
