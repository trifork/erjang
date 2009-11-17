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

public abstract class ENumber extends ETerm {

	public abstract int intValue();

	public static ENumber parseInt(String str) {
        try {     
        	int val = java.lang.Integer.parseInt(str);
        	return new EInteger(val);
         } catch (NumberFormatException e) {
        	 BigInteger val = new java.math.BigInteger(str);
        	 return new EBig(val);
         }
	}

	public ENumber testNumber() {
		return this;
	}


	public abstract ENumber asb();

	public ENumber minus(ENumber n2) {
		throw new NotImplemented();
	}

	public ENumber trunc() {
		throw new NotImplemented();
	}

	public ENumber rem(ENumber n2) {
		throw new NotImplemented();
	}

	public ENumber rem(int v2) {
		throw new NotImplemented();
	}

	public ENumber minus(int v2) {
		throw new NotImplemented();
	}

	public ENumber add(ENumber n2) {
		throw new NotImplemented();
	}

	public ENumber multiply(ENumber n2) {
		throw new NotImplemented();
	}

	public static ENumber valueFor(long res) {
		int intres = (int) res;
		if (res == intres) return new EInteger(intres);
		return new EBig(res);
	}

	public ENumber bitAnd(ENumber n2) {
		throw new NotImplemented();
	}

	public ENumber bitShiftRight(ENumber n2) {
		throw new NotImplemented();
	}

	public ENumber bitShiftLeft(ENumber n2) {
		throw new NotImplemented();
	}

	public ENumber bitOr(ENumber n2) {
		throw new NotImplemented();
	}

	public ENumber div(int v2) {
		throw new NotImplemented();
	}

	public ENumber add(int i2) {
		throw new NotImplemented();
	}

	/**
	 * @param v2
	 * @return
	 */
	public ENumber divide(EObject v2) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @param d2
	 * @return
	 */
	public ENumber divide(double d2) {
		throw new NotImplemented();
	}

}
