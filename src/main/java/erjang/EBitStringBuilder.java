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
 * 
 */
public class EBitStringBuilder {

	/**
	 * @param size
	 * @param flags
	 */
	public EBitStringBuilder(int size, int flags) {
		// TODO Auto-generated constructor stub
	}

	/** return bitstring under construction */
	public EBitString bitstring() {
		throw new NotImplemented();
	}
	
	public void put_integer(EObject value, int flags) {
		throw new NotImplemented();
	}

	public void put_integer(EObject value, int size, int flags) {
		throw new NotImplemented();
	}

	public void put_string(EString str) {
		throw new NotImplemented();
	}

	public void put_bitstring(EBitString str, EAtom how, int flags) {
		throw new NotImplemented();
	}
}
