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


package erjang.m.os;

import erjang.BIF;
import erjang.EAtom;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;
import erjang.EString;
import erjang.ETuple2;

/**
 * Native methods (BIFs) for the Erlang module 'os'. 
 * 
 * The fact that this class represents the module os is deduced from the package name,
 */
public class Native extends ENative {

	/**
	 * 
	 */
	private static final EAtom am_macosx = EAtom.intern("macosx");
	private static final EAtom am_unix = EAtom.intern("unix");

	private static final ETuple2 OS_TYPE = new ETuple2(am_unix, am_macosx);

	/** os:getenv(string()) -> string() | false */
	@BIF
	public static EObject getenv(EObject o) {
		EString str = o.testString();
		if (str == null) throw ERT.badarg(o);
		
		String value = System.getenv(str.stringValue());
		
		if (value == null) {
			return ERT.FALSE;
		} else {
			return EString.fromString(value);
		}
	}
	
	@BIF
	public static ETuple2 type() {
		return OS_TYPE;
	}
}
