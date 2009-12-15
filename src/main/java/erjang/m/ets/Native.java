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

package erjang.m.ets;

import erjang.BIF;
import erjang.EAtom;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;
import erjang.NotImplemented;

/**
 * This class implements the BIFs in ets.
 */
public class Native extends ENative {

	// atoms used in this module are declared using the convention
	// of naming it am_XXX where XXX is the name of the atom (if sensible).

	/** The atom <code>'$end_of_table'</code> */
	public static final EAtom am_$end_of_table = EAtom.intern("$end_of_table");

	/**
	 * Implements <a
	 * href="http://www.erlang.org/doc/man/ets.html#new-2">ets:new/2</a>.
	 * 
	 * @param name
	 *            an atom
	 * @param options
	 *            [Option]
	 */
	@BIF(name = "new")
	public static EObject new$(EObject name, EObject options) {
		// because 'new' is not a valid identifier in Java, we have to provide
		// an erlang-scope name to the the BIF explicitly in the BIF annotation.
		// If the name attribute is omitted, the runtime will use the name of
		// the method as the BIF's name.  
		//
		// BIFs must take EObject as arguments, not more specific
		// types.  They should also be public static, so that calls to
		// it can be embedded directly in compiled code.

		// test argument types
		EAtom aname = name.testAtom();
		ESeq opts = options.testSeq();

		if (aname == null || opts == null) {
			// make sure to pass the original arguments to badarg, not
			// the converted ones. Java 'null' values are never allowed
			// in the erjang world except as a return value from type tests
			// and other guard BIFs.
			throw ERT.badarg(name, options);
		}

		// Use EAtom#getName, not EAtom#toString to get the contents of an atom.
		// EAtom.toString is used for casual printing may enclose the value in
		// quotes
		// as in 'EXIT'.
		String sname = aname.getName();

		// if your BIF is only partially implemented, perhaps because there is
		// an option or variation
		// that is currently not fully implemented throw erjang.NotImplemented.
		throw new NotImplemented();
	}

}
