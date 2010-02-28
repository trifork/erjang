/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
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
package erjang.m.re;

import erjang.ENative;
import erjang.BIF;
import erjang.EObject;
import erjang.EAtom;
import erjang.EBinary;
import erjang.ETuple;
import erjang.EList;
import erjang.ERT;
import erjang.NotImplemented;

public class Native extends ENative {
	public static EAtom NOMATCH_ATOM  = EAtom.intern("nomatch");
	public static EAtom MATCH_ATOM    = EAtom.intern("match");

	@BIF
	public static EObject run(EObject subject, EObject pattern) {
		EBinary binsub, binpat;
		// Implement a simple case:
	    if ((binsub = subject.testBinary()) != null &&
			(binpat = pattern.testBinary()) != null &&
			binpat.byteSize() == 1)
		{
			int needle = binpat.octetAt(0);
			int len = binsub.byteSize();
			for (int i=0; i<len; i++) {
				if (binsub.octetAt(i) == needle)
					return ETuple.make(MATCH_ATOM,
									   EList.make(ETuple.make(ERT.box(i),
															  ERT.box(1))));
			}
			return NOMATCH_ATOM;
		}
		throw new NotImplemented();
	}
}
