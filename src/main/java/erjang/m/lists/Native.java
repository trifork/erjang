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


package erjang.m.lists;

import erjang.BIF;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.ETuple;
import erjang.NotImplemented;

public class Native extends ENative {

	@BIF
	public static EObject keyfind(EObject key, EObject nth_arg, EObject list_arg) {
		ESmall nth = nth_arg.testSmall();
		ESeq list = list_arg.testSeq();
		
		if (key == null || nth == null | list == null)
				throw ERT.badarg(key, nth_arg, list_arg);

		while (!list.isNil()) {
			EObject elm = list.head();
			ETuple tup = elm.testTuple();

			// test that it is a tuple of the right size
			if (tup != null && tup.arity() >= nth.value) {				
				EObject val = tup.elm(nth.value);
				if (val.equals(key)) { return tup; }
			}
			
			list = list.tail();
		}

		return ERT.FALSE;
	}
	
}
