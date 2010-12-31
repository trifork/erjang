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
import erjang.EObject;
import erjang.ECons;
import erjang.ESeq;
import erjang.ERT;

import java.util.LinkedList;

/**
 * 
 */
public class ErlList {

	@BIF(name="--")
	public static ECons listDiff(EObject a1, EObject a2) {
		ESeq l1, l2;
		if ((l1 = a1.testSeq()) == null ||
			(l2 = a2.testSeq()) == null)
			throw ERT.badarg(a1,a2);

		if (l1.isNil()) return l1;

		// Step 1: Copy l1 to a temporary place.
		final EObject[] tmp = l1.toArray();

		// Step 2: Delete elements occurring l2 (but only once)
		int tmp_start = 0;
		for (ESeq cur = l2; !cur.isNil(); cur=cur.tail()) {
			EObject elm = cur.head();
			for (int i=tmp_start; i<tmp.length; i++) {
				if (tmp[i] != null && tmp[i].equalsExactly(elm)) {
					// Delete element
					tmp[i] = null;

					// Update tmp_start
					if (i==tmp_start) {
						do {
							tmp_start++;
						} while (tmp_start<tmp.length && tmp[tmp_start]==null);
						if (tmp_start == tmp.length) // No elements left.
							return ERT.NIL;
					}
					break; // Delete only one.
				}
			}
		}

		// Step 3: Convert back into list
		return ESeq.fromArraySkippingNulls(tmp, tmp_start, tmp.length);
	}
}
