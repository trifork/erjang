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

import erjang.m.erlang.erlang$bifs;

public abstract class ECons extends EObject {

	@Override
	int cmp_order() {
		return 7;
	}
	
	@Override
	int compare_same(EObject rhs) {
		ECons other = rhs.testCons();
		int cmp1 = head().compareTo(other.head());
		if (cmp1 != 0) return cmp1;
		return tail().compareTo(other.tail());
	}

	public static final ENil NIL = new ENil();

	public abstract EObject head();
	public abstract EObject tail();
	
	public ECons testCons() {
		return this;
	}

	// generic count function that also works for non-well-formed lists
	public int count() {
		EObject cell = tail();
		int count = 1;
		while (cell != NIL && (cell instanceof ECons)) {
			cell = ((ECons)cell).tail();
			count += 1;
		}
		return count;
	}
	/**
	 * @param c1
	 * @return
	 */
	public ECons prepend(ECons list) {
		if (list.testNil() !=null) { return this; }
		return prepend(list.tail()).cons(list.head());
	}

	private ECons prepend(EObject o) {
		if (o.testNil() !=null) { return this; }
		ECons list = o.testCons();
		if (list == null) { throw ERT.badarg(); }
		return prepend(list.tail()).cons(list.head());
	}
	
}

