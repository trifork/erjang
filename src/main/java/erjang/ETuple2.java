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

public class ETuple2 extends ETuple {
	public EObject elem1;
	public EObject elem2;

	/**
	 * @param self
	 * @param amClose
	 */
	public ETuple2(EObject elem1, EObject elem2) {
		this.elem1 = elem1;
		this.elem2 = elem2;
	}

	public ETuple2() {}
	
	static public ETuple2 cast(ETuple value) {
		if (value.arity() == 2)
			return (ETuple2) value;
		return null;
	}

	static public ETuple2 cast(EObject value) {
		ETuple t;
		if ((t = value.testTuple()) != null) {
			if (t.arity() == 2)
				return (ETuple2) t;
		}
		return null;
	}

	@Override
	public ETuple2 blank() {
		ETuple2 res = new ETuple2();
		return res;
	}

	@Override
	public int arity() {
		return 2;
	}

	@Override
	public EObject elm(int i) {
		if (i == 1) {
			return elem1;
		} else if (i == 2) {
			return elem2;
		} else {
			return bad_nth(i);
		}
	}

	@Override
	public void set(int i, EObject term) {
		if (i == 1) {
			elem1 = term;
		} else if (i == 2) {
			elem2 = term;
		} else {
			bad_nth(i);
		}
	}
}
