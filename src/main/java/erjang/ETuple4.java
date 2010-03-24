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

public class ETuple4 extends ETuple {

	public EObject elem1;
	public EObject elem2;
	public EObject elem3;
	public EObject elem4;

	static public ETuple4 cast(ETuple value) {
		if (value.arity() == 4) return (ETuple4) value;
		return null;
	}
	
	static public ETuple4 cast(EObject value) {
		if (value instanceof ETuple4) return (ETuple4)value; 
		return null;
	}
	
	@Override
	public ETuple4 blank() {
		ETuple4 res = new ETuple4();
		return res;
	}

	@Override
	public int arity() {
		return 4;
	}

	@Override
	public EObject elm(int i) {
		switch (i) {
		case 1:
			return elem1;
		case 2:
			return elem2;
		case 3:
			return elem3;
		case 4:
			return elem4;
		default:
			return bad_nth(i);
		}
	}

	@Override
	public void set(int i, EObject term) {
		switch (i) {
		case 1:
			elem1 = term;
			break;
		case 2:
			elem2 = term;
			break;
		case 3:
			elem3 = term;
			break;
		case 4:
			elem4 = term;
			break;
		default:
			bad_nth(i);
		}
	}
}
