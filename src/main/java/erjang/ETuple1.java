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

public class ETuple1 extends ETuple {
	public EObject elem1;
	
	public ETuple1() {
	}

	public ETuple1(EObject elem1) {
		this.elem1 = elem1;
	}

	static public ETuple1 cast(ETuple value) {
		if (value.arity() == 1) return (ETuple1) value;
		return null;
	}
	
	static public ETuple1 cast(EObject value) {
		if (value instanceof ETuple1) return (ETuple1)value; 
		return null;
	}
	
	@Override
	public ETuple1 blank() {
		ETuple1 res = new ETuple1();
		return res;
	}

	@Override
	public int arity() {
		return 1;
	}

	@Override
	public void set(int index, EObject term) {
		if (index==1) {
			elem1 = term;
			return;
		}
		bad_nth(index);
	}

	@Override
	public EObject elm(int i) {
		if(i==1) return elem1; 
		return bad_nth(i);
	}

}
