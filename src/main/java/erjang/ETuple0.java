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

public class ETuple0 extends ETuple {

	static public ETuple0 cast(ETuple value) {
		if (value.arity() == 0) return (ETuple0) value;
		return null;
	}
	
	static public ETuple0 cast(EObject value) {
		if (value instanceof ETuple0) return (ETuple0)value; 
		return null;
	}
	
	@Override
	public ETuple0 blank() {
		return new ETuple0();
	}
	
	@Override
	public int arity() {
		return 0;
	}

	@Override
	public EObject elm(int i) {
		return bad_nth(i);
	}
	
	@Override
	public void set(int index, EObject term) {
		bad_nth(index);
	}
}
