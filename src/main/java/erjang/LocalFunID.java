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

package erjang;

public class LocalFunID extends FunID {

	final int index;
	final int new_index;
	final int uniq;
	final EBinary new_uniq;

	public LocalFunID(EAtom module, EAtom function, int arity,
			int index, int new_index, 
			int uniq, EBinary new_uniq) {
		super(module, function, arity);
		
		this.index = index;
		this.new_index = new_index;
		this.uniq = uniq;
		this.new_uniq = new_uniq;
	}

	
}
