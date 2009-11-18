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


package erjang.beam;

import erjang.EAtom;
import erjang.ETuple;

/**
 * 
 */
public class ExtFunc extends erjang.beam.Arg {

	public final EAtom mod;
	public final EAtom fun;
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return mod.toString() + ':' + fun.toString() + "/" + no;
	}

	/**
	 * @param kind
	 * @param reg
	 */
	public ExtFunc(EAtom mod, EAtom fun, int arity) {
		super(Kind.EXT_FUNC, arity);
		this.mod = mod;
		this.fun = fun;

	}

	/**
	 * @param ext
	 */
	public ExtFunc(ETuple ext) {
		super(Kind.EXT_FUNC, ext.elm(3).asInt());
		this.mod = ext.elm(1).testAtom();
		this.fun = ext.elm(2).testAtom();
	}

}
