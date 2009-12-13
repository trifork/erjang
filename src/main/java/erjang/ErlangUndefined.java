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

/**
 * Error throws when a function is undefined
 */
public class ErlangUndefined extends ErlangError {

	private final EAtom module;
	private final EAtom function;
	private final ESmall arity;

	/* (non-Javadoc)
	 * @see erjang.ErlangException#getMessage()
	 */
	@Override
	public String getMessage() {
		return "undefined: " +
			module + ":" + function + "/" + arity;
	}
	
	/**
	 * @param module
	 * @param function
	 * @param make
	 */
	public ErlangUndefined(EAtom module, EAtom function, ESmall arity) {
		super(ERT.am_undef);
		this.module = module;
		this.function = function;
		this.arity = arity;
		
		this.printStackTrace();
	}

	/**
	 * @param mod
	 * @param fun
	 * @param length
	 */
	public ErlangUndefined(EAtom mod, EAtom fun, int length) {
		this(mod, fun, new ESmall(length));
	}

	/* (non-Javadoc)
	 * @see erjang.ErlangError#getTrace()
	 */
	@Override
	public ESeq getTrace() {
		return super.getTrace().cons(ETuple.make(module, function, arity));
	}
}
