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
 * 
 */
public class ErlangRaise extends ErlangException {

	private final EAtom exClass;
	private final ESeq trace;

	/**
	 * @param reason
	 */
	public ErlangRaise(EAtom ex_class, EObject reason, ESeq trace) {
		super(reason);
		this.exClass = ex_class;
		this.trace = trace;
	}

	/* (non-Javadoc)
	 * @see erjang.ErlangException#reason()
	 */
	@Override
	public EObject reason() {
		if (exClass == am_error) {
			return ETuple.make(super.reason(), getTrace());
		} else {
			return super.reason();
		}
	}
	
	@Override
	public EAtom getExClass() {
		return exClass;
	}
	
	@Override
	public ESeq getTrace() {
		return trace;
	}

    @Override
	public EObject getCatchValue() {
    	if (exClass == am_throw) 
    		return reason();	
    	else
    		return ETuple.make(ERT.am_EXIT, reason());
	}
}
