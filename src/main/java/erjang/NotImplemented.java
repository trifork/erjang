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

public class NotImplemented extends ErlangError {

	static EAtom am_not_implemented = EAtom.intern("not_implemented");
	EObject reason;
	private String message;
	
	/**
	 * 
	 */
	public NotImplemented() {
	 super(ETuple.make(am_not_implemented, ERT.NIL,
			 not_implemented_trace(new Throwable().getStackTrace()) ));
	 this.message = "";
	}
	
	public NotImplemented(String message) {
		super(ETuple.make(am_not_implemented, EString.fromString(message),
				not_implemented_trace(new Throwable().getStackTrace()) ));
		this.message = message;
	}
	
	static ESeq not_implemented_trace (StackTraceElement[] elm) {
		if (elm.length <= 1) return ERT.NIL;
		return EString.fromString(elm[1].toString());
	}
	
	
	public EObject reason() {
		return reason;
	}
	
	@Override
	public String getMessage() {
		return message;
	}
	
}
