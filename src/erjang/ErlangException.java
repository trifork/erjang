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

public class ErlangException extends RuntimeException {

	public final EAtom reason;
	private final EAtom module;
	private final EAtom function;
	private final ESeq args;

	public ErlangException(EAtom reason, String module, String function,
			Object[] args, Throwable cause) {
		super(reason.getName(), cause);
		this.reason = reason;
		this.module = EAtom.intern(module);
		this.function = EAtom.intern(function);
		this.args = listify(args);
	}

	public ErlangException(EAtom reason, String module, String function,
			Object[] args) {
		super(reason.getName());
		this.reason = reason;
		this.module = EAtom.intern(module);
		this.function = EAtom.intern(function);
		this.args = listify(args);
	}

	public ErlangException(EAtom reason) {
		super(reason.getName());
		this.reason = reason;
		this.module = null;
		this.function = null;
		this.args = ECons.NIL;
	}

	private ESeq listify(Object[] args) {
		ESeq res = ESeq.NIL;
		for (int i = args.length-1; i >= 0; i--) {
			EObject h = erl_value(args[i]);
			res = res.cons(h);
		}
		return res;
	}

	private EObject erl_value(Object object) {
		// TODO Auto-generated method stub
		return null;
	}

}
