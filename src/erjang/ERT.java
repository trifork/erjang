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

public class ERT {

	public static final EAtom AM_BADARG = EAtom.intern("badarg");
	public static final EAtom AM_BADMATCH = EAtom.intern("badmatch");
	public static final EAtom AM_BADARITH = EAtom.intern("badarith");

	public static ETerm cons(EObject h, EObject t) {
		return t.cons(h);
	}

	public static ErlangException badarith(ArithmeticException cause,
			String module, String function, Object... args) {
		throw new ErlangException(AM_BADARITH, module, function, args, cause);
	}

	public static ErlangException badarith(String module, String function, Object... args) {
		throw new ErlangException(AM_BADARITH, module, function, args);
	}

	public static ErlangException badarg() {
		throw new ErlangException(AM_BADARG);
	}


	public static ErlangException badarg(String module,
			String function, Object... args) {
			throw new ErlangException(AM_BADARG, module, function, args);
	}

	public static ErlangException badarg(Throwable cause, String module,
			String function, Object... args) {
			throw new ErlangException(AM_BADARG, module, function, args, cause);
	}

	public static ErlangException badarg(EObject arg1, int arg2) {
		throw new ErlangException(AM_BADARG);
	}

	public static final EAtom ATOM_TRUE = EAtom.intern("true");
	public static final EAtom ATOM_FALSE = EAtom.intern("false");
	
	public static boolean eq(EObject o1, EObject o2) { return o1==null?o2==null:o1.equals(o2); }
	public static boolean eq(EObject o1, EAtom o2) { return o1==o2; }
	public static boolean eq(EAtom o1, EObject o2) { return o1==o2; }
	public static boolean eq(EAtom o1, EAtom o2) { return o1==o2; }

}
