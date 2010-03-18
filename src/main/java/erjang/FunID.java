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
 * This is just a function identifier
 */
public class FunID implements Comparable<FunID> {
	public final EAtom module;
	public final EAtom function;
	public final int arity;

	public FunID(String module, String function, int arity) {
		this(EAtom.intern(module), EAtom.intern(function), arity);
	}

	public FunID(EAtom module, EAtom function, int arity) {
		this.module = module;
		this.function = function;
		this.arity = arity;
	}

	/**
	 * @param annotation
	 */
	public FunID(Import imp) {
		this(imp.module(), imp.fun(), imp.arity());
	}

	/**
	 * @param exp
	 */
	public FunID(Export exp) {
		this(exp.module(), exp.fun(), exp.arity());
	}

	@Override
	public int hashCode() {
		return module.hashCode() + function.hashCode() + arity;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof FunID) {
			FunID spec = (FunID) obj;
			return module.equals(spec.module) && function.equals(spec.function)
					&& arity == spec.arity;
		}
		return false;
	}

	@Override
	public String toString() {
		return module + ":" + function + "/" + arity;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(FunID o) {
		if (module != o.module) {
			int c1 = module.compareTo(o.module);
			if (c1 != 0)
				return c1;
		}

		if (function != o.function) {
			int c2 = function.compareTo(o.function);
			if (c2 != 0)
				return c2;
		}
		
		return arity - o.arity;
	}

}
