/** -*- tab-width: 4 -*-
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
package erjang.beam.repr;

import erjang.EAtom;
import erjang.ETuple;
import erjang.ESmall;
import static erjang.beam.CodeAtoms.EXTFUNC_ATOM;

public class ExtFun {
	public final EAtom mod, fun;
	public final int arity;

	public ExtFun(EAtom mod, EAtom fun, int arity) {
		this.mod=mod; this.fun=fun; this.arity=arity;
	}

	public String toString() {
		return mod+":"+fun+"/"+arity;
	}
	public EAtom module() {return  mod;}
	public EAtom name()   {return  fun;}

	public ETuple toSymbolic() {
		return ETuple.make(EXTFUNC_ATOM, mod, fun, new ESmall(arity));
	}
}
