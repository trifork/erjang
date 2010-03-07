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

public class AnonFun {
	public final EAtom mod, fun;
	public final int total_arity, free_vars;
	public final int label;
	public final int occur_nr;
	public final int uniq;

	public AnonFun(EAtom mod, EAtom fun, int total_arity,
				   int free_vars, int label, int occur_nr, int uniq)
	{
		this.mod=mod;
		this.fun=fun;
		this.total_arity=total_arity;
		this.free_vars=free_vars;
		this.label=label;
		this.occur_nr=occur_nr;
		this.uniq=uniq;
	}

	public String toString() {
		return fun+"/"+total_arity;
	}

	public ETuple toSymbolic() {
		return ETuple.make(mod, fun, new ESmall(total_arity));
	}
}
