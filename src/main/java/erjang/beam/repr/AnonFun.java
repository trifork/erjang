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
import erjang.EBinary;
import erjang.ETuple;
import erjang.ESmall;

public class AnonFun {
	public final EAtom mod, fun;
	public final int total_arity, free_vars;
	public final int label;
	public final int index;
	public final int old_uniq;
	public final int old_index;
	public final EBinary mod_md5;

	public AnonFun(EAtom mod, EAtom fun, int arity, int label,
			int old_uniq, int old_index, EBinary mod_md5, int index, int free_vars)
	{
		this.mod=mod;
		this.fun=fun;
		this.total_arity=arity;
		this.free_vars=free_vars;
		this.label=label;
		this.index=index;
		this.old_index = old_index;
		this.old_uniq=old_uniq;
		this.mod_md5 = mod_md5;
	}

	public ExtFun asExtFun() {
		return new ExtFun(mod, fun, total_arity);
	}

	public String toString() {
		return fun+"/"+total_arity;
	}

	public ETuple toSymbolic() {
		return ETuple.make(mod, fun, new ESmall(total_arity));
	}
}
