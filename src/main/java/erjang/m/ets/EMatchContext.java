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


package erjang.m.ets;

import java.util.HashMap;

import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;
import erjang.ETuple;

public class EMatchContext {
	final HashMap<Integer, EObject> vars;
	final EObject value;
	private final Integer[] varnames;
	
	EMatchContext (Integer[] varnames, EObject value) {
		this.varnames = varnames;
		this.vars = new HashMap<Integer,EObject>();
		this.value = value;
	}

	public ESeq makeList() {
		ESeq res = ERT.NIL;
		for (int i = varnames.length-1; i >= 0; i--) {
			res = res.cons(vars.get(varnames[i]));
		}
		return res;
	}

	public EObject makeTuple() {
		ETuple res = ETuple.make(varnames.length);
		for (int i = 0; i < varnames.length; i++) {
			res.set(i+1, vars.get(varnames[i]));
		}
		return res;
	}
}
