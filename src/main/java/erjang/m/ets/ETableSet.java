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

import clojure.lang.IPersistentMap;
import clojure.lang.PersistentHashMap;
import clojure.lang.PersistentTreeMap;
import erjang.EAtom;
import erjang.EInteger;
import erjang.EObject;
import erjang.EPID;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.ETuple;
import erjang.NotImplemented;

/**
 * 
 */
public class ETableSet extends ETable {

	IPersistentMap map;
	EAtom type;
	
	ETableSet(EProc owner, EAtom type, EInteger tid, EAtom aname, EAtom access, int keypos,
			boolean write_concurrency, boolean is_named, EPID heirPID, EObject heirData) {
		super(owner, tid, aname, access, keypos, is_named, heirPID, heirData);
		
		this.type = type;
		if (type == Native.am_set) {
			map = PersistentHashMap.EMPTY;
		} else if (type == Native.am_ordered_set) {
			map = PersistentTreeMap.EMPTY;
		}
	}
	
	@Override
	EAtom type() {
		return type;
	}

	@Override
	int size() {
		return map.count();
	}
	
	@Override
	protected void insert_one(ETuple value) {
		map = map.assoc(get_key(value), value);
	}

	@Override
	protected void insert_many(ESeq values) {
		IPersistentMap res = map;
		
		while (!values.isNil()) {
			EObject value = values.head();
			res = res.assoc(get_key(value), value);
			values = values.tail();
		}
		
		map = res;
	}
	
	@Override
	protected EObject lookup(EObject key) {
		ESeq res = ERT.NIL;
		
		EObject val = (EObject) map.valAt(key);
		if (val != null) {
			return res.cons(val);
		} else {
			return res;
		}
	}

	@Override
	protected void insert_new_many(ESeq values) {
		throw new NotImplemented();
	}

	@Override
	protected void insert_new_one(ETuple value) {
		throw new NotImplemented();
	}

}
