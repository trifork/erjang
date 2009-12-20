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

import java.util.Map;

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

	ETableSet(EProc owner, EAtom type, EInteger tid, EAtom aname, EAtom access, int keypos,
			boolean write_concurrency, boolean is_named, EPID heirPID, EObject heirData) {
		super(owner, type, tid, aname, access, keypos, 
				is_named, heirPID, heirData, 
				type == Native.am_set 
						? PersistentHashMap.EMPTY 
						: PersistentTreeMap.EMPTY);
		
		
	}
	
	@Override
	int size() {
		return deref().count();
	}

	@Override
	protected void insert_one(final ETuple value) {
		in_tx(new WithMap<Object>() {
			@Override
			protected Object run(IPersistentMap map) {
				set(map.assoc(get_key(value), value));
				return null;
			}
		});
	}


	@Override
	protected void insert_many(final ESeq values) {
		in_tx(new WithMap<Object>() {
			@Override
			protected Object run(IPersistentMap map) {		
				for (ESeq seq = values; !seq.isNil(); seq = seq.tail()) {
					ETuple value = values.head().testTuple();
					if (value == null) throw ERT.badarg(values);
					map = map.assoc(get_key(value), value);
				}
				set(map);
				return null;
			}});
	}
	
	@Override
	protected EObject lookup(EObject key) {
		ESeq res = ERT.NIL;
		
		// no need to run in_tx if we're only reading
		EObject val = (EObject) deref().valAt(key);
		if (val != null) {
			return res.cons(val);
		} else {
			return res;
		}
	}

	@Override
	protected void insert_new_many(final ESeq values) {	
		in_tx(new WithMap<Object>() {
			@Override
			protected Object run(IPersistentMap map) {
				for (ESeq seq = values; !seq.isNil(); seq = seq.tail()) {
					ETuple value = values.head().testTuple();
					if (value == null) throw ERT.badarg(values);
					EObject key = get_key(value);
					if (!map.containsKey(key)) {
						map = map.assoc(key, value);
					}
				}
	
				set(map);
				return null;
			}
	});
}

	@Override
	protected void insert_new_one(final ETuple value) {
		final EObject key = get_key(value);
		if (!deref().containsKey(key)) {
			in_tx(new WithMap<Object>() {
				@Override
				protected Object run(IPersistentMap map) {
					if (!map.containsKey(key)) {
						set(map.assoc(key, value));
					}
					return null;
				}
			});
		}
	}
	
	@Override
	public ESeq match(ECompiledMatchSpec matcher) {		
		IPersistentMap map = deref();
		ESeq res = ERT.NIL;
		
		EObject key = matcher.getKey(keypos1);
		if (key == null) {
			res = matcher.match(res, (Map<EObject, ETuple>) map);
		} else {
			ETuple candidate = (ETuple) map.valAt(key);
			if (candidate != null) {
				res =  matcher.match(res, candidate);
			}
		}
		
		return res;
	}

	@Override
	public EInteger select_delete(final ECompiledMatchSpec matcher) {		
		int delete_count = in_tx(new WithMap<Integer>() {

			@Override
			protected Integer run(IPersistentMap map) {
				// TODO Auto-generated method stub
				ESeq res = ERT.NIL;
				
				EObject key = matcher.getKey(keypos1);
				
				if (key == null) {
					res = matcher.match_members(res, (Map<EObject, ETuple>) map);
				} else {
					ETuple candidate = (ETuple) map.valAt(key);
					if (candidate != null && matcher.match(candidate)) {
						res = res.cons(candidate);
					}
				}
				
				int count = 0;
				for (; !res.isNil(); res = res.tail()) {
					try {
						map = map.without(res.head());
					} catch (Exception e) {
						// should not happen!
						throw new Error(e);
					}
					count += 1;
				}
				
				set(map);
				return count;
			}});
		
		return ERT.box(delete_count);
	}
}
