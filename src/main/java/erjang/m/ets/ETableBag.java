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

import clojure.lang.IMapEntry;
import clojure.lang.IPersistentCollection;
import clojure.lang.IPersistentMap;
import clojure.lang.IPersistentSet;
import clojure.lang.ISeq;
import clojure.lang.PersistentHashSet;
import clojure.lang.PersistentTreeMap;
import clojure.lang.RT;
import clojure.lang.Ref;
import clojure.lang.Var;
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
public class ETableBag extends ETable {

	static {
		Var in = RT.IN;
	}
	
	/** holds an Integer with the bag size */
	Ref sizeRef;
	
	ETableBag(EProc owner, 
			  EAtom type, 
			  EInteger tid, 
			  EAtom aname, 
			  EAtom access,
			  int keypos, 
			  boolean writeConcurrency,
			  boolean isNamed, 
			  EPID heirPid, 
			  EObject heirData)
	{
		super(owner, type, tid, aname, access, keypos, isNamed, heirPid,
				heirData, PersistentTreeMap.EMPTY);
		try {
			sizeRef = new Ref(new Integer(0));
		} catch (Exception e) {
			throw new Error(e);
		}
	}
	
	//
	// A bag is implemented as a map (of keys), that refer to 
	// a collection of values with that key.  This function creates
	// an empty one of those nested collections depending on
	// the ets type.
	//
	
	IPersistentCollection empty() {
		return type==Native.am_bag 

			// normal bag; there can be several elements with the
			// same key, but no duplicates (i.e. == each other)
			? PersistentHashSet.EMPTY 
					
			// duplicate bag, allows duplicate elements with the
			// same key; just hold values as a list
			: PersistentBag.EMPTY;
	}

	@Override
	protected void insert_many(final ESeq values) {
		in_tx(new WithMap<Object>() {
			@Override
			protected Object run(IPersistentMap map) {
				int count = 0;
				for (ESeq seq = values; !seq.isNil(); seq = seq.tail()) {
					ETuple value = seq.head().testTuple();
					if (value == null) throw ERT.badarg(values);
					EObject key = get_key(value);
					IPersistentCollection c = 
						(IPersistentCollection) map.valAt(key, empty());
					map = map.assoc(key, c.cons(value));
					count += 1;
				}
				sizeRef.set(count + (Integer)sizeRef.deref());
				set(map);
				return null;
			}
		});
	}

	@Override
	protected void insert_new_many(ESeq values) {
		throw new NotImplemented();
	}

	@Override
	protected void insert_new_one(ETuple value) {
		throw new NotImplemented();
	}

	@Override
	protected void insert_one(final ETuple value) {
		in_tx(new WithMap<Object>() {
			@Override
			protected Object run(IPersistentMap map) {
				IPersistentCollection empty = empty();
				EObject key = get_key(value);
				IPersistentCollection c = 
					(IPersistentCollection) map.valAt(key, empty);
				map = map.assoc(key, c.cons(value));
				set(map);
				sizeRef.set(1 + (Integer)sizeRef.deref());
				return null;
			}
		});
	}

	/** return a list of elements at given key */
	@Override
	protected ESeq lookup(EObject key) {
		IPersistentMap ipm = deref();
		IPersistentCollection set = (IPersistentCollection) ipm.valAt(key);
		ESeq res = ERT.NIL;
		if (set == null) return res;
		for(ISeq s = set.seq(); s != null; s = s.next())
		{
			res = res.cons((EObject) s.first());
		}
		return res;
	}

	@Override
	protected EObject first() {
		// no need to run in_tx if we're only reading
		IPersistentMap map = deref();
		
		if (map.count() == 0) {
			return Native.am_$end_of_table;
		} else {
			ISeq entseq = map.seq();
			IMapEntry ent = (IMapEntry) entseq.first();
			return (EObject) ent.getKey();
		}
	}
	
	@Override
	int size() {
		return (Integer)sizeRef.deref();
	}

	@Override
	public ESeq match(EPattern matcher) {		
		throw new NotImplemented();
	}

	@Override
	public ESeq match_object(EPattern matcher) {		
		throw new NotImplemented();
	}

	@Override
	protected void delete(final EObject key) {
		in_tx(new WithMap<Object>() {
			@Override
			protected Object run(IPersistentMap map) {
				IPersistentCollection empty = empty();
				IPersistentCollection c =
					(IPersistentCollection) map.valAt(key, empty);
				try {
				    map = map.without(key);
				} catch (Exception e) {
				    // should not happen!
				    throw new Error(e);
				}
				set(map);
				sizeRef.set((Integer)sizeRef.deref() - c.count());
				return null;
			}
		});
	}

	@Override
	protected void delete_object(final ETuple obj) {
		in_tx(new WithMap<Object>() {
			@Override
			protected Object run(IPersistentMap map) {
				EObject key = get_key(obj);
				IPersistentCollection empty = empty();
				IPersistentCollection c =
					(IPersistentCollection) map.valAt(key, empty);
				
				if (c == null || c.count()==0)
					return null;
				
				IPersistentCollection out = empty();
				int deleted = 0;
				
				for (ISeq s = c.seq(); s != null; s = s.next()) {
					EObject val = (EObject) s.first();
					if (val == null) break;
					
					if (! obj.equals(val)) {
						out = out.cons(val);
					} else {
						deleted += 1;
					}
				}
				
				if (out.count() == 0) {
					try {
						map = map.without(key);
					} catch (Exception e) {
						throw new Error(e);
					}
				} else {
					map = map.assoc(key, out);
				}
				
				set(map);
				sizeRef.set((Integer)sizeRef.deref() - deleted);
				return null;
			}
		});

	
	}
	
	@Override
	protected EInteger select_delete(final EMatchSpec matcher) {

		int delete_count = in_tx(new WithMap<Integer>() {

			@Override
			protected Integer run(IPersistentMap map) {
				ESeq vals = ERT.NIL;
				int initial_count = (Integer) sizeRef.deref();
				
				EObject key = matcher.getTupleKey(keypos1);
				
				if (key == null) {
					vals = matcher.matching_values_bag(vals, (Map<EObject, java.util.Collection <ETuple>>) map);
				} else {
					ETuple candidate = (ETuple) map.valAt(key);
					if (candidate != null && matcher.match(candidate)) {
						vals = vals.cons(key);
					}
				}
				
				int count = 0;
				for (; !vals.isNil(); vals = vals.tail()) {
					try {
						ETuple val = (ETuple) vals.head();
						key = val.elm(keypos1);
						IPersistentCollection coll = (IPersistentCollection) map.valAt(key);

						if (coll instanceof IPersistentSet) {
							IPersistentSet set = (IPersistentSet) coll;
							set = set.disjoin(val);
							if (set != coll) {
								count += 1;
							if (set.count() == 0) {
								map = map.without(key);
							} else {
								map = map.assoc(key, set);
							}
							}
						} else if (coll instanceof IPersistentBag) {
							IPersistentBag bag = (IPersistentBag)coll;
							bag = bag.disjoin(val);
							if (bag != coll) {
								count += 1;
								if (bag.count() == 0) {
								map = map.without(key);
							} else {
								map = map.assoc(key, bag);
							}
							}
							
						}
					} catch (Exception e) {
						// should not happen!
						throw new Error(e);
					}
				}
				
				set(map);
				sizeRef.set(new Integer(initial_count-count));
				return count;
			}});
		
		return ERT.box(delete_count);
	}

	protected void delete_all_objects() {
		in_tx(new WithMap<Object>() {

			@Override
			protected Object run(IPersistentMap map) {
				set(empty);
				sizeRef.set(new Integer(0));
				return null;
			}
		});
	}


}
