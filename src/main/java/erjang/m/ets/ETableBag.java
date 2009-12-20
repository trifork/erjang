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

import clojure.lang.IPersistentCollection;
import clojure.lang.IPersistentMap;
import clojure.lang.ISeq;
import clojure.lang.PersistentHashMap;
import clojure.lang.PersistentHashSet;
import clojure.lang.PersistentList;
import clojure.lang.Ref;
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
				heirData, PersistentHashMap.EMPTY);
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
			: PersistentList.EMPTY;
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
	protected EObject lookup(EObject key) {
		IPersistentMap ipm = deref();
		IPersistentCollection set = (IPersistentCollection) ipm.valAt(key);
		ESeq res = ERT.NIL;
		for(ISeq s = set.seq(); s != null; s = s.next())
		{
			res = res.cons((EObject) s.first());
		}
		return res;
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
	protected EInteger select_delete(EMatchSpec matcher) {
		throw new NotImplemented();
	}

}
