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
import java.util.concurrent.atomic.AtomicInteger;

import com.trifork.clj_ds.IMapEntry;
import com.trifork.clj_ds.IPersistentCollection;
import com.trifork.clj_ds.IPersistentMap;
import com.trifork.clj_ds.IPersistentSet;
import com.trifork.clj_ds.ISeq;
import com.trifork.clj_ds.PersistentHashSet;
import com.trifork.clj_ds.PersistentHashMap;
import com.trifork.clj_ds.Seqable;
import erjang.EAtom;
import erjang.ECons;
import erjang.EInteger;
import erjang.EInternalPID;
import erjang.EList;
import erjang.EObject;
import erjang.EProc;
import erjang.EPseudoTerm;
import erjang.ERT;
import erjang.ESeq;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.NotImplemented;

/**
 * 
 */
public class ETableBag extends ETable {

	/** holds an Integer with the bag size */
	AtomicInteger sizeRef;
	
	ETableBag(EProc owner, 
			  EAtom type, 
			  EInteger tid, 
			  EAtom aname, 
			  EAtom access,
			  int keypos, 
			  boolean writeConcurrency,
			  boolean isNamed, 
			  EInternalPID heirPid, 
			  EObject heirData)
	{
		super(owner, type, tid, aname, access, keypos, isNamed, heirPid,
			  heirData, PersistentHashMap.EMPTY);
		try {
			sizeRef = new AtomicInteger(0);
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

					// Insert element - noting whether the map grows
					int sizeBefore = c.count();
					c = c.cons(value);
					int sizeAfter = c.count();
					map = map.assoc(key, c);

					// Update size - if the map grew
					if (sizeAfter > sizeBefore) count += 1;
				}
				sizeRef.set(count + (Integer)sizeRef.get());
				set(map);
				return null;
			}
		});
	}

	@Override
	protected boolean insert_new_many(ESeq values) {
		throw new NotImplemented();
	}

	@Override
	protected boolean insert_new_one(ETuple value) {
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

				// Insert element - noting whether the map grows
				int sizeBefore = c.count();
				c = c.cons(value);
				int sizeAfter = c.count();
				map = map.assoc(key, c);
				set(map);

				// Update size - if the map grew
				Integer cnt0 = (Integer)sizeRef.get();
				if (sizeAfter > sizeBefore)
					sizeRef.set(1 + (Integer)sizeRef.get());

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
		return res.reverse();
	}

	@Override
	protected EAtom member(EObject key) {
		IPersistentMap ipm = deref();
		IPersistentCollection set = (IPersistentCollection) ipm.valAt(key);
		if (set != null && set.count() != 0) {
			return ERT.TRUE;
		} else {
			return ERT.FALSE;
		}
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
		return sizeRef.get();
	}

	@Override
	public ESeq match(EPattern matcher) {		
		
		EObject key = matcher.getKey(keypos1);
		if (key == null) {
			
			// oops, .. tablescan
			ESeq res = ERT.NIL;
			
			IPersistentMap map = deref();
			for (ISeq entseq = map.seq(); entseq != null; entseq = entseq.next()) {
				IMapEntry ent = (IMapEntry) entseq.first();

				if (ent == null) continue;
				
				Seqable coll = (Seqable)ent.getValue();
				res = matcher.match_vars(res, coll.seq());
			}
			
			return res.reverse();
		}
		
		IPersistentMap map = deref();
		IPersistentCollection coll = (IPersistentCollection) map.valAt(key);
		if (coll == null) return ERT.NIL;
		
		return matcher.match_vars(ERT.NIL, coll.seq()).reverse();
	}
	

	@Override
	public ESeq match_object(final EPattern matcher) {		
		
		EObject key = matcher.getKey(keypos1);
		if (key == null) {
			
			// oops, .. tablescan
			ESeq res = ERT.NIL;
			
			IPersistentMap map = deref();
			for (ISeq entseq = map.seq(); entseq != null; entseq = entseq.next()) {
				IMapEntry ent = (IMapEntry) entseq.first();

				if (ent == null) break;
				
				Seqable coll = (Seqable)ent.getValue();
				res = matcher.match_members(res, coll.seq());
			}
			
			return res.reverse();
		}
		
		IPersistentMap map = deref();
		IPersistentCollection coll = (IPersistentCollection) map.valAt(key);
		if (coll == null) return ERT.NIL;
		
		return matcher.match_members(ERT.NIL, coll.seq()).reverse();
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
				sizeRef.addAndGet(- c.count());
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
					
					if (! obj.equalsExactly(val)) {
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
				sizeRef.addAndGet(-deleted);
				return null;
			}
		});

	
	}
	

	@Override
	public EObject select(final EMatchSpec matcher, int limit) {
		
		IPersistentMap map = deref();
		
		EObject key = matcher.getTupleKey(keypos1);
		
		if (key == null) {
			EBagCont cont0 = new EBagCont(matcher, map.seq(), null, limit);
			return cont0.select();
			
		} else {
			IPersistentCollection coll = (IPersistentCollection) map.valAt(key);
			if (coll == null) {
				return Native.am_$end_of_table;				
			} else {
				EBagCont cont0 = new EBagCont(matcher, null, coll.seq(), limit);
				return cont0.select();
			}
		}
		
	}
	
	static class EBagCont extends EPseudoTerm implements ISelectContinuation {

		private final EMatchSpec matcher;
		private final ISeq map_ent;
		private final ISeq coll_ent;
		private final int limit;

		public EBagCont(EMatchSpec matcher, ISeq mapent, ISeq conent, int limit) {
			this.matcher = matcher;
			this.map_ent = mapent;
			this.coll_ent = conent;
			this.limit = limit;
		}
	
		public EObject select() {
			int count = 0;
			ESeq vals = ERT.NIL;
			
			ISeq map_seq = map_ent;
			ISeq coll_seq = coll_ent;
			while ((seq_has_more(map_seq) || seq_has_more(coll_seq)) && (limit < 0 || count < limit)) {
				
				if (!seq_has_more(coll_seq)) {
					IMapEntry ent = (IMapEntry) map_seq.first();
					IPersistentCollection coll = (IPersistentCollection) ent.getValue();
					coll_seq = coll.seq();
					map_seq = map_seq.next();
				}
				
				assert seq_has_more(coll_seq);
				
				ETuple candidate = (ETuple) coll_seq.first();
				coll_seq = coll_seq.next();
				
				EObject res;
				if ((res = matcher.match(candidate)) != null) {
					count += 1;
					vals = vals.cons(res);
				}
			}
			
			
			if (vals == ERT.NIL) {
				return Native.am_$end_of_table;
			} else if (!seq_has_more(map_seq) && !seq_has_more(coll_seq)) {
				return new ETuple2(vals, Native.am_$end_of_table);
			} else {
				return new ETuple2(vals, new EBagCont(matcher, map_seq, coll_seq, limit));
			}
		}

		private boolean seq_has_more(ISeq ent) {
			return ent != null && ent != ent.empty();
		}

		@Override
		public int hashCode() { // Shouldn't be called.
			return System.identityHashCode(this);
		}
	}
	
	
	@Override
	protected EInteger select_delete(final EMatchSpec matcher) {

		int delete_count = in_tx(new WithMap<Integer>() {

			@Override
			protected Integer run(IPersistentMap map) {
				ESeq vals = ERT.NIL;
				int initial_count = sizeRef.get();
				
				EObject key = matcher.getTupleKey(keypos1);
				
				if (key == null) {
					vals = matcher.matching_values_bag(vals, (Map<EObject, IPersistentCollection>) map);
				} else {
					IPersistentCollection coll = (IPersistentCollection) map.valAt(key);
					if (coll != null) {
						vals = matcher.matching_values_coll(vals, coll.seq());
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
				sizeRef.set(initial_count-count);
				return count;
			}});
		
		return ERT.box(delete_count);
	}

	protected void delete_all_objects() {
		in_tx(new WithMap<Object>() {

			@Override
			protected Object run(IPersistentMap map) {
				set(empty);
				sizeRef.set(0);
				return null;
			}
		});
	}

	@Override
	public ESeq slot() {
		IPersistentMap map = deref();
		if (map.count() == 0) return ERT.NIL;
		return new ELSeq(map.seq());
	}
	
	static class ELSeq extends ESeq {

		private final ISeq coll;
		private final ISeq map_seq;

		ELSeq(ISeq map) {
			IMapEntry collent = (IMapEntry)map.first();
			IPersistentCollection c = (IPersistentCollection) collent.getValue();
			
			while (c.count() == 0) {
				map = map.next();
				collent = (IMapEntry) map.first();
				c = (IPersistentCollection) collent.getValue();
			}
			
			coll = c.seq();
			map_seq = map.next();
		}
		
		ELSeq(ISeq coll, ISeq map_seq) {
			this.coll = coll;
			this.map_seq = map_seq;
		}
		
		@Override
		public ESeq cons(EObject h) {
			return new EList(h, this);
		}

		@Override
		public ESeq tail() {
			ISeq c2 = coll.next();
			if (c2 == null) {
				if (map_seq == null || map_seq == map_seq.empty()) return ERT.NIL;
				
				return new ELSeq(map_seq);
			} else {
				return new ELSeq(c2, map_seq);
			}
		}

		@Override
		public EObject head() {
			return (EObject) coll.first();
		}
		
		@Override
		public boolean isNil() {
			return false;
		}
		
		@Override
		public ECons testNonEmptyList() {
			return this;
		}
		
	}
	


}
