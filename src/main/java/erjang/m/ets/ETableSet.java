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
import java.util.Map.Entry;

import com.trifork.clj_ds.IMapEntry;
import com.trifork.clj_ds.IPersistentMap;
import com.trifork.clj_ds.ISeq;
import com.trifork.clj_ds.PersistentHashMap;
import com.trifork.clj_ds.PersistentTreeMap;
import erjang.EAtom;
import erjang.ECons;
import erjang.EInteger;
import erjang.EInternalPID;
import erjang.EList;
import erjang.ENumber;
import erjang.EObject;
import erjang.EPID;
import erjang.EProc;
import erjang.EPseudoTerm;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.NotImplemented;
import erjang.m.erlang.ErlBif;

/**
 * 
 */
public class ETableSet extends ETable {
	private boolean ordered;

	ETableSet(EProc owner, EAtom type, EInteger tid, EAtom aname, EAtom access, int keypos,
			boolean write_concurrency, boolean is_named, EInternalPID heirPID, EObject heirData) {
		super(owner, type, tid, aname, access, keypos,
				is_named, heirPID, heirData,
				type == Native.am_set
			  ? PersistentHashMap.EMPTY
			  : new PersistentTreeMap(null, EObject.ERLANG_ORDERING));
		this.ordered = type != Native.am_set;
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
				EObject key = get_key(value);
				IPersistentMap new_map = map.assoc(key, value);
				set(new_map);
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
					ETuple value = seq.head().testTuple();
					if (value == null) throw ERT.badarg(values);
					map = map.assoc(get_key(value), value);
				}
				set(map);
				return null;
			}});
	}
	
	@Override
	protected ESeq lookup(EObject key) {
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
	protected EAtom member(EObject key) {
		// no need to run in_tx if we're only reading
		EObject val = (EObject) deref().valAt(key);
		if (val != null) {
			return ERT.TRUE;
		} else {
			return ERT.FALSE;
		}
	}
	
	@Override
	public ESeq slot() {
		IPersistentMap map = deref();
		if (map.count() == 0) return ERT.NIL;
		ISeq seq = map.seq();
		return new ELSeq(seq);
	}
	
	static class ELSeq extends ESeq {

		private ISeq seq;

		ELSeq(ISeq s) {
			this.seq = s;
		}
		
		@Override
		public ESeq cons(EObject h) {
			return new EList(h, this);
		}

		@Override
		public ESeq tail() {
			ISeq next = seq.next();
			if (next == null) return ERT.NIL;
			return new ELSeq(next);
		}

		@Override
		public EObject head() {
			IMapEntry ent = (IMapEntry) seq.first();
			return (EObject) ent.getValue();
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
	
	@Override
	protected EObject first() {
		// no need to run in_tx if we're only reading
		IPersistentMap map = deref();
		
		if (map.count() == 0) {
			return Native.am_$end_of_table;
		} else {
			ISeq entseq = map.seq();
			if (entseq == null) return Native.am_$end_of_table;
			IMapEntry ent = (IMapEntry) entseq.first();
			if (ent == null) return Native.am_$end_of_table;
			return (EObject) ent.getKey();
		}
	}


	@Override
	protected boolean insert_new_many(final ESeq values) {
		// Input verification outside of transaction:
		for (ESeq seq = values; !seq.isNil(); seq = seq.tail()) {
			ETuple value = seq.head().testTuple();
			if (value == null) throw ERT.badarg(values);
			EObject key = get_key(value);
		}

		return in_tx(new WithMap<Boolean>() {
			@Override
			protected Boolean run(IPersistentMap map) {
				for (ESeq seq = values; !seq.isNil(); seq = seq.tail()) {
					ETuple value = seq.head().testTuple();
					EObject key = get_key(value);
					if (map.containsKey(key)) {
						return false;
					}
				}
	
				for (ESeq seq = values; !seq.isNil(); seq = seq.tail()) {
					ETuple value = seq.head().testTuple();
					EObject key = get_key(value);
					map = map.assoc(key, value);
				}
	
				set(map);
				return true;
			}
	});
}

	@Override
	protected boolean insert_new_one(final ETuple value) {
		final EObject key = get_key(value);
		if (!deref().containsKey(key)) {
			return in_tx(new WithMap<Boolean>() {
				@Override
				protected Boolean run(IPersistentMap map) {
					if (!map.containsKey(key)) {
						set(map.assoc(key, value));
						return true;
					}
					return false;
				}
			});
		} else {
			return false;
		}
	}
	
	@Override
	public ESeq match(EPattern matcher) {		
		IPersistentMap map = deref();
		ESeq res = ERT.NIL;
		
		EObject key = matcher.getKey(keypos1);
		if (key == null) {
			res = matcher.match(res, (Map<EObject, ETuple>) map);
			if (ordered) res = res.reverse();
		} else {
			ETuple candidate = (ETuple) map.valAt(key);
			if (candidate != null) {
				res =  matcher.match(res, candidate);
			}
		}
		
		return res;
	}
	
	@Override
	public ESeq match_object(EPattern matcher) {		
		IPersistentMap map = deref();
		ESeq res = ERT.NIL;
		
		EObject key = matcher.getKey(keypos1);
		if (key == null) {
			res = matcher.match_members(res, (Map<EObject, ETuple>) map);
			if (ordered) res = res.reverse();
		} else {
			ETuple candidate = (ETuple) map.valAt(key);
			if (candidate != null) {
				res =  matcher.match_members(res, candidate);
			}
		}
		
		return res;
	}


	@Override
	protected void delete(final EObject key) {
		in_tx(new WithMap<Object>() {
			@Override
				protected Object run(IPersistentMap map) {
			    try {
					map = map.without(key);
			    } catch (Exception e) {
					// should not happen!
					throw new Error(e);
			    }
			    set(map);
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
				
				IMapEntry candidateEntry = map.entryAt(key);
				if (candidateEntry == null) return null;
				EObject candidate = (EObject)candidateEntry.val();
				if (candidate != null && obj.equalsExactly(candidate)) {
					try {
						map = map.without(key);
						set(map);
					} catch (Exception e) {
						throw new Error(e);
					}
				}
				
				return null;
			}
		});
	}
	
	@Override
	public EObject select(final EMatchSpec matcher, int limit) {
		
		IPersistentMap map = deref();
		
		EObject key = matcher.getTupleKey(keypos1);
		
		if (key == null) {
			ESetCont cont0 = new ESetCont(matcher, map.seq(), ordered, limit);
			return cont0.select();
			
		} else {
			ETuple candidate = (ETuple) map.valAt(key);
			if (candidate == null) return Native.am_$end_of_table;
			EObject res;
			if ((res = matcher.match(candidate)) != null) {
				return new ETuple2(ERT.NIL.cons(res), Native.am_$end_of_table);
			}
		}
		
		return Native.am_$end_of_table;
	}
	
	static class ESetCont extends EPseudoTerm implements ISelectContinuation {

		private final ISeq ent;
		private final EMatchSpec matcher;
		private final boolean ordered;
		private final int limit;

		public ESetCont(EMatchSpec matcher, ISeq ent, boolean ordered, int limit) {
			this.matcher = matcher;
			this.ent = ent;
			this.ordered = ordered;
			this.limit = limit;
		}
	
		public EObject select() {
			int count = 0;
			ESeq vals = ERT.NIL;
			
			ISeq map_seq = this.ent;
			while (seq_has_more(map_seq) && (limit < 0 || count < limit)) {
				
				IMapEntry mape = (IMapEntry) map_seq.first();
				map_seq = map_seq.next();
				
				ETuple candidate = (ETuple) mape.getValue();
				EObject res;
				if ((res = matcher.match(candidate)) != null) {
					count += 1;
					vals = vals.cons(res);
				}
			}
			if (ordered) vals = vals.reverse();

			if (vals == ERT.NIL) {
				return Native.am_$end_of_table;
			} else if (!seq_has_more(map_seq)) {
				return new ETuple2(vals, Native.am_$end_of_table);
			} else {
				return new ETuple2(vals, new ESetCont(matcher, map_seq, ordered, limit));
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
	public EInteger select_delete(final EMatchSpec matcher) {		
		int delete_count = in_tx(new WithMap<Integer>() {

			@Override
			protected Integer run(IPersistentMap map) {
				EObject key = matcher.getTupleKey(keypos1);
				int count = 0;
				
				if (key == null) {
					for (Map.Entry<EObject, ETuple> ent : ((Map<EObject, ETuple>) map).entrySet()) {		
						ETuple val = ent.getValue();
						if (matcher.matches(val)) {
							try {
								map = map.without(ent.getKey());
							} catch (Exception e) {
								throw new RuntimeException(e);
							}
							count += 1;
						}
					}
				} else {
					ETuple candidate = (ETuple) map.valAt(key);
					if (candidate != null && matcher.matches(candidate)) {
						try {
							map = map.without(key);
						} catch (Exception e) {
							throw new RuntimeException(e);
						}
						count += 1;
					}
				}
				
				set(map);
				return count;
			}});
		
		return ERT.box(delete_count);
	}

	public EObject update_counter(final EObject key, final EObject upd) {
		return in_tx(new WithMap<EObject>() {

			@Override
			protected EObject run(IPersistentMap map) {
				ETuple rec = (ETuple) map.valAt(key);
				if (rec == null)
					return null; // fail with badarg
				
				// TODO: figure out match/equals semantics
				if (type == Native.am_set) {
					if (!key.equalsExactly( get_key(rec) )) {
						return null;
					}
				}

				EInteger incr;
				ETuple one;
				if ((incr=upd.testInteger()) != null) {
					int idx = keypos1+1;
					
					rec = update(rec, idx, incr);
					if (rec == null) return null;
					map = map.assoc(get_key(rec), rec);
					
					set(map);
					return rec.elm(idx);
					
				} else if ((one=upd.testTuple()) != null) {
					
					if (one.arity() == 2) {
						ESmall eidx = one.elm(1).testSmall();
						incr = one.elm(2).testInteger();
						if (eidx == null || eidx.value > rec.arity() || incr == null) return null;
						int idx = eidx.value;
						
						rec = update(rec, idx, incr);
						if (rec == null) return null;
						map = map.assoc(get_key(rec), rec);
						
						set(map);
						return rec.elm(idx);

					} else if (one.arity() == 4){
						
						ESmall eidx = one.elm(1).testSmall();
						incr = one.elm(2).testInteger();
						EInteger threshold = one.elm(3).testInteger();
						EInteger setvalue = one.elm(4).testInteger();
						if (eidx == null || eidx.value > rec.arity() || incr == null
								|| threshold == null || setvalue == null) return null;
						int idx = eidx.value;
						
						rec = update(rec, idx, incr, threshold, setvalue);
						if (rec == null) return null;
						map = map.assoc(get_key(rec), rec);
						
						set(map);
						return rec.elm(idx);

					} else {
						return null;
					}
					
				} else {
					throw new NotImplemented();
				}
				
			}

			private ETuple update(ETuple rec, int idx, EInteger incr) {

				EInteger old = rec.elm(idx).testInteger();
				if (old == null) return null;
				EObject val = old.add(incr);
				rec = ErlBif.setelement(idx, rec, val);

				return rec;
			}
			
			private ETuple update(ETuple rec, int idx, EInteger incr, EInteger threshold, EInteger setvalue) {

				EInteger old = rec.elm(idx).testInteger();
				if (old == null) return null;
				ENumber val = old.add(incr);
				
				if (incr.is_ge(ESmall.ZERO)) {

					if (threshold.is_lt(val)) {
						val = setvalue;
					}
					
				} else {
					
					if (val.is_lt(threshold)) {
						val = setvalue;
					}
				}

				
				rec = ErlBif.setelement(idx, rec, val);

				return rec;
			}
			
		});
	}
	

	public EObject update_element(final EObject key, final ESeq upd) {
		return in_tx(new WithMap<EObject>() {

			@Override
			protected EObject run(IPersistentMap map) {
				ETuple rec = (ETuple) map.valAt(key);
				if (rec == null)
					return ERT.FALSE; 
				
				// TODO: figure out match/equals semantics
				if (type == Native.am_set) {
					if (!key.equalsExactly( get_key(rec) )) {
						return ERT.FALSE;
					}
				}

				ETuple rep = null;
				
				for (ESeq next = upd ; !next.isNil() ; next = next.tail()) {
					ETuple2 update = ETuple2.cast(next.head());
					if (update == null) return null;
					ESmall idx1 = update.elem1.testSmall();
					if (idx1 == null 
							|| idx1.value < 1 
							|| idx1.value > rec.arity()
							|| idx1.value == keypos1) return null;
					
					if (rep == null) {
						rep = rec.setelement(idx1.value, update.elem2);
					} else {
						rep.set(idx1.value, update.elem2);
					}
				}
				
				if (rep != null) {
					map = map.assoc(get_key(rec), rep);
					set(map);
				}
				
				return ERT.TRUE;				
			}
		});
	}
}
