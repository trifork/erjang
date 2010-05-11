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

import java.lang.ref.WeakReference;
import java.util.concurrent.Callable;

import clojure.lang.APersistentMap;
import clojure.lang.IMapEntry;
import clojure.lang.IPersistentMap;
import clojure.lang.ISeq;
import clojure.lang.LockingTransaction;
import clojure.lang.PersistentTreeMap;
import clojure.lang.RT;
import clojure.lang.Ref;
import clojure.lang.Var;
import erjang.EAtom;
import erjang.EInteger;
import erjang.EInternalPID;
import erjang.ELazySeq;
import erjang.EObject;
import erjang.EPID;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ErlangError;
import erjang.ExitHook;
import erjang.NotImplemented;

/**
 * Abstract super class for an ETS table.  We implement ETS by using
 * Clojure's persistent data types.  
 * 
 * set:           PersistentHashMap [key, value]
 * ordered_set:   PersistentTreeMap [key, value]
 * bag			  PersistentHashMap [key, PersistentSet [value]]
 * duplicate_bag: PersistentHashMap [key, PersistentList [value]]
 * 
 */
abstract class ETable implements ExitHook {

	static {
		Object o = RT.IN;
	}
	

	public static final EAtom am_stm = EAtom.intern("stm");
	
	protected final WeakReference<EProc> owner;
	protected final EAtom access;
	protected final int keypos1;
	protected final EPID heirPID;
	protected final EObject heirData;
	protected final EInteger tid;
	protected final EAtom aname;
	protected final boolean is_named;
	protected final EAtom type;
	protected final APersistentMap empty;
	private Ref mapRef;

	ETable(EProc owner, EAtom type, EInteger tid, EAtom aname, EAtom access, int keypos,
			boolean is_named, EPID heir_pid, EObject heir_data, APersistentMap map) {
		this.type = type;
		this.is_named = is_named;
		this.owner = new WeakReference<EProc>(owner);
		this.tid = tid;
		this.aname = aname;
		this.access = access;
		this.keypos1 = keypos;
		this.heirPID = heir_pid;
		this.heirData = heir_data;
		try {
			this.mapRef = new Ref(map);
		} catch (Exception e) {
			throw new ErlangError(am_stm);
		}
		empty = map;
		owner.add_exit_hook(this);
	}

	/**
	 * Allocate table with specified configuration
	 */
	public static ETable allocate(EProc proc, EInteger tid, EAtom aname,
			EAtom type, EAtom access, int keypos, boolean write_concurrency,
			boolean is_named, EPID heir_pid, EObject heir_data) {

		if (type == Native.am_set || type == Native.am_ordered_set) {
			return new ETableSet(proc, type, tid, aname, access, keypos,
					write_concurrency, is_named, heir_pid, heir_data);
		}

		if (type == Native.am_bag || type == Native.am_duplicate_bag) {
			return new ETableBag(proc, type, tid, aname, access, keypos,
					write_concurrency, is_named, heir_pid, heir_data);
		}

		throw new NotImplemented("ets type=" + type + "; access=" + access
				+ "; keypos=" + keypos + "; write_concurrency="
				+ write_concurrency + "; heir_pid=" + heir_pid);
	}

	/**
	 * @param caller
	 * @param access
	 * @return
	 */
	public final boolean allow_access(EProc caller, boolean write_access) {
		if (access == Native.am_protected) {
			if (write_access) {
				return (caller == owner.get());
			} else {
				return true;
			}
		} else if (access == Native.am_public) {
			return true;
		} else if (access == Native.am_private) {
			return (caller == owner.get());
		} else {
			throw new InternalError("invalid access mode");
		}
	}

	EInternalPID owner_pid() {
		EProc o = owner.get();
		if (o == null)
			throw ERT.badarg();
		return o.self_handle();
	}

	ESeq info() {
		ESeq res = ERT.NIL;

		res = res.cons(new ETuple2(Native.am_owner, owner_pid()));
		res = res.cons(new ETuple2(Native.am_heir, heirPID));
		res = res.cons(new ETuple2(Native.am_name, aname));
		res = res.cons(new ETuple2(Native.am_size, ERT.box(size())));
		res = res.cons(new ETuple2(Native.am_node, ERT.getLocalNode().node()));
		res = res.cons(new ETuple2(Native.am_named_table, ERT.box(is_named)));
		res = res.cons(new ETuple2(Native.am_type, type));
		res = res.cons(new ETuple2(Native.am_keypos, ERT.box(keypos1)));
		res = res.cons(new ETuple2(Native.am_protection, access));

		return res;
	}

	
	@Override
	public void on_exit(EInternalPID pid) {
		if (pid == owner_pid()) {
			delete();
		}
	}

	void delete() {
		EInternalPID p = owner_pid();
		if (p != null) p.remove_exit_hook(this);
		
		Native.tid_to_table.remove(tid);
		if (is_named) {
			Native.name_to_tid.remove(aname);
		}
	}
	
	abstract int size();

	/** utility for subclasses */
	EObject get_key(ETuple value) {
		return value.elm(keypos1);
	}
	
	IPersistentMap deref() {
		return (IPersistentMap) mapRef.deref();
	}
	

	abstract class WithMap<T> implements Callable<T> {
		
		@Override
		public final T call() throws Exception {
			return run((IPersistentMap)mapRef.deref());
		}

		protected abstract T run(IPersistentMap map);

		protected void set(IPersistentMap map) {
			mapRef.set(map);
		}
		
	}
	
	@SuppressWarnings("unchecked")
	<X> X in_tx(Callable<X> run) {
		try {
			return (X) LockingTransaction.runInTransaction(run);
		} catch (Exception e) {
			e.printStackTrace();
			// STM Failure
			throw new ErlangError(am_stm);
		}
	}
	
	


	protected abstract void insert_one(ETuple value);

	protected abstract void insert_many(ESeq values);

	protected abstract void insert_new_one(ETuple value);

	protected abstract void insert_new_many(ESeq values);

	protected abstract ESeq lookup(EObject key);

	protected abstract ESeq match(EPattern matcher);

	protected abstract ESeq match_object(EPattern matcher);

	protected abstract void delete(EObject key);

	protected abstract EInteger select_delete(EMatchSpec matcher);

	protected void delete_all_objects() {
		in_tx(new WithMap<Object>() {

			@Override
			protected Object run(IPersistentMap map) {
				set(empty);
				return null;
			}
		});
	}

	protected abstract EObject first();

	protected EObject next(EObject from) {
		IPersistentMap map = deref();

		if (map instanceof PersistentTreeMap) {
			PersistentTreeMap ptm = (PersistentTreeMap) map;
			ISeq seq = ptm.seqFrom(from, true);
			if (seq == null) return Native.am_$end_of_table;
			seq = seq.next();
			if (seq == null) return Native.am_$end_of_table;
			IMapEntry ent = (IMapEntry) seq.first();
			if (ent == null) return Native.am_$end_of_table;
			return (EObject) ent.getKey();
			
		} else {
			
			for (ISeq seq = map.seq();seq != null;seq = seq.next()) {
				IMapEntry ent = (IMapEntry) seq.first(); 
				if (ent == null) return Native.am_$end_of_table;
				EObject key = (EObject) ent.getKey();
				if (key.equals(from)) {
					seq = seq.next();
					if (seq == null) return Native.am_$end_of_table;
					ent = (IMapEntry) seq.first(); 
					if (ent == null) return Native.am_$end_of_table;
					return (EObject) ent.getKey();
				}
			}
				 
			return Native.am_$end_of_table;
			
		}
	}

	protected abstract void delete_object(ETuple obj);

	public abstract ESeq slot();

	public abstract EObject select(EMatchSpec spec, int i);
	
}
