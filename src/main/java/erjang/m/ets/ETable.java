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
import java.util.concurrent.atomic.AtomicReference;

import kilim.Pausable;
import com.trifork.clj_ds.APersistentMap;
import com.trifork.clj_ds.IMapEntry;
import com.trifork.clj_ds.IPersistentMap;
import com.trifork.clj_ds.ISeq;
import com.trifork.clj_ds.PersistentTreeMap;
import com.trifork.clj_ds.RT;
import erjang.EAtom;
import erjang.EInteger;
import erjang.EInternalPID;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ETuple3;
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


	public static final EAtom am_stm = EAtom.intern("stm");
	
	protected WeakReference<EProc> owner;
	protected EAtom access;
	protected final int keypos1;
	protected EInternalPID heirPID;
	protected EObject heirData;
	protected final EInteger tid;
	protected final EAtom aname;
	protected final boolean is_named;
	protected final EAtom type;
	protected final APersistentMap empty;
	private AtomicReference<IPersistentMap<EObject, Object>> mapRef;
	private boolean is_fixed;

	ETable(EProc owner, EAtom type, EInteger tid, EAtom aname, EAtom access, int keypos,
			boolean is_named, EInternalPID heir_pid, EObject heir_data, APersistentMap map) {
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
			this.mapRef = new AtomicReference<IPersistentMap<EObject,Object>>(map);
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
			boolean is_named, EInternalPID heir_pid, EObject heir_data) {

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
		ESeq rep = ERT.NIL;
		ETable table = this;
		
		rep = rep.cons(new ETuple2(Native.am_owner, table.owner_pid()));
		rep = rep.cons(new ETuple2(Native.am_named_table, ERT.box(table.is_named)));
		rep = rep.cons(new ETuple2(Native.am_name, table.aname));
		if (table.heirPID != null)
			rep = rep.cons(new ETuple2(Native.am_heir, table.heirPID));
		else
			rep = rep.cons(new ETuple2(Native.am_heir, Native.am_none));
		rep = rep.cons(new ETuple2(Native.am_size, ERT.box(table.size())));
		rep = rep.cons(new ETuple2(Native.am_node, ERT.getLocalNode().node()));
		rep = rep.cons(new ETuple2(Native.am_type, table.type));
		rep = rep.cons(new ETuple2(Native.am_keypos, ERT.box(table.keypos1)));
		rep = rep.cons(new ETuple2(Native.am_protection, table.access));
		rep = rep.cons(new ETuple2(Native.am_fixed, ERT.box(is_fixed)));

		return rep;
	}

	
	public void on_exit(EInternalPID dyingPID) 
		throws Pausable 
	{
		if (dyingPID == owner_pid()) {
			
			EInternalPID heirPID = this.heirPID;
			EProc new_owner;
			if (heirPID != null 
					&& heirPID != dyingPID 
					&& (new_owner = heirPID.task()) != null) {
				//System.err.println("received exit from owner "+dyingPID
				//					+" => transfer to "+heirPID);

				ETuple msg = ETuple.make(EAtom.intern("ETS-TRANSFER"),
										 this.aname,
										 dyingPID,
										 this.heirData == null 
										 	? ERT.NIL 
										 	: this.heirData);
				
				this.owner = new WeakReference<EProc>(new_owner);
				new_owner.add_exit_hook(this);
				
				heirPID.send(dyingPID, msg);
				
			} else {				
				//System.err.println("received exit from owner "+dyingPID+" => delete");
				delete();
			}
			
		} else {
			System.err.println("table "+aname+" ("+tid+") received exit from unrelated "+dyingPID);
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
		if (keypos1 > value.arity()) {
			// TODO: return full list of args
			throw ERT.badarg(ERT.NIL.cons(value));
		}
		return value.elm(keypos1);
	}
	
	IPersistentMap deref() {
		return (IPersistentMap) mapRef.get();
	}
	

	abstract class WithMap<T> implements Callable<T> {
		
		private IPersistentMap orig;
		
		@Override
		public final T call() {
			return run(orig = (IPersistentMap)mapRef.get());
		}

		protected abstract T run(IPersistentMap map);

		protected void set(IPersistentMap map) {
			mapRef.compareAndSet(orig, map);
		}
		
	}
	
	@SuppressWarnings("unchecked")
	<X> X in_tx(WithMap<X> run) {
		try {
			synchronized (ETable.this) {
				return run.call();
			}
		} catch (ErlangError e) {
			throw e;
		} catch (Exception e) {
			e.printStackTrace();
			// STM Failure
			throw new ErlangError(am_stm);
		}
	}
	
	


	protected abstract void insert_one(ETuple value);

	protected abstract void insert_many(ESeq values);

	protected abstract boolean insert_new_one(ETuple value);

	protected abstract boolean insert_new_many(ESeq values);

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
				if (key.equalsExactly(from)) {
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

	public EObject info(EObject item) {
		
		if (item == Native.am_owner) {
			return owner_pid();
		} else if (item == Native.am_named_table) {
			return ERT.box(aname != null);
		} else if (item == Native.am_name) {
			return aname;
		} else if (item == Native.am_heir) {
			if (heirPID == null) return Native.am_none;
			else return heirPID;
		} else if (item == Native.am_size) {
			return ERT.box(size());
		} else if (item == Native.am_memory) {
			return ERT.box(10*size());
		} else if (item == Native.am_node) {
			return ERT.getLocalNode().node();
		} else if (item == Native.am_type) {
			return type;
		} else if (item == Native.am_keypos) {
			return ERT.box( keypos1 );
		} else if (item == Native.am_protection) {
			return access;
		} else if (item == Native.am_fixed) {
			return ERT.box(is_fixed);
		} else if (item == Native.am_stats) {
			//
			// The OTP test suite checks these, so we simply
			// provide some values that will make it happy.
			// This is very implementation dependent.
			//
			// {Buckets,AvgLen,StdDev,ExpSD,_MinLen,_MaxLen} 
			//
			return ETuple.make( ERT.box(256), // Buckets
								ERT.box(7),   // AvgLen,
								ERT.box(1),   // StdDev
								ERT.box(1),   // ExpSD
								ERT.box(1),   // MinLen
								ERT.box(10)   // MaxLen
							  );
		} else if (item == Native.am_safe_fixed) {
			throw new NotImplemented();
		} else {
			return null;
		}
	}

	public void setopt(EObject head) {
		
		ETuple3 tup = ETuple3.cast(head);
		if (tup != null) {

			EInternalPID pid;
			if (tup.elem1 == Native.am_heir
					&& (pid=tup.elem2.testInternalPID()) != null)
			{
				if (!pid.is_alive()) {
					this.heirPID = null;
					this.heirData = ERT.NIL;
					return;
				}

				EInternalPID old = this.heirPID;
				
				this.heirData = tup.elem3;
				this.heirPID = pid;

				pid.add_exit_hook(this);

				if (old != null) {
					old.remove_exit_hook(this);
				}
				
				return;
			}
			
		}
		
		ETuple2 tup2 = ETuple2.cast(head);
		if (tup2 != null) {
			
			if (tup2.elem1 == Native.am_protection) {
				EObject mode = tup2.elem2;
				if (mode == Native.am_private 
						|| mode == Native.am_public 
						|| mode == Native.am_protected) {
					this.access = (EAtom) mode;
					return;
				}
			} else if (tup2.elem1 == Native.am_heir 
					&& tup2.elem2 == Native.am_none) {

				EInternalPID old = this.heirPID;
				
				this.heirPID = null;
				this.heirData = ERT.NIL;

				if (old != null) {
					old.remove_exit_hook(this);
				}
				
			}
			
		}
		
		throw ERT.badarg(tid, head);
	}

	protected abstract EAtom member(EObject key);
	
}
