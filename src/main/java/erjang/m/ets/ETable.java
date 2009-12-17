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

import erjang.EAtom;
import erjang.EInteger;
import erjang.EObject;
import erjang.EPID;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.NotImplemented;

/**
 * Abstract super class for an ETS table
 */
abstract class ETable {

	protected final WeakReference<EProc> owner;
	protected final EAtom access;
	protected final int keypos;
	protected final EPID heirPID;
	protected final EObject heirData;
	private final EInteger tid;
	private final EAtom aname;
	private final boolean is_named;

	ETable(EProc owner, EInteger tid, EAtom aname, EAtom access, int keypos,
			boolean is_named, EPID heir_pid, EObject heir_data) {
		this.is_named = is_named;
		this.owner = new WeakReference<EProc>(owner);
		this.tid = tid;
		this.aname = aname;
		this.access = access;
		this.keypos = keypos;
		this.heirPID = heir_pid;
		this.heirData = heir_data;
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

	EPID owner_pid() {
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
		res = res.cons(new ETuple2(Native.am_type, type()));
		res = res.cons(new ETuple2(Native.am_keypos, ERT.box(keypos)));
		res = res.cons(new ETuple2(Native.am_protection, access));

		return res;
	}

	abstract int size();

	abstract EAtom type();

	/** utility for subclasses */
	EObject get_key(ETuple value) {
		return value.elm(keypos);
	}

	/** utility for subclasses */
	EObject get_key(EObject value) {
		ETuple tuple;
		if ((tuple = value.testTuple()) == null)
			throw ERT.badarg();
		return tuple.elm(keypos);
	}

	protected abstract void insert_one(ETuple value);

	protected abstract void insert_many(ESeq values);

	protected abstract void insert_new_one(ETuple value);

	protected abstract void insert_new_many(ESeq values);

	protected abstract EObject lookup(EObject key);

}
