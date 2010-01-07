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
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

import erjang.BIF;
import erjang.EAtom;
import erjang.EInteger;
import erjang.EList;
import erjang.ENative;
import erjang.EObject;
import erjang.EPID;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ETuple3;

/**
 * This class implements the BIFs in ets.
 */
public class Native extends ENative {

	// atoms used in this module are declared using the convention
	// of naming it am_XXX where XXX is the name of the atom (if sensible).

	/** The atom <code>'$end_of_table'</code> */
	public static final EAtom am_$end_of_table = EAtom.intern("$end_of_table");

	public static final EAtom am_set = EAtom.intern("set");
	public static final EAtom am_ordered_set = EAtom.intern("ordered_set");
	public static final EAtom am_bag = EAtom.intern("bag");
	public static final EAtom am_duplicate_bag = EAtom.intern("duplicate_bag");

	public static final EAtom am_public = EAtom.intern("public");
	public static final EAtom am_private = EAtom.intern("private");
	public static final EAtom am_protected = EAtom.intern("protected");

	public static final EAtom am_owner = EAtom.intern("owner");
	public static final EAtom am_heir = EAtom.intern("heir");
	public static final EAtom am_keypos = EAtom.intern("keypos");
	public static final EAtom am_name = EAtom.intern("name");
	public static final EAtom am_size = EAtom.intern("size");
	public static final EAtom am_node = EAtom.intern("node");
	public static final EAtom am_named_table = EAtom.intern("named_table");
	public static final EAtom am_write_concurrency = EAtom
			.intern("write_concurrency");
	public static final EAtom am_type = EAtom.intern("type");
	public static final EAtom am_none = EAtom.intern("none");
	public static final EAtom am_protection = EAtom.intern("protection");

	static AtomicLong next_tid = new AtomicLong(1);

	/** maps a table name to the corresponding TID */
	static Map<EAtom, EInteger> name_to_tid = new ConcurrentHashMap<EAtom, EInteger>();

	/** maps a TID to the corresponding table */
	static Map<EInteger, ETable> tid_to_table = new ConcurrentHashMap<EInteger, ETable>();

	/**
	 * Implements <a
	 * href="http://www.erlang.org/doc/man/ets.html#new-2">ets:new/2</a>.
	 * 
	 * @param name
	 *            an atom
	 * @param options
	 *            [Option]
	 */
	@BIF(name = "new")
	public static EObject new$(EProc self, EObject name, EObject options) {
		// because 'new' is not a valid identifier in Java, we have to provide
		// an erlang-scope name to the the BIF explicitly in the BIF annotation.
		// If the name attribute is omitted, the runtime will use the name of
		// the method as the BIF's name.
		//
		// BIFs must take EObject as arguments, not more specific
		// types. They should also be public static, so that calls to
		// it can be embedded directly in compiled code.

		// test argument types
		EAtom aname = name.testAtom();
		ESeq opts = options.testSeq();

		if (aname == null || opts == null) {
			// make sure to pass the original arguments to badarg, not
			// the converted ones. Java 'null' values are never allowed
			// in the erjang world except as a return value from type tests
			// and other guard BIFs.
			throw ERT.badarg(name, options);
		}

		// default configuration
		EAtom type = am_set;
		EAtom access = am_protected;
		int keypos = 1;
		boolean write_concurrency = false;
		EPID heir_pid = null;
		EObject heir_data = null;
		boolean is_named = false;

		for (; !opts.isNil(); opts = opts.tail()) {
			EObject option = opts.head();

			EAtom atom;
			ETuple2 t2;
			ETuple3 t3;
			if ((atom = option.testAtom()) != null) {
				if (atom == am_bag || atom == am_duplicate_bag
						|| atom == am_set || atom == am_ordered_set) {
					type = atom;
					continue;
				} else if (atom == am_public || atom == am_private
						|| atom == am_protected) {
					access = atom;
					continue;
				} else if (atom == am_named_table) {
					is_named = true;
					continue;
				}
			} else if ((t2 = ETuple2.cast(option)) != null) {
				ESmall pos;
				if (t2.elem1 == am_heir && t2.elem2 == am_none) {
					heir_data = null;
					heir_pid = null;
					continue;
				} else if (t2.elem1 == am_keypos
						&& ((pos = t2.elem2.testSmall()) != null)) {
					keypos = pos.value;
					continue;
				} else if (t2.elem1 == am_write_concurrency) {
					write_concurrency = (t2.elem2 == ERT.TRUE);
					continue;
				}
			} else if ((t3 = ETuple3.cast(option)) != null) {
				if (t3.elem1 == am_heir
						&& ((heir_pid = t3.elem2.testPID()) != null)) {
					heir_data = t3.elem3;
					continue;
				}
			}

			throw ERT.badarg(name, options);
		}

		EInteger tid = ERT.box(next_tid.incrementAndGet());

		ETable table = ETable.allocate(self, tid, aname, type, access, keypos,
				write_concurrency, is_named, heir_pid, heir_data);


		tid_to_table.put(tid, table);

		if (is_named) {
			name_to_tid.put(aname, tid);
			return aname;
		} else {
			return tid;
		}
	}

	/** used internally to resolve a tid|name */
	private static ETable resolve(EProc caller, EObject nameOrTid,
			boolean write_access) {

		EInteger tid = null;
		EAtom name;
		if ((name = nameOrTid.testAtom()) != null) {
			tid = name_to_tid.get(name);
		} else if ((tid = nameOrTid.testInteger()) != null) {
			// ok
		} else {
			return null;
		}

		if (tid == null) { return null; }
		
		ETable table = tid_to_table.get(tid);

		if (table != null && table.allow_access(caller, write_access)) {
			return table;
		} else {
			return null;
		}
	}

	/*
	 * [from ets.erl]: The following functions used to be found in this module,
	 * but are now BIFs (i.e. implemented in C). [THUS NEED TO BE IMPLEMENTED HERE]
	 * 
	 * all/0 new/2 delete/1 delete/2 first/1 info/1 info/2 safe_fixtable/2
	 * lookup/2 lookup_element/3 insert/2 is_compiled_ms/1 last/1 next/2 prev/2
	 * rename/2 slot/2 match/1 match/2 match/3 match_object/1 match_object/2
	 * match_object/3 match_spec_compile/1 match_spec_run_r/3 select/1 select/2
	 * select/3 select_reverse/1 select_reverse/2 select_reverse/3
	 * select_delete/2 update_counter/3
	 */

	@BIF
	public static EObject insert(EProc proc, EObject tab, EObject oneOrMore) {
		
		// test arguments
		ETable table = resolve(proc, tab, true);
		ETuple one = oneOrMore.testTuple();
		ESeq more = oneOrMore.testSeq();
		if (table == null || (one == null && more == null)) {
			throw ERT.badarg(tab, oneOrMore);
		}

		if (one != null) {
			table.insert_one(one);
		} else {
			table.insert_many(more);
		}
		
		return ERT.TRUE;
	}


	@BIF
	public static EObject lookup(EProc proc, EObject tab, EObject key) {
		
		// test arguments
		ETable table = resolve(proc, tab, true);
		if (table == null) {
			throw ERT.badarg(tab, key);
		}

		return table.lookup(key);
	}


	@BIF
	public static EObject insert_new(EProc proc, EObject tab, EObject oneOrMore) {
		
		// test arguments
		ETable table = resolve(proc, tab, true);
		ETuple one = oneOrMore.testTuple();
		ESeq more = oneOrMore.testSeq();
		if (table == null || (one == null && more == null)) {
			throw ERT.badarg(tab, oneOrMore);
		}

		if (one != null) {
			table.insert_new_one(one);
		} else {
			table.insert_new_many(more);
		}
		
		return ERT.TRUE;
	}

	@BIF
	public static EAtom delete(EProc caller, EObject nameOrTid)
	{
		ETable table = resolve(caller, nameOrTid, true);
		if (table == null) { throw ERT.badarg(nameOrTid); }
		
		table.delete();

		return ERT.TRUE;
	}
	
	@BIF static ESeq match(EProc caller, EObject nameOrTid, EObject spec)
	{
		ETuple ts = spec.testTuple();
		ETable table = resolve(caller, nameOrTid, false);
		if (ts == null || table == null) throw ERT.badarg(nameOrTid, spec);
		
		EPattern matcher = new EPattern(table.keypos1, ts);
		
		return table.match(matcher);
	}
	
	@BIF static EInteger select_delete(EProc caller, EObject nameOrTid, EObject spec)
	{
		ESeq lspec = spec.testSeq();
		ETable table = resolve(caller, nameOrTid, true);
		if (lspec == null || table == null) throw ERT.badarg(nameOrTid, spec);
		
		EMatchSpec matcher = EMatchSpec.compile(lspec);
		
		return table.select_delete(matcher);
	}
	
	@BIF static EAtom delete_all_objects(EProc caller, EObject nameOrTid) {
		ETable table = resolve(caller, nameOrTid, true);
		if (table == null) throw ERT.badarg(nameOrTid);
		table.delete_all_objects();
		return ERT.TRUE;
	}
}
