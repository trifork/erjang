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

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

import erjang.BIF;
import erjang.EAtom;
import erjang.EInteger;
import erjang.EInternalPID;
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
import erjang.ErlangError;
import erjang.ErlangException;
import erjang.NotImplemented;

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
	public static final EAtom am_memory = EAtom.intern("memory");
	public static final EAtom am_node = EAtom.intern("node");
	public static final EAtom am_named_table = EAtom.intern("named_table");
	public static final EAtom am_write_concurrency = EAtom
			.intern("write_concurrency");
	public static final EAtom am_type = EAtom.intern("type");
	public static final EAtom am_none = EAtom.intern("none");
	public static final EAtom am_protection = EAtom.intern("protection");
	public static final EAtom am_fixed = EAtom.intern("fixed");
	public static final EAtom am_safe_fixed = EAtom.intern("safe_fixed");
	public static final EAtom am_stats = EAtom.intern("stats");

	static AtomicLong next_tid = new AtomicLong(1);

	/** maps a table name to the corresponding TID */
	static ConcurrentHashMap<EAtom, EInteger> name_to_tid = new ConcurrentHashMap<EAtom, EInteger>();

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
		EInternalPID heir_pid = null;
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
						   && ((pos = t2.elem2.testSmall()) != null)
						   && pos.value >= 1) {
					keypos = pos.value;
					continue;
				} else if (t2.elem1 == am_write_concurrency) {
					write_concurrency = (t2.elem2 == ERT.TRUE);
					continue;
				}
			} else if ((t3 = ETuple3.cast(option)) != null) {
				if (t3.elem1 == am_heir
						&& ((heir_pid = t3.elem2.testInternalPID()) != null)) {
					
					if (!heir_pid.is_alive()) {
						heir_pid = null;
					} else {
						heir_data = t3.elem3;
					}
					
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
			Object existing = name_to_tid.putIfAbsent(aname, tid);
			if (existing != null) throw ERT.badarg(name, options);
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
	public static EObject delete(EProc proc, EObject tab, EObject key) {
		// test arguments
		ETable table = resolve(proc, tab, true);
		if (table == null) {
			throw ERT.badarg(tab, key);
		}

		table.delete(key);

		return ERT.TRUE;
	}

	@BIF
	public static EObject lookup_element(EProc proc, EObject tab, EObject key, EObject pos) {
		
		ESmall p = pos.testSmall();
		
		// test arguments
		ETable table = resolve(proc, tab, false);
		if (table == null || p == null) {
			throw ERT.badarg(tab, key, pos);
		}

		ESeq ent = table.lookup(key);
		if (ent == ERT.NIL) {
			throw ERT.badarg(tab, key, pos);
		}
		
		if (table.type == am_set || table.type == am_ordered_set) {
			return get_elm(tab, key, p, ent);
		} else {
			ESeq res = ERT.NIL;
			
			for (; !ent.isNil(); ent = ent.tail()) {
				res = res.cons( get_elm(tab, key, p, ent) );
			}
			return res;
		}
	}

	private static EObject get_elm(EObject tab, EObject key, ESmall p,
			ESeq ent) {
		
		ETuple tup = ent.head().testTuple();
		if (tup == null || p.value < 1 || p.value > tup.arity()) {
			throw ERT.badarg(tab, key, p);
		}
		
		return tup.elm(p.value);
	}

	
	
	@BIF
	public static ESeq lookup(EProc proc, EObject tab, EObject key) {
		
		// test arguments
		ETable table = resolve(proc, tab, false);
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

		boolean result;
		if (one != null) {
			result = table.insert_new_one(one);
		} else {
			result = table.insert_new_many(more);
		}
		
		return ERT.box(result);
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
		try {
		ETable table = resolve(caller, nameOrTid, false);
		if (table == null) throw ERT.badarg(nameOrTid, spec);
		
		EPattern matcher = new EPattern(table.keypos1, spec);
		
		return table.match(matcher);
		} catch (ErlangException e) {
			e.printStackTrace();
			throw e;
		}
	}
	
	@BIF static EInteger select_delete(EProc caller, EObject nameOrTid, EObject spec)
	{
		ESeq lspec = spec.testSeq();
		ETable table = resolve(caller, nameOrTid, true);
		if (lspec == null || table == null) throw ERT.badarg(nameOrTid, spec);
		
		EMatchSpec matcher = EMatchSpec.compile(lspec);
		
		return table.select_delete(matcher);
	}
	
	@BIF static public EAtom delete_all_objects(EProc caller, EObject nameOrTid) {
		ETable table = resolve(caller, nameOrTid, true);
		if (table == null) throw ERT.badarg(nameOrTid);
		table.delete_all_objects();
		return ERT.TRUE;
	}
	
	@BIF static public EObject match_spec_compile(EObject spec) {
		ESeq lspec = spec.testSeq();
		if (lspec == null) throw ERT.badarg(spec);
		EMatchSpec matcher = EMatchSpec.compile(lspec);
		return matcher;
	}
	
	@BIF static public EObject is_compiled_ms(EObject spec) {
		return ERT.box(spec instanceof EMatchSpec);
	}
	
	@BIF static public EObject prev(EObject obj, EObject obj2) {
		throw new NotImplemented(); 
	}

	@BIF public static EObject first(EProc proc, EObject tab) {
		ETable table = resolve(proc, tab, false);
		if (table == null) {
			throw ERT.badarg(tab);
		}

		return table.first();
	}
	

	@BIF static public EObject slot(EProc caller, EObject nameOrTid, EObject i) {
		ETable table = resolve(caller, nameOrTid, false);
		ESmall pos;
		if ((pos=i.testSmall())==null || table == null) 
			throw ERT.badarg(nameOrTid, i);

		if (pos.value == 0) {
			
			return table.slot();
			
		} else if (pos.value == 1) {
			return am_$end_of_table;
		} else {
			throw ERT.badarg(nameOrTid, i);
		}
	}
	
	@BIF static public ESeq all() {
		
		ESeq res = ERT.NIL;
		
		for (ETable table : tid_to_table.values()) {
			if (table.is_named) {
				res = res.cons(table.aname);
			} else {
				res = res.cons(table.tid);
			}
		}
		
		return res;
	}

	@BIF static public EObject last(EObject obj) {
		throw new NotImplemented(); 
	}

	@BIF static public EObject match_object(EProc caller, EObject nameOrTid, EObject spec) {
		ETable table = resolve(caller, nameOrTid, false);
		if ((spec.testAtom()==null && spec.testTuple()==null) || table == null) 
			throw ERT.badarg(nameOrTid, spec);
		
		EPattern matcher;
		try {
			matcher = new EPattern(table.keypos1, spec);
		} catch (ErlangException e) {
			System.err.println("bad match spec: "+spec);
			e.printStackTrace();
			throw ERT.badarg(nameOrTid, spec);
		}
		
		return table.match_object(matcher);
	}

	@BIF static public EObject next(EProc proc, EObject tab, EObject key) {
		ETable table = resolve(proc, tab, false);
		if (table == null) {
			throw ERT.badarg(tab,key);
		}

		return table.next(key);
	}

	@BIF static public EObject safe_fixtable(EProc proc, EObject tab, EObject onOff) {
		ETable table = resolve(proc, tab, false);
		if (table == null || (onOff != ERT.TRUE && onOff != ERT.FALSE)) {
			throw ERT.badarg(tab,onOff);
		}

		// TODO: fix somehow?
		
		return ERT.TRUE;
	}
	
	@BIF static public EObject repair_continuation(EObject cont, EObject ms) {
		System.err.println("repair cont="+cont+"; ms="+ms);
		return cont;
	}

	@BIF static public EObject select(EObject obj1) {
		
		if (obj1 == Native.am_$end_of_table) {
			return obj1;
		}
		
		if (obj1 instanceof ISelectContinuation) {
			ISelectContinuation cont = (ISelectContinuation) obj1;			
			return cont.select();
			
		} else {
			throw ERT.badarg(obj1);
		}
		
	}

	@BIF static public ESmall select_count(EProc caller, EObject nameOrTid, EObject matchSpec) {
		ESeq res = select(caller, nameOrTid, matchSpec);
		int result = 0;
		while (!res.isNil()) {
			if (res.head() == ERT.TRUE)
				result += 1;
			res = res.tail();
		}
		return ERT.box(result);
	}

	@BIF static public ESeq select(EProc caller, EObject nameOrTid, EObject matchSpec) {
		
		ETable table = resolve(caller, nameOrTid, false);
		if (table == null) throw ERT.badarg(nameOrTid, matchSpec);
		
		EMatchSpec spec;
		ESeq seq;
		if ((matchSpec instanceof EMatchSpec)) {
			spec = (EMatchSpec) matchSpec;
		} else if ((seq=matchSpec.testSeq()) != null) {
			try {
				spec = EMatchSpec.compile(seq);
			} catch (ErlangError e) {
				if (e.reason() == ERT.am_badarg) {
					throw ERT.badarg(nameOrTid, matchSpec);
				} else {
					throw e;
				}
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
				throw ERT.badarg(nameOrTid, matchSpec);
			}
		} else {
			throw ERT.badarg(nameOrTid, matchSpec);
		}
		
		EObject res = table.select(spec, -1);

		ETuple2 tup;
		if (res == am_$end_of_table) {
			return ERT.NIL;
		} else if ((tup=ETuple2.cast(res)) != null 
				&& (seq = tup.elem1.testSeq()) != null) {
			return seq;
		} else {
			throw new InternalError();
		}
		
	}

	@BIF static public EObject select(EProc caller, EObject nameOrTid, EObject matchSpec, EObject limit) {

		ETable table = resolve(caller, nameOrTid, false);
		ESmall lim = limit.testSmall();
		if (table == null || lim == null || lim.value < 1) throw ERT.badarg(nameOrTid, matchSpec, limit);
		
		EMatchSpec spec;
		ESeq seq;
		if ((matchSpec instanceof EMatchSpec)) {
			spec = (EMatchSpec) matchSpec;
		} else if ((seq=matchSpec.testSeq()) != null) {
			try {
				spec = EMatchSpec.compile(seq);
			} catch (ErlangError e) {
				if (e.reason() == ERT.am_badarg) {
					throw ERT.badarg(nameOrTid, matchSpec, limit);
				} else {
					throw e;
				}
			}
		} else {
			throw ERT.badarg(nameOrTid, matchSpec, limit);
		}
		
		return table.select(spec, lim.value);
		
	}

	@BIF static public EObject match_spec_run_r(EObject list, EObject matchSpec, EObject tail_arg) {
		
		ESeq res = tail_arg.testSeq();
		ESeq input = list.testSeq();
		
		if (res == null || input == null)
			throw ERT.badarg(list, matchSpec, tail_arg);

		EMatchSpec spec;
		ESeq seq;
		if ((matchSpec instanceof EMatchSpec)) {
			spec = (EMatchSpec) matchSpec;
		} else if ((seq=matchSpec.testSeq()) != null) {
			try {
				spec = EMatchSpec.compile(seq);
			} catch (ErlangError e) {
				if (e.reason() == ERT.am_badarg) {
					throw ERT.badarg(list, matchSpec, tail_arg);
				} else {
					throw e;
				}
			}
		} else {
			throw ERT.badarg(list, matchSpec, tail_arg);
		}

		while (!input.isNil()) {
			
			EObject candidate = input.head();
			EObject o = spec.match(candidate);
			if (o != null) {
				res = res.cons(o);
			}
			
			input = input.tail();
		}
		
		return res;
	}
	
	@BIF static public EObject setopts(EProc proc, EObject tab, EObject opts) {
		// test arguments
		ETable table = resolve(proc, tab, true);
		if (table == null || table.owner_pid() != proc.self_handle()) {
			throw ERT.badarg(tab, opts);
		}
		
		ESeq seq;
		if ((seq=opts.testSeq()) != null) {
			while (!seq.isNil()) {
				table.setopt(seq.head());
				seq = seq.tail();
			}
		} else {
			table.setopt(opts);
		}
	
		return ERT.TRUE;
	}

	@BIF static public EObject info(EProc proc, EObject nameOrTid) {
		
		ETable table;
		if ((table = get_table(nameOrTid)) == null) {
			return ERT.am_undefined;
		}
		
		return table.info();
	}

	@BIF static public EObject info(EProc proc, EObject nameOrTid, EObject item) {

		ETable table;
		if ((table = get_table(nameOrTid)) == null) {
			return ERT.am_undefined;
		}

		EObject info = table.info(item);

		if (info == null)
			throw ERT.badarg(nameOrTid, item);
		else
			return info;
	}

	private static ETable get_table(EObject nameOrTid) {
		ETable table = null;
		EInteger tid;
		EAtom name;
		if ((tid=nameOrTid.testInteger()) != null) {
			table = tid_to_table.get(tid);
		} else if ((name=nameOrTid.testAtom()) != null) {
			tid = name_to_tid.get(name);
			if (tid != null) {
				table = tid_to_table.get(tid);
			}
		} else {
			throw ERT.badarg(nameOrTid);
		}
		return table;
	}

	/** this is not documented anywhere, but referenced from the module global */
	@BIF static public EObject member(EProc proc, EObject tab, EObject key) {
		
		// test arguments
		ETable table = resolve(proc, tab, false);
		if (table == null) {
			throw ERT.badarg(tab, key);
		}

		return table.member(key);
	}

	/** this is not documented anywhere, but referenced from the module global */
	@BIF static public EObject delete_object(EProc proc, EObject tab, EObject obj) {
		
		
		// test arguments
		ETable table = resolve(proc, tab, true);
		ETuple one = obj.testTuple();
		if (table == null || one == null ) {
			throw ERT.badarg(tab, obj);
		}

		table.delete_object(one);
		return ERT.TRUE;
	}

	/** this is not documented anywhere, but referenced from the module global */
	@BIF static public EObject update_counter(EProc proc, EObject tab, EObject key, EObject upd) {
		ETable table = resolve(proc, tab, true);
		
		if (table == null || !(table.type==am_set || table.type==am_ordered_set)) {
			throw ERT.badarg(tab,key,upd);
		}
		
		ETableSet ets = (ETableSet) table;
		
		EObject res = ets.update_counter(key, upd);
		
		if (res == null) {
			throw ERT.badarg(tab,key,upd);			
		}
		
		return res;
	}

	@BIF static public EObject update_element(EProc proc, EObject tab, EObject key, EObject upd) {
		ETable table = resolve(proc, tab, true);
		
		ETuple2 t_upd = ETuple2.cast(upd);
		ESeq   s_upd = upd.testSeq();
		
		if (table == null 
				|| (t_upd == null && s_upd == null)  
				|| !(table.type==am_set || table.type==am_ordered_set)) {
			throw ERT.badarg(tab,key,upd);
		}
		
		// always turn it into a list of updates
		if (s_upd == null) {
			s_upd = ERT.NIL.cons(t_upd);
		}
		
		ETableSet ets = (ETableSet) table;
		
		EObject res = ets.update_element(key, s_upd);
		
		if (res == null) {
			throw ERT.badarg(tab,key,upd);			
		}
		
		return res;
	}


}
