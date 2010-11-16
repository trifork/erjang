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
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Pattern;

import com.trifork.clj_ds.ISeq;

import erjang.EAtom;
import erjang.EBitString;
import erjang.ECons;
import erjang.EFun;
import erjang.EList;
import erjang.ENumber;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.ERT;
import erjang.ERef;
import erjang.ESeq;
import erjang.ETuple;

/**
 * 
 */
public class EPattern {

	ETermPattern matcher;
	private final EObject spec;
	private Integer[] out_vars;
	private final int keyidx;

	@Override
	public String toString() {
		return "#Pattern<keyidx=" + keyidx + ";" + spec.toString() + ">";
	}

	/**
	 * @param spec
	 */
	public EPattern(int keyidx, EObject spec) {
		this.keyidx = keyidx;
		this.spec = spec;
		SortedSet<Integer> out = new TreeSet<Integer>();

		matcher = spec.compileMatch(out);

		this.out_vars = out.toArray(new Integer[out.size()]);
	}

	ESeq match_members(ESeq out, Collection<ETuple> in) {

		for (EObject elm : in) {
			EMatchContext res = new EMatchContext(out_vars, elm);
			if (elm.match(matcher, res)) {
				out = out.cons(elm);
			}
		}

		return out;
	}

	ESeq match(ESeq out, Map<EObject, ETuple> in) {

		for (ETuple elm : in.values()) {
			ETuple val = elm.testTuple();
			EMatchContext res = new EMatchContext(out_vars, val);
			if (matcher.match(val, res)) {
				out = out.cons(res.makeList());
			}
		}

		return out;
	}

	/** return list of matching members */
	ESeq match_members(ESeq out, Map<EObject, ETuple> in) {

		for (ETuple elm : in.values()) {
			EMatchContext res = new EMatchContext(out_vars, elm);
			if (matcher.match(elm, res)) {
				out = out.cons(elm);
			}
		}

		return out;
	}

	/** return list of matching members */
	ESeq match_members(ESeq out, ISeq in) {

		while (in != null) {
			ETuple elm = (ETuple) in.first();
			if (elm == null)
				break;

			EMatchContext res = new EMatchContext(out_vars, elm);
			if (matcher.match(elm, res)) {
				out = out.cons(elm);
			}

			in = in.next();
		}

		return out;
	}

	/** return list of matching members' vars */
	ESeq match_vars(ESeq out, ISeq in) {

		while (in != null) {
			ETuple elm = (ETuple) in.first();
			if (elm == null)
				break;

			EMatchContext res = new EMatchContext(out_vars, elm);
			if (matcher.match(elm, res)) {
				out = out.cons(res.makeList());
			}

			in = in.next();
		}

		return out;
	}

	public static EPattern compile(int keyidx, ETuple spec) {
		return new EPattern(keyidx, spec);
	}

	/** matcher for '_' */
	static class AnyPattern extends ETermPattern {
		public boolean match(ETuple t, EMatchContext r) {
			return true;
		}

		public boolean match(ENumber n, EMatchContext r) {
			return true;
		}

		public boolean match(EAtom a, EMatchContext r) {
			return true;
		}

		public boolean match(ECons c, EMatchContext r) {
			return true;
		}

		public boolean match(EPID p, EMatchContext r) {
			return true;
		}

		public boolean match(EPort p, EMatchContext r) {
			return true;
		}

		public boolean match(EBitString bs, EMatchContext r) {
			return true;
		}

	}

	/** matcher for '$N' */
	static class VarPattern extends ETermPattern {
		private Integer name;
		private boolean free;

		/**
		 * @param idx2
		 * @param out
		 */
		public VarPattern(int idx1, Set<Integer> out) {
			this.name = idx1;
			if (this.free = !out.contains(this.name)) {
				out.add(this.name);
			}
		}

		public boolean match(ETuple t, EMatchContext r) {
			if (free) {
				r.vars.put(name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(name));
			}
		}

		public boolean match(ENumber t, EMatchContext r) {
			if (free) {
				r.vars.put(name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(name));
			}
		}

		public boolean match(EAtom t, EMatchContext r) {
			if (free) {
				r.vars.put(name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(name));
			}
		}

		public boolean match(EFun fu, EMatchContext r) {
			if (free) {
				r.vars.put(name, fu);
				return true;
			} else {
				return fu.equalsExactly(r.vars.get(name));
			}
		}

		public boolean match(ERef fu, EMatchContext r) {
			if (free) {
				r.vars.put(name, fu);
				return true;
			} else {
				return fu.equalsExactly(r.vars.get(name));
			}
		}

		public boolean match(ECons t, EMatchContext r) {
			if (free) {
				r.vars.put(name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(name));
			}
		}

		public boolean match(EPID t, EMatchContext r) {
			if (free) {
				r.vars.put(name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(name));
			}
		}

		public boolean match(EPort t, EMatchContext r) {
			if (free) {
				r.vars.put(name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(name));
			}
		}

		public boolean match(EBitString t, EMatchContext r) {
			if (free) {
				r.vars.put(name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(name));
			}
		}
	}

	static class TuplePattern extends ETermPattern {
		int arity;
		ETermPattern[] elems;

		/**
		 * @param tup
		 * @param out
		 */
		public TuplePattern(ETuple tup, Set<Integer> out) {
			arity = tup.arity();
			elems = new ETermPattern[arity];
			for (int idx1 = 1; idx1 <= arity; idx1++) {
				elems[idx1 - 1] = tup.elm(idx1).compileMatch(out);
			}
		}

		public boolean match(EObject elm, EMatchContext res) {
			ETuple tup;
			if ((tup = elm.testTuple()) == null)
				return false;
			return match(tup, res);
		}

		public boolean match(ETuple t, EMatchContext r) {
			if (t.arity() != arity)
				return false;
			for (int idx1 = 1; idx1 < elems.length + 1; idx1++) {
				if (!t.elm(idx1).match(elems[idx1 - 1], r))
					return false;
			}
			return true;
		}
	}

	static class NilPattern extends ETermPattern {
		@Override
		public boolean match(ECons c, EMatchContext r) {
			return c.isNil();
		}
	}

	static class ConsPattern extends ETermPattern {

		ETermPattern head, tail;

		/**
		 * @param cons
		 * @param out
		 */
		public ConsPattern(ECons cons, Set<Integer> out) {
			head = cons.head().compileMatch(out);
			tail = cons.tail().compileMatch(out);
		}

		public boolean match(ECons c, EMatchContext r) {
			return (!c.isNil()) && c.head().match(head, r) && c.tail().match(tail, r);
		}
	}

	/** matcher for all the non-recursive types */
	static class ValuePattern extends ETermPattern {
		final EObject value;

		/**
		 * @param epid
		 */
		public ValuePattern(EObject val) {
			this.value = val;
		}

		public boolean match(EPID pid, EMatchContext r) {
			return pid.equalsExactly(value);
		}

		public boolean match(ERef ref, EMatchContext r) {
			return ref.equalsExactly(value);
		}

		public boolean match(EPort port, EMatchContext r) {
			return port.equalsExactly(value);
		}

		public boolean match(EAtom port, EMatchContext r) {
			return port.equalsExactly(value);
		}

		public boolean match(EFun fu, EMatchContext r) {
			return fu.equalsExactly(value);
		}

		public boolean match(EBitString port, EMatchContext r) {
			return port.equalsExactly(value);
		}

		public boolean match(ENumber num, EMatchContext r) {
			return num.equalsExactly(value);
		}

		public boolean match(ETuple t, EMatchContext r) {
			return t.equalsExactly(value);
		}

		@Override
		public boolean match(ECons c, EMatchContext r) {
			return c.equalsExactly(value);
		}

	}

	/**
	 * @param eTuple
	 * @param out
	 * @return
	 */
	public static ETermPattern compilePattern(ETuple tup, Set<Integer> out) {
		TuplePattern tp = new TuplePattern(tup, out);
		for (int i = 0; i < tp.elems.length; i++) {
			if (!(tp.elems[i] instanceof ValuePattern)) {
				return tp;
			}
		}
		return new ValuePattern(tup);
	}

	/** generic compare-equals matcher */
	public static ETermPattern compilePattern(EObject epid, Set<Integer> out) {
		return new ValuePattern(epid);
	}

	/**
	 * @param eCons
	 * @param out
	 * @return
	 */
	public static ETermPattern compilePattern(ECons cons, Set<Integer> out) {
		if (cons.isNil()) {
			return new NilPattern();
		} else {
			return new ConsPattern(cons, out);
		}
	}

	static Pattern VAR = Pattern.compile("^\\$[0-9]+$");
	static EAtom am_ANY = EAtom.intern("_");

	/**
	 * @param eAtom
	 * @param out
	 * @return
	 */
	public static ETermPattern compilePattern(EAtom am, Set<Integer> out) {
		String name = am.getName();

		if (VAR.matcher(name).matches()) {
			int idx1 = Integer.parseInt(name.substring(1));
			return new VarPattern(idx1, out);
		} else if (am == am_ANY) {
			return new AnyPattern();
		} else {
			return new ValuePattern(am);
		}
	}

	/**
	 * @param keypos1
	 * @return
	 */
	public EObject getKey(int keypos1) {
		if (matcher instanceof TuplePattern) {
			TuplePattern tm = (TuplePattern) matcher;
			if (keypos1 < 1 || keypos1 > tm.elems.length) {
				return null;
			}
			ETermPattern m = tm.elems[keypos1 - 1];
			if (m instanceof ValuePattern) {
				ValuePattern vm = (ValuePattern) m;
				return vm.value;
			}
		} else if (matcher instanceof ValuePattern) {
			ValuePattern vp = (ValuePattern) matcher;
			if (vp.value instanceof ETuple) {
				ETuple et = (ETuple) vp.value;
				if (keypos1 < 1 || keypos1 > et.arity())
					return null;
				return et.elm(keypos1);
			}
		}
		return null;
	}

	/**
	 * @param res
	 * @param candidate
	 * @return
	 */
	ESeq match(ESeq out, ETuple val) {
		EMatchContext res = new EMatchContext(out_vars, val);

		if (matcher.match(val, res)) {
			out = out.cons(res.makeList());
		}

		return out;
	}

	ESeq match_members(ESeq out, ETuple val) {
		EMatchContext res = new EMatchContext(out_vars, val);

		if (matcher.match(val, res)) {
			out = out.cons(val);
		}

		return out;
	}

	boolean match(ETuple val) {
		EMatchContext res = new EMatchContext(out_vars, val);
		return matcher.match(val, res);
	}

}
