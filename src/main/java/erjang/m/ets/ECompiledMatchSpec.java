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

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import clojure.lang.ISeq;

import erjang.EAtom;
import erjang.EBitString;
import erjang.ECons;
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
public class ECompiledMatchSpec extends EObject {

	TupleMatcher matcher;
	private final ETuple spec;
	private int out_length;
	
	/**
	 * @param spec
	 */
	public ECompiledMatchSpec(int keyidx, ETuple spec) {
		this.spec = spec;
		Set<Integer> out = new HashSet<Integer>();
		
		matcher = new TupleMatcher(spec, out);
		
		// find max element index
		int max = 0;
		for (int i : out) {
			max = Math.max(i, max);
		}
		
		// make sure we match out all values in $1, $2, ..., $<max>
		for (int i = 1; i <= max; i++) {
			if (!out.contains(i)) 
				throw ERT.badarg(spec);
		}
		
		this.out_length = max;
	}

	ESeq match(ESeq out, Map<EObject, ETuple> in) {
		
		EObject[] res = new EObject[out_length];
		
		for (ETuple elm : in.values())
		{
			ETuple val = elm.testTuple();
			if (matcher.match(val, res)) {
				out = out.cons(EList.make(res));
			}
		}
		
		return out;
	}
	
	public static ECompiledMatchSpec compile(int keyidx, ETuple spec)
	{
		return new ECompiledMatchSpec(keyidx, spec);
	}
	
	/** matcher for '_' */
	static class AnyMatcher extends ETermMatcher {
		public boolean match(ETuple t, EObject[] r) {
			return true;
		}

		public boolean match(ENumber n, EObject[] r) {
			return true;
		}

		public boolean match(EAtom a, EObject[] r) {
			return true;
		}

		public boolean match(ECons c, EObject[] r) {
			return true;
		}

		public boolean match(EPID p, EObject[] r) {
			return true;
		}

		public boolean match(EPort p, EObject[] r) {
			return true;
		}

		public boolean match(EBitString bs, EObject[] r) {
			return true;
		}

	}
	
	/** matcher for '$N' */
	static class VarMatcher extends ETermMatcher {
		int idx0;

		/**
		 * @param idx2
		 * @param out
		 */
		public VarMatcher(int idx1, Set<Integer> out) {
			this.idx0 = idx1-1;
			if (out.contains(idx1)) {
				// the match expression contains "$<idx>" twice!
				throw ERT.badarg();
			}
			out.add(idx1);
		}

		public boolean match(ETuple t, EObject[] r) {
			r[idx0] = t;
			return true;
		}

		public boolean match(ENumber n, EObject[] r) {
			r[idx0] = n;
			return true;
		}

		public boolean match(EAtom a, EObject[] r) {
			r[idx0] = a;
			return true;
		}

		public boolean match(ECons c, EObject[] r) {
			r[idx0] = c;
			return true;
		}

		public boolean match(EPID p, EObject[] r) {
			r[idx0] = p;
			return true;
		}

		public boolean match(EPort p, EObject[] r) {
			r[idx0] = p;
			return true;
		}

		public boolean match(EBitString bs, EObject[] r) {
			r[idx0] = bs;
			return true;
		}
	}

	static class TupleMatcher extends ETermMatcher {
		int arity;
		ETermMatcher[] elems;

		/**
		 * @param tup
		 * @param out
		 */
		public TupleMatcher(ETuple tup, Set<Integer> out) {
			arity = tup.arity();
			elems = new ETermMatcher[arity];
			for (int idx1 = 1; idx1 <= arity; idx1++) {
				elems[idx1-1] = tup.elm(idx1).compileMatch(out);
			}
		}

		public boolean match(ETuple t, EObject[] r) {
			if (t.arity() != arity)
				return false;
			for (int idx1 = 1; idx1 < elems.length + 1; idx1++) {
				if (!t.elm(idx1).match(elems[idx1-1], r))
					return false;
			}
			return true;
		}
	}

	static class NilMatcher extends ETermMatcher {
		@Override
		public boolean match(ECons c, EObject[] r) {
			return c.isNil();
		}
	}
	
	static class ConsMatcher extends ETermMatcher {
		
		ETermMatcher head, tail;

		/**
		 * @param cons
		 * @param out
		 */
		public ConsMatcher(ECons cons, Set<Integer> out) {
			head = cons.head().compileMatch(out);
			tail = cons.tail().compileMatch(out);
		}

		public boolean match(ECons c, EObject[] r) {
			return c.head().match(head, r) 
				&& c.tail().match(tail, r);
		}
	}

	/** matcher for all the non-recursive types */
	static class ValueMatcher extends ETermMatcher {
		final EObject value;

		/**
		 * @param epid
		 */
		public ValueMatcher(EObject val) {
			this.value = val;
		}

		public boolean match(EPID pid, EObject[] r) {
			return pid.compareTo(value) == 0;
		}
		
		public boolean match(ERef ref, EObject[] r) {
			return ref.compareTo(value) == 0;
		}

		public boolean match(EPort port, EObject[] r) {
			return port.compareTo(value) == 0;
		}

		public boolean match(EAtom port, EObject[] r) {
			return port.compareTo(value) == 0;
		}

		public boolean match(EBitString port, EObject[] r) {
			return port.compareTo(value) == 0;
		}

		public boolean match(ENumber num, EObject[] r) {
			return num.compareTo(value) == 0;
		}

	}

	/**
	 * @param eTuple
	 * @param out
	 * @return
	 */
	public static ETermMatcher compileMatch(ETuple tup, Set<Integer> out) {
		return new TupleMatcher(tup, out);
	}

	/** generic compare-equals matcher */
	public static ETermMatcher compileMatch(EObject epid, Set<Integer> out) {
		return new ValueMatcher(epid);
	}

	/**
	 * @param eCons
	 * @param out
	 * @return
	 */
	public static ETermMatcher compileMatch(ECons cons, Set<Integer> out) {
		if (cons.isNil()) {
			return new NilMatcher();
		} else {
			return new ConsMatcher(cons, out);
		}
	}

	static Pattern VAR = Pattern.compile("^\\$[1-9][0-9]*$");
	static EAtom am_ANY =  EAtom.intern("_");
	
	/**
	 * @param eAtom
	 * @param out
	 * @return
	 */
	public static ETermMatcher compileMatch(EAtom am, Set<Integer> out) {
		String name = am.getName();
		
		if (VAR.matcher(name).matches()) {
			int idx1 = Integer.parseInt(name.substring(1));
			return new VarMatcher(idx1, out);
		} else if (am == am_ANY) {
			return new AnyMatcher();
		} else {
			return new ValueMatcher(am);
		}
	}

	/**
	 * @param keypos1
	 * @return
	 */
	public EObject getKey(int keypos1) {
		if (keypos1 < 1 || keypos1 > matcher.elems.length) {
			throw new IllegalStateException();
		}
		ETermMatcher m = matcher.elems[keypos1-1];
		if (m instanceof ValueMatcher) {
			ValueMatcher vm = (ValueMatcher) m;
			return vm.value;
		} else {
			return null;
		}
	}

	/**
	 * @param res
	 * @param candidate
	 * @return
	 */
	ESeq match(ESeq out, ETuple val) {
		EObject[] res = new EObject[out_length];
		
		if (matcher.match(val, res)) {
			out = out.cons(ETuple.make(res));
		}
		
		return out;	
	}

	
}
