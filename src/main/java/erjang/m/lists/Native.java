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


package erjang.m.lists;

import kilim.Pausable;
import erjang.BIF;
import erjang.EAtom;
import erjang.EFun;
import erjang.EInteger;
import erjang.EList;
import erjang.ENative;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.NotImplemented;

public class Native extends ENative {
	
	@BIF
	public static EObject keymember(EObject key, EObject nth_arg, EObject list_arg) {
		ESmall nth = nth_arg.testSmall();
		ESeq list = list_arg.testSeq();
		
		if (key == null || nth == null | list == null)
				throw ERT.badarg(key, nth_arg, list_arg);

		while (!list.isNil()) {
			EObject elm = list.head();
			ETuple tup = elm.testTuple();

			// test that it is a tuple of the right size
			if (tup != null && tup.arity() >= nth.value) {				
				EObject val = tup.elm(nth.value);
				if (val.equals(key)) { return ERT.TRUE; }
			}
			
			list = list.tail();
		}

		return ERT.FALSE;
	}
	
	@BIF
	public static EObject keyfind(EObject key, EObject nth_arg, EObject list_arg) {
		ESmall nth = nth_arg.testSmall();
		ESeq list = list_arg.testSeq();
		
		if (key == null || nth == null | list == null)
				throw ERT.badarg(key, nth_arg, list_arg);

		while (!list.isNil()) {
			EObject elm = list.head();
			ETuple tup = elm.testTuple();

			// test that it is a tuple of the right size
			if (tup != null && tup.arity() >= nth.value) {				
				EObject val = tup.elm(nth.value);
				if (val.equals(key)) { return tup; }
			}
			
			list = list.tail();
		}

		return ERT.FALSE;
	}
	
	@BIF
	public static EObject keysearch(EObject k, EObject n, EObject list) {
		ESmall idx = n.testSmall();
		ESeq src = list.testSeq();
		
		if (k==null||idx==null||src==null||idx.value<1)
			throw ERT.badarg(k, n, list);
		
		int index = idx.value;

		while (!src.isNil()) {
			EObject elm = src.head();
			
			ETuple tup;
			if ((tup = elm.testTuple()) != null) {
				if (tup.arity() >= index) {
					if (tup.elm(index).equals(k)) {
						return new ETuple2(ERT.am_value, tup);
					}
				}
			}
			
			src = src.tail();
		}
		
		return ERT.FALSE;
	}

	
	@BIF
	public static ESeq reverse(EObject hd, EObject tl) {
		ESeq res = tl.testSeq();
		ESeq front = hd.testSeq();
		
		if (res == null) throw ERT.badarg(hd, tl);
		
		return reverse(front, res);
	}

	@BIF
	public static ESeq reverse(ESeq front, ESeq res) {		
		while (!front.isNil()) {
			res = res.cons(front.head());
			front = front.tail();
		}
		
		return res;
	}
	@BIF
	public static EAtom member(EObject e, EObject l) {
		ESeq list = l.testSeq();
		if (list == null) throw ERT.badarg(e, l);

		while (!list.isNil()) {
			if (e.equals(list.head())) return ERT.TRUE;
			list = list.tail();
		}

		return ERT.FALSE;
	}

	@BIF
	public static ESeq map(EProc proc, EObject f, EObject s) throws Pausable {
		EFun fun = f.testFunction2(1);
		ESeq seq = s.testSeq();
		if (fun == null || seq == null) throw ERT.badarg(f, s);
		
		EObject[] arg = new EObject[1];
		
		ESeq rev = ERT.NIL;
		for (; !seq.isNil(); seq = seq.tail()) {
			arg[0] = seq.head();
			EObject val = fun.invoke(proc, arg);
			rev = rev.cons( val );
		}

		return reverse(rev, ERT.NIL);
	}
	
	@BIF
	public static ESeq seq(EObject start, EObject end)
	{
		ESmall sm_s = start.testSmall();
		ESmall sm_e = end.testSmall();
		
		if (sm_s == null || sm_e == null) {
			EInteger i_s;
			EInteger i_e;
			if ((i_s=start.testInteger()) == null 
				|| (i_e=end.testInteger()) == null) {

				throw ERT.badarg(start, end);
				
			}
			
			return seq_big(i_s, i_e);
		}
		
		if ((sm_e.value+1) < sm_s.value)
			throw ERT.badarg(start, end);
		
		ESeq l = ERT.NIL;
		int val = sm_e.value;
		int first = sm_s.value;
		
		while (val >= first) {
			l = l.cons(val);
			val -= 1;
		}
		
		return l;
	}
	
	static ESeq seq_big(EInteger start, EInteger end)
	{
		if ((end.inc()).is_lt(start))
			throw ERT.badarg(start, end);
		
		ESeq l = ERT.NIL;
		EInteger val = end;
		EInteger first = start;
		
		while (val.is_ge(first)) {
			l = l.cons(val);
			val = val.dec();
		}
		
		return l;
		
	}
	
}
