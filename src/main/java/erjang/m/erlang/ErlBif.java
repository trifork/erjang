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

package erjang.m.erlang;

import kilim.Pausable;
import kilim.Task;
import erjang.BIF;
import erjang.EAtom;
import erjang.EBinary;
import erjang.ECons;
import erjang.EDouble;
import erjang.EFun;
import erjang.EHandle;
import erjang.EInteger;
import erjang.EList;
import erjang.EModule;
import erjang.ENumber;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.EString;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ETuple3;
import erjang.ErlFun;
import erjang.ErlangError;
import erjang.FunID;
import erjang.Module;
import erjang.NotImplemented;
import erjang.BIF.Type;

/** bifs for the module erlang */
@Module("erlang")
public class ErlBif {

	@BIF
	static EObject apply(EProc proc, EObject fun, EObject args) throws Pausable {
		ESeq a = args.testWellformedList();
		if (a==null) throw ERT.badarg(fun,args);
		
		EFun f = fun.testFunction();
		if (f != null) {
			return f.apply(proc, a);
		}

		ETuple t = fun.testTuple();
		if (t == null) {
			throw ERT.badarg(fun,args);
		}

		ETuple2 t2 = ETuple2.cast(t);
		if (t2 == null) {
			throw ERT.badarg(fun,args);
		}
		
		EAtom mn = t2.elem1.testAtom();
		EAtom fn = t2.elem2.testAtom();
		
		FunID funspec;
		f = EModule.resolve(funspec=new FunID(mn,fn,a.length()));
		
		if (f == null) {
			throw ERT.undef(funspec, a.toArray());
		}
		
		return f.apply(proc, a);
	}
	
	@BIF
	static ESeq binary_to_list(EObject val) {
		EBinary bin;
		if ((bin=val.testBinary()) == null) throw ERT.badarg(val);
		return EString.fromBinary(bin);
	}
	
	
	@BIF
	static EObject apply(EProc proc, EObject one, EObject two, EObject three) throws Pausable {
		throw new NotImplemented();		
	}
	
	@BIF
	@ErlFun(export = true)
	static public EPID self(EProc proc) {
		return proc.self();
	}

	@BIF
	@ErlFun(export = true)
	static public ETuple3 date() {
		throw new NotImplemented();
	}

	@BIF
	@ErlFun(export = true)
	static public EString integer_to_list(EObject num) {
		return new EString(num.toString());
	}

	@BIF
	@ErlFun(export = true)
	static public EAtom list_to_atom(EObject obj) {
		EString es;
		if ((es = obj.testString()) != null) {
			return EAtom.intern(es.stringValue());
		}
		throw ERT.badarg(obj);
	}

	@BIF
	@ErlFun(export = true)
	static public EPID list_to_pid(EObject obj) {
		ECons list;
		if ((list = obj.testCons()) != null) {
			EString s = EString.make(list);
			return ERT.loopkup_pid(s);
		}
		throw ERT.badarg();
	}

	@BIF
	@ErlFun(export = true)
	static public EString pid_to_list(EObject obj) {
		EPID pid;
		if ((pid = obj.testPID()) != null) {
			return pid.getName();
		}
		throw ERT.badarg();
	}

	@BIF
	@ErlFun(export = true)
	static public EString port_to_list(EObject obj) {
		EPort port;
		if ((port = obj.testPort()) != null) {
			return port.getName();
		}
		throw ERT.badarg();
	}

	@BIF
	@ErlFun(export = true)
	static public EString float_to_list(EObject obj) {
		EDouble value;
		if ((value = obj.testFloat()) != null) {
			return value.to_list();
		}
		throw ERT.badarg();
	}

	@BIF
	@ErlFun(export = true)
	static public EObject error(EObject err) {
		throw new NotImplemented();
	}

	@BIF
	@ErlFun(export = true)
	static public EObject get_module_info(EObject mod) {
		throw new NotImplemented();
	}

	@BIF
	@ErlFun(export = true)
	static public EObject get_module_info(EObject mod, EObject fun) {
		throw new NotImplemented();
	}

	@BIF
	static public EObject setelement(EObject a1, EObject a2, EObject term) {
		ETuple t = a2.testTuple();
		ESmall i = a1.testSmall();
		if (t == null || i == null) throw ERT.badarg();
		return t.setelement(i.value, term);
	}

	@BIF
	static public EObject setelement(int index, ETuple a2, EObject term) {
		return a2.setelement(index, term);
	}

	@BIF(type = Type.GUARD, name = "element")
	static public EObject element$g(EObject idx, EObject tup) {
		EInteger i = idx.testInteger();
		ETuple t = tup.testTuple();
		if (i == null || t == null) {
			return null;
		}
		if (i.intValue() > t.arity())
			return null;

		return t.elm(i.asInt());
	}

	@BIF
	static public EObject element(EObject idx, EObject tup) {
		EInteger i = idx.testInteger();
		ETuple t = tup.testTuple();
		if (i == null || t == null || i.intValue() > t.arity()) {
			throw ERT.badarg();
		}

		return t.elm(i.asInt());
	}

	@BIF
	@ErlFun(export = true)
	static public EObject element(ESmall idx, EObject obj) {
		ETuple tup;
		if ((tup = obj.testTuple()) != null && tup.arity() >= idx.value) {
			return tup.elm(idx.value);
		}
		throw ERT.badarg(idx, obj);
	}

	@BIF
	static public EObject element(ESmall idx, ETuple tup) {
		if (tup.arity() >= idx.value) {
			return tup.elm(idx.value);
		}
		throw ERT.badarg(idx, tup);
	}

	@BIF
	static public EObject element(int idx, ETuple tup) {
		if (tup.arity() >= idx) {
			return tup.elm(idx);
		}
		throw ERT.badarg(new ESmall(idx), tup);
	}

	@BIF
	static public EObject element(int idx, EObject obj) {
		ETuple tup;
		if ((tup = obj.testTuple()) != null && tup.arity() >= idx) {
			return tup.elm(idx);
		}
		throw ERT.badarg(new ESmall(idx), obj);
	}

	@BIF
	static public EObject hd(ECons cell) {
		return cell.head();
	}

	@BIF(type = Type.GUARD, name = "hd")
	static public EObject hd$p(ECons cell) {
		return cell.head();
	}

	@BIF
	static public EObject hd(EObject cell) {
		ECons cons;
		if ((cons = cell.testCons()) != null) {
			return cons.head();
		}
		throw ERT.badarg();
	}

	@BIF
	static public EObject tl(EObject cell) {
		ECons cons;
		if ((cons = cell.testCons()) != null) {
			return cons.tail();
		}
		throw ERT.badarg();
	}

	@BIF(type = Type.GUARD, name = "hd")
	static public EObject hd$p(EObject cell) {
		ECons cons;
		if ((cons = cell.testCons()) != null) {
			return cons.head();
		}
		return null;
	}

	@BIF(type = Type.GUARD, name = "tl")
	static public EObject tl$p(EObject cell) {
		ECons cons;
		if ((cons = cell.testCons()) != null) {
			return cons.tail();
		}
		return null;
	}

	@BIF
	static public EInteger length(EObject list) {
		ESeq seq;
		if ((seq = list.testSeq()) != null) {
			return ERT.box(seq.length());
		}
		throw ERT.badarg(list);
	}

	@BIF
	static public int length(ESeq list) {
		return list.length();
	}

	@BIF(name = "length", type = Type.GUARD)
	static public ESmall length$p(EObject list) {
		ESeq seq;
		if ((seq = list.testSeq()) != null) {
			return ERT.box(seq.length());
		}
		return null;
	}

	@BIF
	static public EObject whereis(EProc proc, EObject regname) {
		return ERT.whereis(regname);
	}

	@BIF
	static public EObject port_info(EObject a1, EObject a2) {
		throw new NotImplemented();
	}

	@BIF
	static public EObject statistics(EObject list) {
		throw new NotImplemented();
	}

	@BIF
	static public EObject put(EProc proc, EObject key, EObject value) {
		return proc.put(key, value);
	}

	@BIF
	static public EString name(EObject a1, EObject a2) {
		throw new NotImplemented();
	}

	@BIF
	@ErlFun(export = true)
	static public EAtom node() {
		return null;
	}

	@BIF
	@ErlFun(export = true)
	static public EAtom node(EObject name) {
		return null;
	}

	@BIF(type = Type.GUARD, name = "node")
	@ErlFun(export = true)
	static public EAtom node$p(EObject name) {
		return null;
	}

	// process dict

	@BIF
	@ErlFun(export = true)
	static public ECons get(EProc proc) {
		return proc.get();
	}

	@BIF
	@ErlFun(export = true)
	static public EObject get(EProc proc, EObject key) {
		return proc.get(key);
	}

	// floats

	@BIF(type = Type.ARITHBIF)
	static public double fdiv(double v1, double v2) {
		test_zero(v1, v2);
		return v1 / v2;
	}

	private static void test_zero(double v1, double v2) {
		if (v2 == 0.0)
			throw new ErlangError(ERT.AM_BADARITH, ERT.NIL.cons(v2).cons(v1));
	}

	@BIF(type = Type.ARITHBIF)
	static public double fsub(double v1, double v2) {
		return v1 - v2;
	}

	@BIF(type = Type.ARITHBIF)
	static public double fadd(double v1, double v2) {
		return v1 + v2;
	}

	@BIF(type = Type.ARITHBIF)
	static public double fmul(double v1, double v2) {
		return v1 * v2;
	}

	// arithmetic

	@BIF(name = "-")
	static public ENumber neg(EObject v1) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			return n1.negate();
		}
		throw ERT.badarg();
	}

	@BIF
	static public EInteger div(EObject o1, EObject o2) {
		return o1.idiv(o2);
	}

	@BIF(name = "div")
	static public ENumber div(EObject v1, int v2) {
		return v1.idiv(v2);
	}

	@BIF(name = "div")
	static public ENumber div(ENumber n1, int v2) {
		return n1.idiv(v2);
	}

	@BIF(name = "-")
	static public ENumber minus(EObject v1, int v2) {
		return v1.subtract(v2);
	}

	@BIF(name = "-")
	static public ENumber minus(int v1, int v2) {
		return ERT.box((long) v1 - (long) v2);
	}

	@BIF(name = "-")
	@ErlFun(name = "-", export = true)
	static public ENumber minus(EObject v1, EObject v2) {
		return v1.subtract(v2, false);
	}

	@BIF(name = "-", type = Type.GUARD)
	static public ENumber subtract$p(EObject v1, EObject v2) {
		return v1.subtract(v2, true);
	}

	@BIF(name = "/", type = Type.GUARD)
	static public ENumber divide$p(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.testNumber()) != null) {
				if (n2.doubleValue() == 0.0)
					return null;
				return n1.divide(v2);
			}
		}
		return null;
	}

	@BIF(name = "/", type = Type.GUARD)
	static public ENumber divide$p(EObject v1, double d2) {
		ENumber n1;
		if (d2 != 0.0 && (n1 = v1.testNumber()) != null) {
			return n1.divide(d2);
		}
		return null;
	}

	@BIF(name = "+", type = Type.GUARD)
	static public ENumber plus$p(EObject v1, EObject v2) {
		return v1.add(v2, true);
	}

	@BIF(name = "+", type = Type.GUARD)
	static public ENumber plus$p(EObject v1, ESmall s2) {
		return v1.add(s2.value, true);
	}

	@BIF(name = "+")
	static public ENumber plus(int v1, int v2) {
		return ERT.box((long) v1 + (long) v2);
	}

	@BIF(name = "+")
	static public ENumber plus(EObject v1, EObject v2) {
		return v1.add(v2, false);
	}

	@BIF(name = "+")
	static public ENumber plus(EObject v1, int i2) {
		return v1.add(i2, false);
	}

	@BIF(name = "+")
	static public ENumber plus(EObject v1, ESmall i2) {
		return v1.add(i2.value, false);
	}

	@BIF(name = "-")
	static public ENumber minus(EObject v1, ESmall i2) {
		return v1.subtract(i2.value);
	}

	@BIF(name = "-", type = Type.GUARD)
	static public ENumber minus$g(EObject v1, ESmall i2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			return n1.subtract(i2.value);
		}
		return null;
	}

	@BIF(name = "*")
	static public ENumber multiply(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.testNumber()) != null) {
				return n1.multiply(n2);
			}
		}
		throw ERT.badarg(v1, v2);
	}

	@BIF(name = "*", type=Type.GUARD)
	static public ENumber multiply$g(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.testNumber()) != null) {
				return n1.multiply(n2);
			}
		}
		return null;
	}

	@BIF(name = "*")
	static public ENumber multiply(int v1, int v2) {
		return ERT.box((long) v1 * (long) v2);
	}

	@BIF
	@ErlFun(export = true)
	static public EDouble trunc(EObject v1) {
		EDouble n1;
		if ((n1 = v1.testFloat()) != null) {
			return trunc(n1.value);
		}
		throw ERT.badarg();
	}

	@BIF
	static public EDouble trunc(double d) {
		return ERT.box(Math.floor(d));
	}

	@BIF
	static public EDouble trunc(EDouble d1) {
		return ERT.box(Math.floor(d1.value));
	}

	@BIF(name = "round")
	static public EInteger round(double d) {
		return ERT.box(Math.round(d));
	}

	@BIF(name = "round")
	static public EInteger round(EDouble d) {
		return ERT.box(Math.round(d.value));
	}

	@BIF(name = "round")
	static public EInteger round(EObject o) {
		EDouble d;
		if ((d = o.testFloat()) == null)
			throw ERT.badarg();
		return ERT.box(Math.round(d.value));
	}

	@BIF
	@ErlFun(export = true)
	static public ENumber rem(EObject v1, EObject v2) {
		return v1.irem(v2);
	}

	@BIF(name = "rem", type = Type.GUARD)
	static public EInteger rem$p(EObject v1, EObject v2) {
		if (v2.equals(ESmall.ZERO)) {
			return null;
		} else {
			return v1.irem(v2);
		}
	}

	@BIF(name = "rem")
	static public EInteger rem(EObject v1, int v2) {
		return v1.irem(v2);
	}

	@BIF(name = "abs", type = Type.GUARD)
	static public ENumber abs$p(EObject v1) {
		ENumber num;
		if ((num = v1.testNumber()) != null) {
			return abs(num);
		}
		return null;
	}

	@BIF(name = "abs")
	@ErlFun(export = true)
	static public ENumber abs(EObject v1) {
		ENumber num;
		if ((num = v1.testNumber()) != null) {
			return abs(num);
		}
		throw ERT.badarg(v1);
	}

	@BIF(name = "abs")
	static public ENumber abs(ENumber v1) {
		return v1.abs();
	}

	@BIF(name = "now")
	@ErlFun(export = true)
	static public ETuple3 now() {

		long now = System.currentTimeMillis();
		long megas = now / 1000000000;
		long secs = ((now % 1000000000) / 1000);
		long micros = (now % 1000) * 1000;

		ETuple3 res = new ETuple3();

		res.elem1 = ERT.box(megas);
		res.elem2 = ERT.box(secs);
		res.elem3 = ERT.box(micros);

		//System.out.println("now() => "+res);
		
		return res;
	}

	// tests

	@BIF(name = "==", type = Type.GUARD)
	public static final EAtom is_eq$p(EObject a1, EObject a2) {
		return ERT.box(a1.compareTo(a2) == 0);
	}

	@BIF(name = "=/=", type = Type.GUARD)
	public static final EAtom is_ne_exact$g2(EObject a1, EObject a2) {
		return ERT.guard(!a1.equalsExactly(a2));
	}

	@BIF(name = "=/=", type = Type.GUARD)
	public static final EAtom is_ne_exact$g(EObject a1, EAtom a2) {
		return ERT.guard(a1 != a2);
	}

	@BIF(name = "=/=")
	public static final EAtom is_ne_exact(EObject a1, EAtom a2) {
		return ERT.box(a1 != a2);
	}

	@BIF(name = "=/=")
	public static final EAtom is_ne_exact(EObject a1, EObject a2) {
		return ERT.box(!a1.equalsExactly(a2));
	}

	// what are these supposed to do? Get the size of a tuple_

	@BIF
	public static final EInteger size(EObject o) {
		ETuple t;
		if ((t = o.testTuple()) == null)
			throw ERT.badarg(o);
		return ERT.box(t.arity());
	}

	@BIF
	public static final EInteger size(ETuple t) {
		return ERT.box(t.arity());
	}

	@BIF(type = Type.GUARD, name = "size")
	public static final EInteger size$g(EObject o) {
		ETuple t;
		if ((t = o.testTuple()) == null)
			return null;
		return ERT.box(t.arity());
	}

	@BIF(type = Type.GUARD, name = "size")
	public static final EInteger size$g(ETuple t) {
		return ERT.box(t.arity());
	}

	@BIF(name = "=:=", type = Type.GUARD)
	public static final EAtom eqxp(EObject a1, EAtom a2) {
		return ERT.guard(a1 == a2);
	}

	@BIF(name = "=:=", type = Type.GUARD)
	public static final EAtom eqxp(EObject a1, ESmall s2) {
		ESmall s1;
		if ((s1 = a1.testSmall()) != null) {
			return ERT.guard(s1.value == s2.value);
		}

		return ERT.guard(a1.equalsExactly(s2));
	}

	@BIF(name = "=:=", type = Type.GUARD)
	public static final EAtom eqxp(EObject a1, EObject a2) {
		return ERT.guard(a1.equalsExactly(a2));
	}

	@BIF(name = "==", type = Type.GUARD)
	public static final EAtom is_eq_op$g(EObject a1, EObject a2) {
		return ERT.guard(a1.compareTo(a2) == 0);
	}

	@BIF(name = "==")
	public static final EAtom is_eq_op(EObject a1, EObject a2) {
		return a1.equals(a2) ? ERT.TRUE : ERT.FALSE;
	}

	@BIF(name = "=/=", type = Type.GUARD)
	public static final EAtom is_ne_exact$g(EObject a1, EObject a2) {
		return !a1.equals(a2) ? ERT.TRUE : null;
	}

	@BIF(name = ">=", type = Type.GUARD)
	public static final EAtom is_ge$g2(EObject a1, EObject a2) {
		return a1.compareTo(a2) >= 0 ? ERT.TRUE : null;
	}

	@BIF(name = "is_ge", type = Type.GUARD)
	public static final EAtom is_ge$g(EObject a1, EObject a2) {
		return a1.compareTo(a2) >= 0 ? ERT.TRUE : null;
	}

	@BIF(name = "/=")
	public static final EAtom is_ne(EObject a1, EObject a2) {
		return ERT.box(a1.compareTo(a2) != 0);
	}

	@BIF(name = "/=", type = Type.GUARD)
	public static final EAtom is_ne$g(EObject a1, EObject a2) {
		return ERT.guard(a1.compareTo(a2) != 0);
	}

	@BIF(name = "<", type = Type.GUARD)
	public static final EAtom is_lt$g(EObject a1, EObject a2) {
		return ERT.guard(a1.compareTo(a2) < 0);
	}

	@BIF(name = "=<")
	public static final EAtom is_le(EObject a1, EObject a2) {
		return ERT.box(a1.compareTo(a2) <= 0);
	}

	@BIF(name = "<")
	public static final EAtom is_lt(EObject a1, ESmall a2) {
		return ERT.box( a2.compareTo(a1) >= 0 );
	}

	@BIF(name = "<")
	public static final EAtom is_lt(ESmall a1, EObject a2) {
		return ERT.box( a1.compareTo(a2) < 0 );
	}

	@BIF(name = "=<", type = Type.GUARD)
	public static final EAtom is_le$g(EObject a1, EObject a2) {
		return ERT.guard(a1.compareTo(a2) <= 0);
	}

	@BIF(name = "<")
	public static final EAtom is_lt(EObject a1, EObject a2) {
		return ERT.box(a1.compareTo(a2) < 0);
	}

	@BIF(name = ">=")
	public static final EAtom is_ge(EObject a1, EObject a2) {
		return ERT.box(a1.compareTo(a2) >= 0);
	}

	@BIF
	public static final EAtom is_eq(EObject a1, EObject a2) {
		return ERT.box(a1.compareTo(a2) == 0);
	}

	@BIF(name = "=:=")
	public static final EAtom is_eq_exact(EObject a1, EObject a2) {
		return ERT.box(a1.equalsExactly(a2));
	}

	@BIF(name = "++")
	public static ECons append(EObject l1, EObject l2) {
		ECons c1 = is_list(l1);
		ECons c2 = is_list(l2);

		if (c1 == null || c2 == null)
			throw ERT.badarg();

		return c2.prepend(c1);
	}

	@BIF(name = "++")
	public static ECons append(ECons l1, ECons l2) {
		return l2.prepend(l1);
	}

	@BIF(name = "++")
	public static ECons append(EObject o1, ECons l2) {
		ECons l1;
		if ((l1 = is_list(o1)) != null)
			return l2.prepend(l1);
		throw ERT.badarg();
	}

	@BIF
	public static ECons is_list(EObject o) {
		return o == null ? ERT.NIL : o.testCons();
	}

	@BIF
	public static EAtom is_nil(EObject o) {
		return ERT.box(o.testNil() != null);
	}

	@BIF
	@ErlFun(export = true)
	public static EString atom_to_list(EObject atom) {
		EAtom am = atom.testAtom();
		if (am == null)
			throw ERT.badarg();
		return new EString(am.getName());
	}

	@BIF
	public static EObject process_flag(EProc proc, EObject a1, EObject a2) {
		return proc.process_flag(a1.testAtom(), a2);
	}

	@BIF
	public static EList nodes() {
		throw new NotImplemented();
	}

	@BIF(name = "is_atom", type = Type.GUARD)
	public static EAtom is_atom$p(EObject obj) {
		return obj == null ? null : obj.testAtom();
	}

	@BIF
	public static EAtom is_atom(EObject obj) {
		return (obj.testAtom() != null) ? ERT.TRUE : ERT.FALSE;
	}

	@BIF(name = "is_list", type = Type.GUARD)
	public static ECons is_list$p(EObject obj) {
		return obj.testCons();
	}

	@BIF(name = "is_tuple", type = Type.GUARD)
	public static ETuple is_tuple$p(EObject obj) {
		return obj == null ? null : obj.testTuple();
	}

	@BIF
	public static EAtom is_tuple(EObject obj) {
		return ERT.box(obj.testAtom() != null);
	}

	@BIF(name = "is_binary", type = Type.GUARD)
	public static EAtom is_binary$p(EObject obj) {
		return ERT.guard(obj.testBinary() != null);
	}

	@BIF
	public static EAtom is_binary(EObject obj) {
		return ERT.box(obj.testBinary() != null);
	}

	@BIF(name = "is_integer", type = Type.GUARD)
	public static EAtom is_integer$p(EObject obj) {
		return ERT.guard(obj.testInteger() != null);
	}

	@BIF
	@ErlFun(export = true)
	public static ETuple2 load_module(EObject mod, EObject bin) {
		EAtom name = mod.testAtom();
		EBinary binary = bin.testBinary();
		return load_module(name, binary);
	}

	@BIF
	@ErlFun(export = true)
	public static ETuple2 load_module(EAtom mod, EBinary bin) {
		if (mod == null || bin == null)
			throw ERT.badarg();

		return ERT.load_module(mod, bin);
	}

	@BIF
	public static EAtom is_integer(EObject o) {
		return ERT.box(o.testInteger() != null);
	}

	@BIF
	public static ESmall tuple_size(ETuple tup) {
		return ERT.box(tup.arity());
	}

	@BIF
	public static ESmall tuple_size(EObject tup) {
		ETuple t;
		if ((t = tup.testTuple()) == null)
			throw ERT.badarg();
		return ERT.box(t.arity());
	}

	@BIF(type = Type.GUARD, name = "tuple_size")
	public static ESmall tuple_size$g(EObject tup) {
		ETuple t;
		if ((t = tup.testTuple()) == null)
			return null;
		return ERT.box(t.arity());
	}

	@BIF
	public static ESmall byte_size(EObject tup) {
		EBinary bin = tup.testBinary();
		if (bin == null)
			throw ERT.badarg();
		return ERT.box(bin.byteSize());
	}

	
	@BIF(type=Type.GUARD, name="byte_size")
	public static ESmall byte_size$g(EObject o) {
		EBinary bin = o.testBinary();
		if (bin == null)
			return null;
		return ERT.box(bin.byteSize());
	}

	@BIF
	public static EInteger bit_size(EObject tup) {
		EBinary bin = tup.testBinary();
		if (bin == null)
			throw ERT.badarg();
		return ERT.box(bin.bitCount());
	}


	@BIF(type=Type.GUARD, name="bit_size")
	public static EInteger bit_size$g(EObject tup) {
		EBinary bin = tup.testBinary();
		if (bin == null)
			return null;
		return ERT.box(bin.bitCount());
	}

	@BIF
	public static EAtom or(EObject o1, EObject e2) {
		return ERT.box((o1 == ERT.TRUE) || (e2 == ERT.TRUE));
	}

	@BIF
	public static EAtom and(EObject o1, EObject e2) {
		return ERT.box((o1 == ERT.TRUE) && (e2 == ERT.TRUE));
	}

	@BIF(name = "<")
	public static EAtom lt(EObject v1, EObject v2) {
		return ERT.box(v1.compareTo(v2) < 0);
	}

	@BIF(name = ">")
	public static EAtom gt(EObject v1, EObject v2) {
		return ERT.box(v1.compareTo(v2) > 0);
	}

	@BIF(type = Type.GUARD, name = "or")
	public static EAtom or$g(EObject o1, EObject o2) {
		return ERT.guard(o1 == ERT.TRUE || o2 == ERT.TRUE);
	}

	@BIF(type = Type.GUARD, name = "and")
	public static EAtom and$g(EObject o1, EObject o2) {
		return ERT.guard(o1 == ERT.TRUE && o2 == ERT.TRUE);
	}

	@BIF
	public static EAtom not(EObject o1) {
		return (o1 == ERT.FALSE) ? ERT.TRUE : ERT.FALSE;
	}

	@BIF
	public static EInteger bnot(EObject o) {
		return o.bnot();
	}

	@BIF
	public static EInteger bor(EObject o1, EObject o2) {
		return o1.bor(o2);
	}

	@BIF
	public static EInteger bxor(EObject o1, EObject o2) {
		return o1.bxor(o2);
	}

	@BIF
	public static EInteger band(EObject o1, EObject o2) {
		return o1.band(o2);
	}

	@BIF
	public static EInteger bsl(EObject o1, EObject o2) {
		return o1.bsl(o2);
	}

	@BIF
	public static EInteger bsr(EObject o1, EObject o2) {
		return o1.bsr(o2);
	}

	@BIF(type = Type.GUARD, name = "not")
	public static EAtom not$g(EObject o1) {
		return (o1 == ERT.FALSE) ? ERT.TRUE : ERT.FALSE;
	}

	@BIF(type = Type.GUARD, name = "bnot")
	public static EInteger bnot$g(EObject o) {
		EInteger i;
		if ((i = o.testInteger()) == null)
			return null;
		return i.bnot();
	}

	@BIF(type = Type.GUARD, name = "bor")
	public static EInteger bor$g(EObject o1, EObject o2) {
		EInteger i1;
		EInteger i2;
		if ((i1 = o1.testInteger()) == null || (i2 = o2.testInteger()) == null)
			return null;
		return i1.bor(i2);
	}

	@BIF(type = Type.GUARD, name = "bxor")
	public static EInteger bxor$g(EObject o1, EObject o2) {
		EInteger i1;
		EInteger i2;
		if ((i1 = o1.testInteger()) == null || (i2 = o2.testInteger()) == null)
			return null;
		return i1.bxor(i2);
	}

	@BIF(type = Type.GUARD, name = "band")
	public static EInteger band$g(EObject o1, EObject o2) {
		EInteger i1;
		EInteger i2;
		if ((i1 = o1.testInteger()) == null || (i2 = o2.testInteger()) == null)
			return null;
		return i1.band(i2);
	}

	@BIF(type = Type.GUARD, name = "bsl")
	public static EInteger bsl$g(EObject o1, EObject o2) {
		EInteger i1;
		EInteger i2;
		if ((i1 = o1.testInteger()) == null || (i2 = o2.testInteger()) == null)
			return null;
		return i1.bsl(i2);
	}

	@BIF(type = Type.GUARD, name = "bsr")
	public static EInteger bsr$g(EObject o1, EObject o2) {
		EInteger i1;
		EInteger i2;
		if ((i1 = o1.testInteger()) == null || (i2 = o2.testInteger()) == null)
			return null;
		return i1.bsr(i2);
	}
	
	@BIF(name="lists:reverse/2")
	public static ESeq reverse(EObject hd, EObject tl) {
		ESeq res = tl.testSeq();
		ESeq front = hd.testSeq();
		
		if (res == null) throw ERT.badarg(hd, tl);
		
		while (!front.isNil()) {
			res = res.cons(front.head());
			front = front.tail();
		}
		
		return res;
	}

	@BIF(name="erlang:bump_reductions/1")
	public static EObject bump_reductions(EProc self, EObject howmuch) throws Pausable {
		// yield?
		Task.yield();
		return ERT.box(1);
	}
	
}
