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

import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;
import java.util.logging.Level;
import java.util.logging.Logger;

import kilim.Pausable;
import kilim.Task;
import erjang.BIF;
import erjang.EAtom;
import erjang.EBinary;
import erjang.EBitString;
import erjang.ECons;
import erjang.EDouble;
import erjang.EFun;
import erjang.EInteger;
import erjang.EModuleManager;
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
import erjang.ErlangError;
import erjang.ErlangException;
import erjang.ErlangThrow;
import erjang.FunID;
import erjang.Module;
import erjang.NotImplemented;
import erjang.BIF.Type;

/** bifs for the module erlang */
@Module("erlang")
public class ErlBif {

    /**
	 * 
	 */
	private static final TimeZone UTC_TIME_ZONE = TimeZone.getTimeZone("UTC");
	private static Logger log = Logger.getLogger("erlang");
	private static EAtom am_wall_clock = EAtom.intern("wall_clock");
	private static EAtom am_reductions = EAtom.intern("reductions");
	private static EAtom am_garbage_collection = EAtom.intern("garbage_collection");
	private static EAtom am_runtime = EAtom.intern("runtime");
	private static final EAtom am_run_queue = EAtom.intern("run_queue");
	
	@BIF
	static EObject apply(EProc proc, EObject fun, EObject args) throws Pausable {
		ESeq a = args.testSeq();
		if (a==null) throw ERT.badarg(fun,args);
		
		EFun f = fun.testFunction();
		if (f != null) {
			return apply(proc, f, a);
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
		f = EModuleManager.resolve(funspec=new FunID(mn,fn,a.length()));
		
		if (f == null) {
			throw ERT.undef(funspec, a.toArray());
		}
		
		return apply(proc, f, a);
	}
	
	@BIF
	static ESeq binary_to_list(EObject val) {
		EBinary bin;
		if ((bin=val.testBinary()) == null) throw ERT.badarg(val);
		return EString.make(bin);
	}

	@BIF
	static EBinary list_to_binary(EObject val) {
		EString es;
		if ((es = val.testString()) != null) { 
			return es.asBitString();
		}

		ECons cons = val.testCons();
		if (cons == null) throw ERT.badarg(val);
		List<ByteBuffer> out = new ArrayList<ByteBuffer>();
		if (!cons.collectIOList(out)) {
			throw ERT.badarg(val);
		}

		if (out.size() == 1) {
			ByteBuffer b = out.get(0);
			return EBinary.make(b);
		}
		
		int length = 0;
		for (int i = 0; i < out.size(); i++) {
			length += out.get(i).remaining();
		}

		byte[] all = new byte[length];
		int pos = 0;
		for (int i = 0; i < out.size(); i++) {
			ByteBuffer bb = out.get(i);
			int len = bb.remaining();
			bb.get(all, pos, len);
			pos += len;
		}
		
		assert(pos == length);
		
		return new EBinary(all);
	}

	@BIF
	static ESmall iolist_size(EObject val) {

		EBinary bin;
		if ((bin=val.testBinary()) != null) {
			return new ESmall(bin.byteSize());
		}
		
		EString str;
		if ((str=val.testString()) != null) {
			return new ESmall(str.length());
		}
		
		
		ESeq seq;
		if ((seq = val.testSeq()) == null) {
			throw ERT.badarg(val);
		}
		
		ArrayList<ByteBuffer> al = new ArrayList<ByteBuffer>();
		seq.collectIOList(al);

		int size = 0;
		for (int i = 0; i < al.size(); i++) {
			size += al.get(i).remaining();
		}
		
		return new ESmall(size);
	}

	@BIF
	static EObject apply(EProc proc, EObject one, EObject two, EObject three) throws Pausable {
		EAtom mod = one.testAtom();
		EAtom fun = two.testAtom();
		ESeq  args = three.testSeq();
		
		if (mod==null||fun==null||args==null) throw ERT.badarg(one, two, three);
		
		EFun f = EModuleManager.resolve(new FunID(mod, fun, args.length()));
		
		return apply(proc, f, args);
	}

	private static EObject apply(EProc proc, EFun fun, ESeq args)
			throws Pausable {
		ESeq rargs = args.reverse();
		int len = args.length();
		switch(len) {		
		case 11:
			proc.arg10 = rargs.head(); rargs = rargs.tail();
		case 10:
			proc.arg9 = rargs.head(); rargs = rargs.tail();
		case 9:
			proc.arg8 = rargs.head(); rargs = rargs.tail();
		case 8:
			proc.arg7 = rargs.head(); rargs = rargs.tail();
		case 7:
			proc.arg6 = rargs.head(); rargs = rargs.tail();
		case 6:
			proc.arg5 = rargs.head(); rargs = rargs.tail();
		case 5:
			proc.arg4 = rargs.head(); rargs = rargs.tail();
		case 4:
			proc.arg3 = rargs.head(); rargs = rargs.tail();
		case 3:
			proc.arg2 = rargs.head(); rargs = rargs.tail();
		case 2:
			proc.arg1 = rargs.head(); rargs = rargs.tail();
		case 1:
			proc.arg0 = rargs.head();
		case 0:
			break;
			
		default:
			return fun.apply(proc, args);
		}
		
		proc.tail = fun;
		return EProc.TAIL_MARKER;
	}
	
	@BIF
	
	static public EPID self(EProc proc) {
		if (proc == null) {
			System.out.println("Houston, we have a problem.");
		}
		return proc.self_handle();
	}

	@BIF
	static public ETuple date() {
		GregorianCalendar cal = new GregorianCalendar();
		int year = cal.get(Calendar.YEAR);
		int month = cal.get(Calendar.MONTH)+1;
		int day = cal.get(Calendar.DAY_OF_MONTH);
		
		return ETuple.make(ERT.box(year), ERT.box(month), ERT.box(day));
	}

	@BIF
	static public ETuple time() {
		GregorianCalendar cal = new GregorianCalendar();
		int year = cal.get(Calendar.HOUR_OF_DAY);
		int month = cal.get(Calendar.MINUTE);
		int day = cal.get(Calendar.SECOND);
		
		return ETuple.make(ERT.box(year), ERT.box(month), ERT.box(day));
	}

	@BIF
	
	static public EString integer_to_list(EObject num) {
		return new EString(num.toString());
	}

	@BIF
	
	static public EAtom list_to_atom(EObject obj) {
		EString es;
		if ((es = obj.testString()) != null) {
			return EAtom.intern(es.stringValue());
		}
		throw ERT.badarg(obj);
	}

	@BIF
	
	static public EPID list_to_pid(EObject obj) {
		ECons list;
		if ((list = obj.testCons()) != null) {
			ESeq s = EString.make(list);
			return ERT.loopkup_pid(s);
		}
		throw ERT.badarg();
	}

	@BIF
	
	static public EString pid_to_list(EObject obj) {
		EPID pid;
		if ((pid = obj.testPID()) != null) {
			return pid.getName();
		}
		throw ERT.badarg();
	}

	@BIF
	
	static public EString port_to_list(EObject obj) {
		EPort port;
		if ((port = obj.testPort()) != null) {
			return port.getName();
		}
		throw ERT.badarg();
	}

	@BIF
	
	static public EString float_to_list(EObject obj) {
		EDouble value;
		if ((value = obj.testFloat()) != null) {
			return value.to_list();
		}
		throw ERT.badarg();
	}

	@BIF
	
	static public EObject error(EObject reason) {
		throw new ErlangError(reason);
	}

	@BIF
	
	static public EObject error(EObject reason, EObject args) {
		ESeq aseq = args.testSeq();
		if (aseq == null) throw ERT.badarg(reason, args);
		throw new ErlangError(reason, aseq.toArray());
	}

	@BIF(name="throw")
	
	static public EObject throw_ex(EObject reason) {
		throw new ErlangThrow(reason);
	}

	@BIF
	
	static public ESeq get_module_info(EObject mod) {
		// TODO: get all the attributes from the beam code
		ESeq res = ERT.NIL;
		res = res.cons(new ETuple2(ERT.am_attributes, 
								   get_module_info(mod, ERT.am_attributes)));
		return res;
	}

	@BIF
	
	static public EObject get_module_info(EObject mod, EObject key) {
		EAtom m = mod.testAtom();
		EAtom k = key.testAtom();
		
		if (m == null || k == null) {
			throw ERT.badarg(mod, key);
		}

		if (k == ERT.am_attributes) {
			return EModuleManager.get_attributes(m);
		}
		
		if (k == ERT.am_exports) {
			return EModuleManager.get_exports(m);
		}
		
		if (k == ERT.am_module) {
			return mod;
		}

		return ERT.am_undefined;
	}

	public static final String[] PRE_LOADED_MODULES = new String[] {
		"erlang",
		"erl_prim_loader",
		"init",
		"otp_ring0",
		"prim_file",
		"prim_inet",
		"prim_zip",
		"zlib"
	};
	
	@BIF
	public static ESeq pre_loaded() {
		ESeq res = ERT.NIL;
		for (int i = 0; i < PRE_LOADED_MODULES.length; i++) {
			res = res.cons(EAtom.intern(PRE_LOADED_MODULES[i]));
		}
		return res;
	}

	@BIF
	public static EObject garbage_collect() {
		System.gc();
		return ERT.TRUE;
	}
	
	@BIF
	public static EObject garbage_collect(EObject pid) {
		System.gc();
		return ERT.TRUE;
	}
	
	@BIF
	static public ETuple setelement(EObject a1, EObject a2, EObject term) {
		ETuple t = a2.testTuple();
		ESmall i = a1.testSmall();
		if (t == null || i == null) throw ERT.badarg(a1,a2,term);
		return t.setelement(i.value, term);
	}

	/** 
	 * Optimized version which does not test the types of arguments.
	 * Used in case the type-analysis can infer the exact argument types
	 * (which is the case for BEAM code generated for record-update). 
	 * */
	@BIF
	static public ETuple setelement(int index, ETuple a2, EObject term) {
		return a2.setelement(index, term);
	}

	@BIF
	static public ETuple setelement(ESmall index, ETuple a2, EObject term) {
		return a2.setelement(index.value, term);
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
			throw ERT.badarg(idx, tup);
		}

		return t.elm(i.asInt());
	}

	/*
	@BIF
	
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
	*/

	@BIF
	static public EObject element(int idx, ETuple tup) {
		if (tup.arity() >= idx) {
			return tup.elm(idx);
		}
		throw ERT.badarg(new ESmall(idx), tup);
	}

	@BIF
	static public EObject element(ESmall sidx, ETuple tup) {
		int idx = sidx.value;
		if (tup.arity() >= idx) {
			return tup.elm(idx);
		}
		throw ERT.badarg(sidx, tup);
	}

	@BIF
	static public EObject element(int idx, EObject obj) {
		ETuple tup;
		if ((tup = obj.testTuple()) != null && tup.arity() >= idx) {
			return tup.elm(idx);
		}
		throw ERT.badarg(new ESmall(idx), obj);
	}

	/*
	 * TODO: figure out why sofs.erl compiles to this with EInteger 1st arg
	@BIF
	static public EObject element(ESmall sidx, EObject obj) {
		int idx = sidx.value;
		ETuple tup;
		if ((tup = obj.testTuple()) != null && tup.arity() >= idx) {
			return tup.elm(idx);
		}
		throw ERT.badarg(new ESmall(idx), obj);
	}
*/
	
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
		if ((cons = cell.testNonEmptyList()) != null) {
			return cons.head();
		}
		throw ERT.badarg(cell);
	}

	@BIF
	static public EObject tl(EObject cell) {
		ECons cons;
		if ((cons = cell.testNonEmptyList()) != null) {
			return cons.tail();
		}
		throw ERT.badarg(cell);
	}

	@BIF(type = Type.GUARD, name = "hd")
	static public EObject hd$p(EObject cell) {
		ECons cons;
		if ((cons = cell.testNonEmptyList()) != null) {
			return cons.head();
		}
		return null;
	}

	@BIF(type = Type.GUARD, name = "tl")
	static public EObject tl$p(EObject cell) {
		ECons cons;
		if ((cons = cell.testNonEmptyList()) != null) {
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

	static final long wall_clock0 = System.currentTimeMillis();
	
	// TODO: figure out if this needs to be stored in the current process
	static long last_wall_clock = wall_clock0;
	static long last_reductions = 0;
	static long last_runtime = 0;

	@BIF
	static public EObject statistics(EProc proc, EObject spec) {
		
		if (spec == am_wall_clock) {
			long now = System.currentTimeMillis();
			long since_last = now-last_wall_clock;
			long since_epoch = now-wall_clock0;
			last_wall_clock = now;
			return ETuple.make(ERT.box(since_epoch), ERT.box(since_last));

		} else if (spec == am_reductions) {
			long current_reds = proc.reds;
			long since_last = current_reds - last_reductions;
			last_reductions = current_reds;
			
			return new ETuple2(ERT.box(current_reds),ERT.box(since_last));			

		} else if (spec == am_runtime) {
			
			// TODO: should return cpu time spent; can do?
			
			RuntimeMXBean b = ManagementFactory.getRuntimeMXBean();
			
			long current_runtime = b.getUptime();
			long since_last = current_runtime - last_runtime;
			last_runtime = current_runtime;
			
			return new ETuple2(ERT.box(current_runtime),ERT.box(since_last));	
			
		} else if (spec == am_garbage_collection) {
			
			List<GarbageCollectorMXBean> b = ManagementFactory.getGarbageCollectorMXBeans();

			long num_gcs = 0;
			long time_gcs = 0;

			for (GarbageCollectorMXBean bb : b) {
				num_gcs += bb.getCollectionCount();
				time_gcs += bb.getCollectionTime();
			}
			
			return ETuple.make(ERT.box(num_gcs), ERT.box(time_gcs), ERT.box(0));
			
		} else if (spec == am_run_queue) {
			return ERT.box(0);
		}
		
		throw new NotImplemented("erlang:statistics("+spec+")");
	}

	@BIF
	static public EObject put(EProc proc, EObject key, EObject value) {
		return proc.put(key, value);
	}

	@BIF
	static public EString name(EObject a1, EObject a2) {
		throw new NotImplemented();
	}


	// process dict

	@BIF
	
	static public ECons get(EProc proc) {
		return proc.get();
	}

	@BIF
	
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
	static public double fdiv(EDouble v1, double v2) {
		test_zero(v1.value, v2);
		return v1.value / v2;
	}

	@BIF(type = Type.ARITHBIF)
	static public double fsub(double v1, double v2) {
		return v1 - v2;
	}

	@BIF(type = Type.ARITHBIF)
	static public double fsub(EObject v1, EObject v2) {
		ENumber n1, n2;
		if ((n1 = v1.testNumber()) != null &&
		    (n2 = v2.testNumber()) != null)
		{
			return n1.doubleValue() - n2.doubleValue();
		}
		throw ERT.badarg();
	}


	@BIF(type = Type.ARITHBIF)
	static public double fadd(double v1, double v2) {
		return v1 + v2;
	}

	@BIF(type = Type.ARITHBIF)
	static public double fmul(double v1, double v2) {
		return v1 * v2;
	}

	@BIF(type = Type.ARITHBIF)
	static public double fmul(double v1, EObject v2) {
		EDouble d2;
		if ((d2=v2.testFloat()) == null) throw ERT.badarg(new EDouble(v1), v2);
		return v1 * d2.value;
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

	@BIF(name = "-", type=Type.GUARD)
	static public ENumber neg$g(EObject v1) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			return n1.negate();
		}
		return null;
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

	@BIF(name = "div", type = Type.GUARD)
	static public ENumber div$g(EObject v1, EObject v2) {
		ENumber n1, n2;
		if ((n1 = v1.testInteger()) != null &&
		    (n2 = v2.testInteger()) != null)
		{
			if (n2.equals(ESmall.ZERO)) return null;
			return n1.idiv(n2);
		}
		return null;
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

	@BIF(name = "/")
	static public ENumber divide(EObject v1, EObject v2) {
		return v1.divide(v2);
	}

	@BIF(type=Type.ARITHBIF)
	public static double fnegate(double val) { return -val; }
	
	@BIF
	public static EDouble fnegate(EDouble val) { return new EDouble(-val.value); }
	
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


	@BIF(name = "*")
	static public ENumber multiply(EObject v1, EObject v2) {
		return v1.multiply(v2);
	}

	@BIF
	static public EInteger trunc(EObject v1) {
		EInteger i1;
		if ((i1 = v1.testInteger()) != null) {
			return i1;
		}
		EDouble n1;
		if ((n1 = v1.testFloat()) != null) {
			return trunc(n1.value);
		}
		throw ERT.badarg();
	}

	@BIF
	static public EInteger trunc(double d) {
		//TODO: use (int)d or (long)d if magnitude is small
		return ERT.box(new BigDecimal(d).toBigInteger());
	}

	@BIF
	static public EInteger trunc(EDouble d1) {
		//TODO: use (int)d.value or (long)d.value if magnitude is small
		return ERT.box(new BigDecimal(d1.value).toBigInteger());
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
			throw ERT.badarg(o);
		return ERT.box(Math.round(d.value));
	}

	@BIF(name = "round", type = Type.GUARD)
	static public EInteger round$g(double d) {
		return ERT.box(Math.round(d));
	}

	@BIF(name = "round", type = Type.GUARD)
	static public EInteger round$g(EDouble d) {
		return ERT.box(Math.round(d.value));
	}

	@BIF(name = "round", type = Type.GUARD)
	static public EInteger round$g(EObject o) {
		EDouble d = o.testFloat();
		if (d == null) return null;
		return ERT.box(Math.round(d.value));
	}

	@BIF(name = "float")
	static public double float$n(int v) {
		return (double)v;
	}
	@BIF(name = "float")
	static public double float$n(ENumber v) {
		return v.doubleValue();
	}
	@BIF(name = "float")
	static public EDouble float$n(EObject v) {
		ENumber n = v.testNumber();
		if (n==null) throw ERT.badarg(v);
		return ERT.box(n.doubleValue());
	}

	@BIF
	static public ENumber rem(EObject v1, EObject v2) {
		return v1.irem(v2);
	}

	@BIF(name = "rem", type = Type.GUARD)
	static public EInteger rem$p(EObject v1, EObject v2) {
		if (v2.equals(ESmall.ZERO) || v1.testInteger()==null || v2.testInteger()==null) {
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
		return ERT.guard(a1.equals(a2));
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
		return ERT.guard(a1.equals(a2));
	}

	@BIF(name = "==")
	public static final EAtom is_eq_op(EObject a1, EObject a2) {
		return a1.equals(a2) ? ERT.TRUE : ERT.FALSE;
	}

	@BIF(name = "=/=", type = Type.GUARD)
	public static final EAtom is_ne_exact$g(EObject a1, EObject a2) {
		return ERT.guard( !a1.equalsExactly(a2) );
	}

	@BIF(name = ">=", type = Type.GUARD)
	public static final EAtom is_ge$g2(EObject a1, EObject a2) {
		return ERT.guard( a1.compareTo(a2) >= 0 );
	}

	@BIF(name = ">", type = Type.GUARD)
	public static final EAtom is_gt$g(EObject a1, EObject a2) {
		return ERT.guard( a1.compareTo(a2) > 0 );
	}

	@BIF(name = "is_ge", type = Type.GUARD)
	public static final EAtom is_ge$g(EObject a1, EObject a2) {
		return ERT.guard(a1.compareTo(a2) >= 0);
	}

	@BIF(name = "/=")
	public static final EAtom is_ne(EObject a1, EObject a2) {
		boolean eq = a1.equals(a2);
		return ERT.box(!eq);
	}

	@BIF(name = "/=", type = Type.GUARD)
	public static final EAtom is_ne$g(EObject a1, EObject a2) {
		boolean eq = a1.equals(a2);
		return ERT.guard(!eq);
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
	public static EObject append(EObject l1, EObject l2) {
		
		ESeq ll1 = l1.testSeq();
		if (ll1 == null) throw ERT.badarg(l1, l2);
		
		return l2.prepend(ll1);
	}

	@BIF
	public static EAtom is_list(EObject o) {
		return ERT.box(o.testCons() != null || o.testNil() != null);
	}

	@BIF
	public static EAtom is_nil(EObject o) {
		return ERT.box(o.testNil() != null);
	}

	@BIF
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
	public static ESeq nodes() {
		return ERT.getRemoteNodes();
	}

	@BIF(name = "is_atom", type = Type.GUARD)
	public static EAtom is_atom$p(EObject obj) {
		return ERT.guard(obj.testAtom() != null);
	}

	@BIF
	public static EAtom is_atom(EObject obj) {
		return (obj.testAtom() != null) ? ERT.TRUE : ERT.FALSE;
	}

	@BIF(name = "is_list", type = Type.GUARD)
	public static EAtom is_list$p(EObject obj) {
		return ERT.guard(obj.testCons() != null);
	}

	@BIF(name = "is_tuple", type = Type.GUARD)
	public static EAtom is_tuple$p(EObject obj) {
		return ERT.guard(obj.testTuple() != null);
	}

	@BIF
	public static EAtom is_tuple(EObject obj) {
		return ERT.box(obj.testTuple() != null);
	}

	@BIF(name = "is_binary", type = Type.GUARD)
	public static EAtom is_binary$p(EObject obj) {
		return ERT.guard(obj.testBinary() != null);
	}

	@BIF
	public static EAtom is_binary(EObject obj) {
		return ERT.box(obj.testBinary() != null);
	}

	@BIF
	public static EAtom is_bitstring(EObject obj) {
		return ERT.box(obj.testBitString() != null);
	}

	@BIF(name = "is_bitstring", type = Type.GUARD)
	public static EAtom is_bitstring$g(EObject obj) {
		return ERT.guard(obj.testBitString() != null);
	}

	@BIF
	public static EAtom is_boolean(EObject obj) {
		return ERT.box(obj==ERT.TRUE || obj==ERT.FALSE);
	}

	@BIF(type=Type.GUARD, name="is_boolean")
	public static EAtom is_boolean$g(EObject obj) {
		return ERT.guard(obj==ERT.TRUE || obj==ERT.FALSE);
	}

	@BIF(name = "is_integer", type = Type.GUARD)
	public static EAtom is_integer$p(EObject obj) {
		return ERT.guard(obj.testInteger() != null);
	}

	@BIF(name = "is_float", type = Type.GUARD)
	public static EAtom is_float$g(EObject obj) {
		return ERT.guard(obj.testFloat() != null);
	}

	@BIF(name = "is_number", type = Type.GUARD)
	public static EAtom is_number$g(EObject obj) {
		return ERT.guard(obj.testNumber() != null);
	}

	@BIF
	public static EAtom is_function(EObject obj) {
		return ERT.box(obj.testFunction() != null);
	}
	
	@BIF
	public static EAtom is_function(EObject obj, ESmall num) {
		return ERT.box(obj.testFunction2(num.value) != null);
	}
	
	@BIF(name="is_function", type=Type.GUARD)
	public static EAtom is_function_guard(EObject obj) {
		return ERT.guard(obj.testFunction() != null);
	}
	
	@BIF
	public static EAtom is_reference(EObject obj) {
		return ERT.box(obj.testReference() != null);
	}
	
	@BIF
	public static EAtom is_pid(EObject obj) {
		return ERT.box(obj.testPID() != null);
	}
	
	@BIF(name="is_pid", type=Type.GUARD)
	public static EAtom is_pid_guard(EObject obj) {
		return ERT.guard(obj.testPID() != null);
	}

	@BIF
	public static EAtom is_port(EObject obj) {
		return ERT.box(obj.testPort() != null);
	}
	
	@BIF(name="is_port", type=Type.GUARD)
	public static EAtom is_port$g(EObject obj) {
		return ERT.guard(obj.testPort() != null);
	}
	
	@BIF	
	public static ETuple2 load_module(EProc proc, EObject mod, EObject bin) throws Pausable {
		EAtom name = mod.testAtom();
		EBinary binary = bin.testBinary();
		return load_module(proc, name, binary);
	}

	@BIF
	public static ETuple2 load_module(EProc proc, EAtom mod, EBinary bin) {
		if (mod == null || bin == null)
			throw ERT.badarg();

		try {
			ERT.load_module(mod, bin);
		} catch (ErlangException e) {
			log.log(Level.FINE, "cannot load module", e);
			return new ETuple2(ERT.am_error, e.reason());
		} catch (ThreadDeath e) {
			throw e;
		} catch (Throwable e) {
			ErlangError ee = new ErlangError(ERT.am_badfile, e, mod, bin);
			ETuple2 result = new ETuple2(ERT.am_error, ee.reason());
			
			log.log(Level.SEVERE, "cannot load module "+mod, e);
			
			return result;
		} 
		
		return new ETuple2(ERT.am_module, mod);
	}
	
	@BIF
	public static ETuple make_tuple(EObject arity, EObject initial) {
		ESmall sm = arity.testSmall();
		if (sm == null) throw ERT.badarg(arity, initial);
		ETuple et = ETuple.make(sm.value);
		for (int i = 1; i <= sm.value; i++) {
			et.set(i, initial);
		}
		return et;
	}

	@BIF
	public static EAtom is_integer(EObject o) {
		return ERT.box(o.testInteger() != null);
	}

	@BIF
	public static EAtom is_float(EObject o) {
		return ERT.box(o.testFloat() != null);
	}

	@BIF
	public static EAtom is_number(EObject o) {
		return ERT.box(o.testNumber() != null);
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
	public static ESmall tuple_size_guard(EObject tup) {
		ETuple t;
		if ((t = tup.testTuple()) == null)
			return null;
		return ERT.box(t.arity());
	}

	@BIF
	public static ESmall byte_size(EObject o) {
		EBitString bin = o.testBitString();
		if (bin == null)
			throw ERT.badarg();
		return ERT.box(bin.totalByteSize());
	}

	
	@BIF(type=Type.GUARD, name="byte_size")
	public static ESmall byte_size_guard(EObject o) {
		EBitString bin = o.testBitString();
		if (bin == null)
			return null;
		return ERT.box(bin.totalByteSize());
	}

	@BIF
	public static EInteger bit_size(EObject o) {
		EBitString bin = o.testBitString();
		if (bin == null)
			throw ERT.badarg();
		return ERT.box(bin.bitSize());
	}


	@BIF(type=Type.GUARD, name="bit_size")
	public static EInteger bit_size_guard(EObject o) {
		EBitString bin = o.testBitString();
		if (bin == null)
			return null;
		return ERT.box(bin.bitSize());
	}

	@BIF
	public static EAtom or(EObject o1, EObject o2) {
		if (o1==ERT.TRUE) {
			if (o2==ERT.TRUE || o2==ERT.FALSE) return ERT.TRUE;
		} else if (o2==ERT.TRUE) {
			if (o1==ERT.FALSE) return ERT.TRUE;
		} else if (o1 == ERT.FALSE && o2 == ERT.FALSE) {
			return ERT.FALSE;
		}
		throw ERT.badarg(o1, o2);
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
		if (o1==ERT.TRUE) {
			if (o2==ERT.TRUE || o2==ERT.FALSE) return ERT.TRUE;
		} else if (o2==ERT.TRUE) {
			if (o1==ERT.FALSE) return ERT.TRUE;
		} else if (o1 == ERT.FALSE && o2 == ERT.FALSE) {
			return ERT.FALSE;
		}
		return null;
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
	public static EInteger band(EObject o1, ESmall o2) {
		return o2.band(o1);
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
		return ERT.box(o1 == ERT.FALSE);
	}

	@BIF
	public static EAtom xor(EObject o1, EObject o2) {
		EAtom a1, a2;
		if ((a1 = o1.testBoolean()) == null ||
		    (a2 = o2.testBoolean()) == null)
			throw ERT.badarg(o1, o2);
		boolean b1 = a1 == ERT.TRUE,
		        b2 = a2 == ERT.TRUE;
		return ERT.box(b1 ^ b2);
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

	@BIF
	public static EObject bump_reductions(EProc self, EObject howmuch) throws Pausable {
		// yield?
		Task.yield();
		return ERT.box(1);
	}
	
	@BIF
	public static ETuple2 localtime()
	{
		Calendar c = GregorianCalendar.getInstance();
		
		ETuple3 date = new ETuple3();
		date.set(1, ERT.box(c.get(Calendar.YEAR)));
		date.set(2, ERT.box(c.get(Calendar.MONTH)-Calendar.JANUARY+1));
		date.set(3, ERT.box(c.get(Calendar.DAY_OF_MONTH)));
		
		ETuple3 time = new ETuple3();
		time.set(1, ERT.box(c.get(Calendar.HOUR_OF_DAY)));
		time.set(2, ERT.box(c.get(Calendar.MINUTE)));
		time.set(3, ERT.box(c.get(Calendar.SECOND)));
		
		return new ETuple2(date, time);
	}
	

	@BIF
	public static ETuple2 universaltime()
	{
		Calendar c = GregorianCalendar.getInstance();
		
		c.setTimeZone(UTC_TIME_ZONE);
		
		ETuple3 date = new ETuple3();
		date.set(1, ERT.box(c.get(Calendar.YEAR)));
		date.set(2, ERT.box(c.get(Calendar.MONTH)-Calendar.JANUARY+1));
		date.set(3, ERT.box(c.get(Calendar.DAY_OF_MONTH)));
		
		ETuple3 time = new ETuple3();
		time.set(1, ERT.box(c.get(Calendar.HOUR_OF_DAY)));
		time.set(2, ERT.box(c.get(Calendar.MINUTE)));
		time.set(3, ERT.box(c.get(Calendar.SECOND)));
		
		return new ETuple2(date, time);
	}
	
	@BIF
	static public EObject localtime_to_universaltime(EObject a1, EObject a2)
	{
		throw new NotImplemented();
	}
	
	@BIF
	static public EObject system_flag(EObject flag_arg, EObject value)
	{
		throw new NotImplemented();
	}
	
	@BIF
	public static EObject md5(EObject iolist_arg)
	{
		
		List<ByteBuffer> buf = new ArrayList<ByteBuffer>();
		if (!iolist_arg.collectIOList(buf)) {
			throw ERT.badarg(iolist_arg);
		}
		
		MessageDigest md;
		 try {
			 md = MessageDigest.getInstance("MD5");
		} catch (NoSuchAlgorithmException e) {
			throw new NotImplemented();
		}
		
		for (int i = 0; i < buf.size(); i++) {
			md.update(buf.get(i));
		}
		
		byte[] res = md.digest();
		return EBinary.make(res, 0, res.length, 0);
	}
	
	@BIF
	public static EObject raise(EObject kind, EObject value, EObject trace)
			throws ErlangException {
		throw ERT.raise(kind, value, trace);
	}
	
	@BIF
	public static ESeq registered() {
		return ERT.registered();
	}
}
