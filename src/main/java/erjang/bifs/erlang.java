package erjang.bifs;

import java.math.BigInteger;

import erjang.BIF;
import erjang.EAtom;
import erjang.EBig;
import erjang.EBinary;
import erjang.ECons;
import erjang.EDouble;
import erjang.ESmall;
import erjang.EList;
import erjang.ENil;
import erjang.ENode;
import erjang.ENumber;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.EString;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ETuple3;
import erjang.ErlFun;
import erjang.Module;
import erjang.NotImplemented;
import erjang.BIF.Type;

/** bifs for the module erlang */
@Module("erlang")
public class erlang {

	@BIF
	@ErlFun(export = true)
	static public EPID self(EProc proc) {
		return proc.self();
	}
	
	@BIF @ErlFun(export=true)
	static public EObject link(EObject other) {
		throw new NotImplemented();
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
	static public EString list_to_atom(EObject obj) {
		ECons list;
		if ((list = obj.testCons()) != null) {
			return EString.make(list);
		}
		throw ERT.badarg();
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
	static public EObject setelement(EObject a1, EObject a2, EObject a3) {
		throw new NotImplemented();
	}

	@BIF
	static public EObject setelement(int a1, ETuple a2, EObject a3) {
		throw new NotImplemented();
	}

	@BIF(type=Type.GUARD, name="element")
	static public EObject element$g(EObject idx, EObject tup) {
		ESmall i = idx.testInteger();
		ETuple t = tup.testTuple();
		if (i == null || t == null) {
			return null;
		}
		if (i.intValue() > t.arity()) return null;
		
		return t.elm(i.asInt());
	}
	
	@BIF
	static public EObject element(EObject idx, EObject tup) {
		ESmall i = idx.testInteger();
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
		throw ERT.badarg("erlang", "element", idx, obj);
	}

	@BIF
	static public EObject element(ESmall idx, ETuple tup) {
		if (tup.arity() >= idx.value) {
			return tup.elm(idx.value);
		}
		throw ERT.badarg("erlang", "element", idx, tup);
	}

	@BIF
	static public EObject element(int idx, ETuple tup) {
		if (tup.arity() >= idx) {
			return tup.elm(idx);
		}
		throw ERT.badarg("erlang", "element", idx, tup);
	}

	@BIF
	static public EObject element(int idx, EObject obj) {
		ETuple tup;
		if ((tup = obj.testTuple()) != null && tup.arity() >= idx) {
			return tup.elm(idx);
		}
		throw ERT.badarg("erlang", "element", idx, obj);
	}

	@BIF
	static public EObject hd(ECons cell) {
		return cell.head();
	}

	@BIF(type=Type.GUARD,name="hd")
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

	@BIF(type=Type.GUARD,name="hd")
	static public EObject hd$p(EObject cell) {
		ECons cons;
		if ((cons = cell.testCons()) != null) {
			return cons.head();
		}
		return null;
	}

	@BIF
	static public int length(EObject list) {
		ESeq seq;
		if ((seq = list.testSeq()) != null) {
			return seq.length();
		}
		throw ERT.badarg("erlang", "length", list);
	}

	@BIF
	static public int length(ESeq list) {
		return list.length();
	}

	@BIF(name = "length", type = Type.GUARD)
	static public ESmall length$p(EObject list) {
		ESeq seq;
		if ((seq = list.testSeq()) != null) {
			return new ESmall(seq.length());
		}
		return null;
	}

	@BIF
	static public EObject whereis(EObject list) {
		throw new NotImplemented();
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
	static public EObject put(EProc proc, EObject key, EObject value)
	{
		return proc.put(key, value);
	}
	
	@BIF 
	static public EString name(EObject a1, EObject a2)
	{
		throw new NotImplemented();
	}

	@BIF
	@ErlFun(export = true)
	static public ENode node() {
		return null;
	}

	@BIF
	@ErlFun(export = true)
	static public ENode node(EObject name) {
		return null;
	}

	@BIF(type=Type.GUARD,name="node")
	@ErlFun(export = true)
	static public ENode node$p(EObject name) {
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
		if (v2 == 0.0)
			throw ERT.badarith("erlang", "/", v1, v2);

		return v1 / v2;
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


	@BIF(name = "div")
	static public ENumber div(EObject v1, int v2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			return n1.idiv(v2);
		}
		throw ERT.badarg();
	}

	@BIF(name = "div")
	static public ENumber div(ENumber n1, int v2) {
		return n1.div(v2);
	}

	@BIF(name = "-")
	static public ENumber minus(EObject v1, int v2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			return n1.minus(v2);
		}
		throw ERT.badarg("erlang", "-", v1, v2);
	}

	@BIF(name = "-")
	static public ENumber minus(int v1, int v2) {
		long res = (long) v1 - (long) v2;
		int intres = (int) res;
		if (res == intres)
			return new ESmall(intres);
		else
			return new EBig(BigInteger.valueOf(res));
	}

	@BIF(name = "-")
	@ErlFun(name = "-", export = true)
	static public ENumber minus(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.testNumber()) != null) {
				return n1.subtract(n2);
			}
		}
		throw ERT.badarg((Throwable) null, "erlang", "-", v1, v2);
	}

	@BIF(name = "-", type = Type.GUARD)
	static public ENumber subtract$p(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.testNumber()) != null) {
				return n1.subtract(n2);
			}
		}
		return null;
	}

	@BIF(name = "/", type = Type.GUARD)
	static public ENumber divide$p(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.testNumber()) != null) {
				if (n2.intValue() == 0)
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
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.testNumber()) != null) {
				return n1.add(n2);
			}
		}
		return null;
	}

	@BIF(name = "+")
	static public ENumber plus(int v1, int v2) {
		return ENumber.valueFor((long) v1 + (long) v2);
	}

	@BIF(name = "+")
	static public ENumber plus(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.testNumber()) != null) {
				return n1.add(n2);
			}
		}
		throw ERT.badarg(v1, v2);
	}

	@BIF(name = "+")
	static public ENumber plus(EObject v1, int i2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			return n1.add(i2);
		}
		throw ERT.badarg(v1, i2);
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

	@BIF(name = "*")
	static public ENumber multiply(int v1, int v2) {
		return ENumber.valueFor((long) v1 * (long) v2);
	}

	@BIF(name = "trunc")
	@ErlFun(export = true)
	static public ENumber trunc(EObject v1) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			return n1.trunc();
		}
		throw ERT.badarg();
	}

	@BIF(name = "trunc")
	static public double trunc(double d) {
		return Math.floor(d);
	}

	@BIF(name = "trunc")
	static public double trunc(EDouble d1) {
		return Math.floor(d1.value);
	}

	@BIF(name = "round")
	static public double round(double d) {
		return Math.round(d);
	}

	@BIF(name = "round")
	static public double round(EDouble d) {
		return Math.round(d.value);
	}

	@BIF(name = "round")
	static public double round(EObject o) {
		EDouble d;
		if ((d=o.testFloat()) == null) throw ERT.badarg();
		return Math.round(d.value);
	}

	@BIF
	@ErlFun(export = true)
	static public ENumber rem(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.testNumber()) != null) {
				return n1.rem(n2);
			}
		}
		throw ERT.badarg("erlang", "rem", v1, v2);
	}

	@BIF(name = "rem", type = Type.GUARD)
	static public ENumber rem$p(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.testNumber()) != null) {
				return n1.rem(n2);
			}
		}
		return null;
	}

	@BIF(name = "rem")
	static public ENumber rem(EObject v1, int v2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			return n1.rem(v2);
		}
		throw ERT.badarg("erlang", "rem", v1, v2);
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
		throw ERT.badarg("erlang", "abs", v1);
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
		long secs = (now / 1000) % 1000000;
		long micros = (now % 1000) * 1000;

		ETuple3 res = new ETuple3();

		res.elem1 = new ESmall((int) megas);
		res.elem2 = new ESmall((int) secs);
		res.elem2 = new ESmall((int) micros);

		return res;
	}

	// tests

	@BIF(name = "is_eq", type = Type.GUARD)
	public static final EAtom is_eq$p(EObject a1, EObject a2) {
		return a1.equals(a2) ? ERT.ATOM_TRUE : ERT.ATOM_FALSE;
	}

	@BIF(name = "is_eq_exact", type = Type.GUARD)
	public static final boolean is_eq_exact$p(EObject a1, EAtom a2) {
		return a1 == a2;
	}

	@BIF(name = "is_eq_exact", type = Type.GUARD)
	public static final boolean is_eq_exact$p(EObject a1, EObject a2) {
		return a1.equals(a2);
	}

	@BIF(name = "=/=")
	public static final boolean is_ne_exact(EObject a1, EAtom a2) {
		return a1 != a2;
	}

	@BIF(name = "=/=")
	public static final boolean is_ne_exact(EObject a1, EObject a2) {
		return !a1.equals(a2);
	}

	@BIF
	public static final int size(EObject o) {
		throw new NotImplemented();
	}
	
	@BIF(name = "=:=", type = Type.GUARD)
	public static final boolean is_eq_exact$g(EObject a1, EAtom a2) {
		return a1 == a2;
	}

	@BIF(name = "=:=", type = Type.GUARD)
	public static final EAtom is_eq_exact$g(EObject a1, EObject a2) {
		return a1.equals(a2) ? ERT.ATOM_TRUE : null;
	}

	@BIF(name = "==", type = Type.GUARD)
	public static final EAtom is_eq_op$g(EObject a1, EObject a2) {
		return a1.equals(a2) ? ERT.ATOM_TRUE : null;
	}

	@BIF(name = "==")
	public static final EAtom is_eq_op(EObject a1, EObject a2) {
		return a1.equals(a2) ? ERT.ATOM_TRUE : ERT.ATOM_FALSE;
	}

	@BIF(name = "is_ne_exact", type = Type.GUARD)
	public static final boolean is_ne_exact$g(EObject a1, EObject a2) {
		return !a1.equals(a2);
	}

	@BIF(name = "is_ge", type = Type.GUARD)
	public static final boolean is_ge(EObject a1, EObject a2) {
		throw new NotImplemented();
	}

	@BIF(name = "is_ne", type = Type.GUARD)
	public static final EAtom is_ne(EObject a1, EObject a2) {
		return a1.equals(a2) ? null : ERT.ATOM_TRUE;
	}

	@BIF(name = "is_lt", type = Type.GUARD)
	public static final boolean is_lt(EObject a1, EObject a2) {
		// return a1.equals(a2);

		throw new NotImplemented();
	}

	@BIF
	public static final EAtom is_eq(EObject a1, EObject a2) {
		return a1.equals(a2) ? ERT.ATOM_TRUE : ERT.ATOM_FALSE;
	}

	@BIF(name = "=:=")
	public static final EAtom is_eq_exact(EObject a1, EObject a2) {
		return a1.equals(a2) ? ERT.ATOM_TRUE : ERT.ATOM_FALSE;
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
		return o==null ? ENil.NIL : o.testCons();
	}

	@BIF
	public static boolean is_nil(EObject o) {
		return o==null || o == ENil.NIL;
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
	public static EObject process_flag(EObject a1, EObject a2) {
		throw new NotImplemented();
	}

	@BIF
	public static EObject process_info(EObject pid, EObject what) {
		throw new NotImplemented();
	}

	@BIF
	public static EObject process_info(EObject pid) {
		throw new NotImplemented();
	}

	@BIF
	public static EList nodes() {
		throw new NotImplemented();
	}

	@BIF(name = "is_atom", type = Type.GUARD)
	public static EAtom is_atom$p(EObject obj) {
		return obj==null?null:obj.testAtom();
	}

	@BIF
	public static boolean is_atom(EObject obj) {
		return obj.testAtom() != null;
	}

	@BIF(name = "is_list", type = Type.GUARD)
	public static ECons is_list$p(EObject obj) {
		return obj.testCons();
	}


	@BIF(name = "is_tuple", type = Type.GUARD)
	public static ETuple is_tuple$p(EObject obj) {
		return obj==null?null:obj.testTuple();
	}

	@BIF
	public static boolean is_tuple(EObject obj) {
		return obj.testAtom() != null;
	}

	@BIF(name = "is_binary", type = Type.GUARD)
	public static EBinary is_binary$p(EObject obj) {
		return obj.testBinary();
	}

	@BIF
	public static boolean is_binary(EObject obj) {
		return obj.testBinary() != null;
	}

	@BIF @ErlFun(export=true)
	public static ETuple2 load_module(EObject mod, EObject bin) {
		EAtom name = mod.testAtom();
		EBinary binary = bin.testBinary();
		return load_module(name, binary);
	}
	
	@BIF @ErlFun(export=true)
	public static ETuple2 load_module(EAtom mod, EBinary bin) {
		if (mod == null || bin == null) throw ERT.badarg();
		
		return ERT.load_module(mod, bin);
	}
	
	@BIF 
	public static boolean is_integer(EObject o) {
		return (o.testInteger() != null);
	}
	
	@BIF
	public static int tuple_size(ETuple tup) {
		return tup.arity();
	}
	
	@BIF
	public static int tuple_size(EObject tup) {
		ETuple t;
		if ((t=tup.testTuple()) == null) throw ERT.badarg();
		return t.arity();
	}
	
	@BIF
	public static int byte_size(EObject tup) {
		EBinary bin = tup.testBinary();
		if (bin == null) throw ERT.badarg();
		return bin.byte_size();
	}
	
	@BIF
	public static int bit_size(EObject tup) {
		EBinary bin = tup.testBinary();
		if (bin == null) throw ERT.badarg();
		return bin.bit_size();
	}

	@BIF
	public static boolean or(EObject o1, EObject e2) {
		return (o1==ERT.ATOM_TRUE) || (e2==ERT.ATOM_TRUE);
	}
	
	@BIF
	public static boolean and(EObject o1, EObject e2) {
		return (o1==ERT.ATOM_TRUE) & (e2==ERT.ATOM_TRUE);
	}
	
	@BIF(name="<")
	public static boolean lt(EObject v1, EObject v2) {
		throw new NotImplemented();
	}
	
	@BIF(name=">")
	public static boolean gt(EObject v1, EObject v2) {
		throw new NotImplemented();
	}
	
	@BIF(type=Type.GUARD,name="or")
	public static EAtom or$g(EObject o1, EObject o2) {
		if (or(o1,o2)) { return ERT.ATOM_TRUE; }
		else return null;
	}
	
	@BIF(type=Type.GUARD,name="and")
	public static EAtom and$g(EObject o1, EObject o2) {
		if (and(o1,o2)) { return ERT.ATOM_TRUE; }
		else return null;
	}
	
	@BIF
	public static EAtom not(EObject o1) {
		return (o1==ERT.ATOM_FALSE) ? ERT.ATOM_TRUE : ERT.ATOM_FALSE;
	}
	
}
