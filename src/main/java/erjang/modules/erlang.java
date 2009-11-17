package erjang.modules;

import java.math.BigInteger;

import erjang.BIF;
import erjang.EAtom;
import erjang.EBig;
import erjang.EBinary;
import erjang.ECons;
import erjang.EDouble;
import erjang.EInteger;
import erjang.EList;
import erjang.ENil;
import erjang.ENode;
import erjang.ENumber;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.ERT;
import erjang.ESeq;
import erjang.EString;
import erjang.ETuple;
import erjang.ETuple3;
import erjang.ErlFun;
import erjang.Module;
import erjang.NotImplemented;
import erjang.BIF.Type;
import erjang.beam.EProc;

@Module("erlang")
public class erlang {

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
	static public EObject setelement(int a1, EObject a2, EObject a3) {
		throw new NotImplemented();
	}

	@BIF
	@ErlFun(export = true)
	static public EObject element(EInteger idx, EObject obj) {
		ETuple tup;
		if ((tup = obj.testTuple()) != null && tup.arity() >= idx.value) {
			return tup.elm(idx.value);
		}
		throw ERT.badarg("erlang", "element", idx, obj);
	}

	@BIF
	static public EObject element(EInteger idx, ETuple tup) {
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

	@BIF
	static public EObject hd(EObject cell) {
		ECons cons;
		if ((cons = cell.testCons()) != null) {
			return cons.head();
		}
		throw ERT.badarg("erlang", "hd", cell);
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
	static public EInteger length$p(EObject list) {
		ESeq seq;
		if ((seq = list.testSeq()) != null) {
			return new EInteger(seq.length());
		}
		return null;
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

	// process dict

	@BIF
	@ErlFun(export = true)
	static public ECons get() {
		return null;
	}

	@BIF
	@ErlFun(export = true)
	static public EObject get(EObject key) {
		return null;
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

	@BIF(name = "div")
	static public ENumber div(EObject v1, int v2) {
		ENumber n1;
		if ((n1 = v1.testNumber()) != null) {
			return n1.div(v2);
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
			return new EInteger(intres);
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
				return n1.minus(n2);
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
				return n1.minus(n2);
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
		throw ERT.badarg((Throwable) null, "erlang", "+", v1, v2);
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
		throw ERT.badarg((Throwable) null, "erlang", "+", v1, v2);
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
		throw ERT.badarg((Throwable) null, "erlang", "trunc", v1);
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
		return v1.asb();
	}

	@BIF(name = "now")
	@ErlFun(export = true)
	static public ETuple3 now() {

		long now = System.currentTimeMillis();
		long megas = now / 1000000000;
		long secs = (now / 1000) % 1000000;
		long micros = (now % 1000) * 1000;

		ETuple3 res = new ETuple3();

		res.elem1 = new EInteger((int) megas);
		res.elem2 = new EInteger((int) secs);
		res.elem2 = new EInteger((int) micros);

		return res;
	}

	// tests

	@BIF(name = "is_eq", type = Type.GUARD)
	public static final boolean is_eq$p(EObject a1, EObject a2) {
		return a1.equals(a2);
	}

	@BIF(name = "is_eq_exact", type = Type.GUARD)
	public static final boolean is_eq_exact$p(EObject a1, EAtom a2) {
		return a1 == a2;
	}

	@BIF(name = "is_eq_exact", type = Type.GUARD)
	public static final boolean is_eq_exact$p(EObject a1, EObject a2) {
		return a1.equals(a2);
	}

	@BIF(name = "=:=", type = Type.GUARD)
	public static final boolean is_eq_exact$g(EObject a1, EAtom a2) {
		return a1 == a2;
	}

	@BIF(name = "=:=", type = Type.GUARD)
	public static final boolean is_eq_exact$g(EObject a1, EObject a2) {
		return a1.equals(a2);
	}

	@BIF(name = "is_ne_exact", type = Type.GUARD)
	public static final boolean is_ne_exact(EObject a1, EObject a2) {
		return !a1.equals(a2);
	}

	@BIF(name = "is_ge", type = Type.GUARD)
	public static final boolean is_ge(EObject a1, EObject a2) {
		throw new NotImplemented();
	}

	@BIF(name = "is_ne", type = Type.GUARD)
	public static final boolean is_ne(EObject a1, EObject a2) {
		return !a1.equals(a2);
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
		return o.testCons();
	}

	@BIF
	public static boolean is_nil(EObject o) {
		return o == ENil.NIL;
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
	public static EList nodes() {
		throw new NotImplemented();
	}

	@BIF(name = "is_atom", type = Type.GUARD)
	public static EAtom is_atom$p(EObject obj) {
		return obj.testAtom();
	}

	@BIF
	public static boolean is_atom(EObject obj) {
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

}
