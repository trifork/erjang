package erjang.modules;

import java.math.BigInteger;

import erjang.BIF;
import erjang.EAtom;
import erjang.EBig;
import erjang.ECons;
import erjang.EDouble;
import erjang.EInteger;
import erjang.ENode;
import erjang.ENumber;
import erjang.EObject;
import erjang.EPID;
import erjang.ERT;
import erjang.ESeq;
import erjang.ETuple;
import erjang.ETuple3;
import erjang.ErlFun;
import erjang.Module;
import erjang.BIF.Type;

@Module("erlang")
public class ErlangModule {

	static final EAtom ATOM_TRUE = EAtom.intern("true");
	static final EAtom ATOM_FALSE = EAtom.intern("false");
	
	static {
	}
	
	@BIF @ErlFun(export=true)
	static public EPID self() {
		return null;
	}

	@BIF @ErlFun(export=true)
	static public EObject element(EInteger idx, EObject obj) {
		ETuple tup;
		if ((tup = obj.asTuple()) != null && tup.arity() >= idx.value) {
			return tup.nth(idx.value);
		}
		throw ERT.badarg("erlang", "element", idx, obj);
	}

	@BIF
	static public EObject element(EInteger idx, ETuple tup) {
		if (tup.arity() >= idx.value) {
			return tup.nth(idx.value);
		}
		throw ERT.badarg("erlang", "element", idx, tup);
	}

	@BIF
	static public EObject element(int idx, ETuple tup) {
		if (tup.arity() >= idx) {
			return tup.nth(idx);
		}
		throw ERT.badarg("erlang", "element", idx, tup);
	}

	@BIF
	static public EObject element(int idx, EObject obj) {
		ETuple tup;
		if ((tup = obj.asTuple()) != null && tup.arity() >= idx) {
			return tup.nth(idx);
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
		if ((cons = cell.asCons()) != null) {
			return cons.head();
		}
		throw ERT.badarg("erlang", "hd", cell);
	}

	@BIF
	static public int length(EObject list) {
		ESeq seq;
		if ((seq = list.asSeq()) != null) {
			return seq.length();
		}
		throw ERT.badarg("erlang", "length", list);
	}

	@BIF
	static public int length(ESeq list) {
		return list.length();
	}

	@BIF(name="length", type=Type.GUARD)
	static public EInteger length$guard(EObject list) {
		ESeq seq;
		if ((seq = list.asSeq()) != null) {
			return new EInteger(seq.length());
		}
		return null;
	}

	@BIF @ErlFun(export=true)
	static public ENode node() {
		return null;
	}

	@BIF @ErlFun(export=true)
	static public ENode node(EObject name) {
		return null;
	}

	// process dict

	@BIF @ErlFun(export=true)
	static public ECons get() {
		return null;
	}

	@BIF @ErlFun(export=true)
	static public EObject get(EObject key) {
		return null;
	}

	// floats

	@BIF(type=Type.ARITHBIF)
	static public double fdiv(double v1, double v2) {
		if (v2 == 0.0)
			throw ERT.badarith("erlang", "/", v1, v2);

		return v1 / v2;
	}

	@BIF(type=Type.ARITHBIF)
	static public double fsub(double v1, double v2) {
		return v1 - v2;
	}

	@BIF(type=Type.ARITHBIF)
	static public double fadd(double v1, double v2) {
		return v1 + v2;
	}

	@BIF(type=Type.ARITHBIF)
	static public double fmul(double v1, double v2) {
		return v1 * v2;
	}

	// arithmetic

	@BIF(name="div") 
	static public ENumber div(EObject v1, int v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			return n1.div(v2);
		}
		throw ERT.badarg();
	}

	@BIF(name="div") 
	static public ENumber div(ENumber n1, int v2) {
		return n1.div(v2);
	}

	@BIF(name="-") 
	static public ENumber minus(EObject v1, int v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			return n1.minus(v2);
		}
		throw ERT.badarg("erlang", "-", v1, v2);
	}

	@BIF(name="-") 
	static public ENumber minus(int v1, int v2) {
		long res = (long)v1 - (long)v2;
		int intres = (int) res;
		if (res == intres)
			return new EInteger(intres);
		else
			return new EBig(BigInteger.valueOf(res));
	}

	@BIF(name="-") @ErlFun(name="-", export=true)
	static public ENumber minus(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.asNumber()) != null) {
				return n1.minus(n2);
			}
		}
		throw ERT.badarg((Throwable) null, "erlang", "-", v1, v2);
	}

	@BIF(name="-", type=Type.GUARD)
	static public ENumber subtract$guard(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.asNumber()) != null) {
				return n1.minus(n2);
			}
		}
		return null;
	}

	@BIF(name="/", type=Type.GUARD)
	static public ENumber divide$guard(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.asNumber()) != null) {
				if (n2.intValue() == 0) return null;
				return n1.divide(v2);
			}
		}
		return null;
	}

	@BIF(name="/", type=Type.GUARD)
	static public ENumber divide$guard(EObject v1, double d2) {
		ENumber n1;
		if (d2 != 0.0 && (n1 = v1.asNumber()) != null) {
			return n1.divide(d2);
		}
		return null;
	}


	
	@BIF(name = "+", type=Type.GUARD)
	static public ENumber plus$guard(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.asNumber()) != null) {
				return n1.add(n2);
			}
		}
		return null;
	}


	@BIF(name="+")
	static public ENumber plus(int v1, int v2) {
		return ENumber.valueFor((long)v1 + (long)v2);
	}
	
	@BIF(name = "+")
	static public ENumber plus(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.asNumber()) != null) {
				return n1.add(n2);
			}
		}
		throw ERT.badarg((Throwable) null, "erlang", "+", v1, v2);
	}

	@BIF(name = "+")
	static public ENumber plus(EObject v1, int i2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			return n1.add(i2);
		}
		throw ERT.badarg(v1, i2);
	}

	@BIF(name = "*")
	static public ENumber multiply(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.asNumber()) != null) {
				return n1.multiply(n2);
			}
		}
		throw ERT.badarg((Throwable) null, "erlang", "+", v1, v2);
	}

	@BIF(name = "*")
	static public ENumber multiply(int v1, int v2) {
		return ENumber.valueFor((long)v1 * (long)v2);
	}

	@BIF(name = "trunc") @ErlFun(export=true)
	static public ENumber trunc(EObject v1) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
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

	@BIF(name="round")
	static public double round(double d) {
		return Math.round(d);
	}

	@BIF(name="round")
	static public double round(EDouble d) {
		return Math.round(d.value);
	}

	@BIF @ErlFun(export=true)
	static public ENumber rem(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.asNumber()) != null) {
				return n1.rem(n2);
			}
		}
		throw ERT.badarg("erlang", "rem", v1, v2);
	}

	@BIF(name="rem", type=Type.GUARD)
	static public ENumber rem$guard(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.asNumber()) != null) {
				return n1.rem(n2);
			}
		}
		return null;
	}

	@BIF(name = "rem")
	static public ENumber rem(EObject v1, int v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			return n1.rem(v2);
		}
		throw ERT.badarg("erlang", "rem", v1, v2);
	}

	@BIF(name="abs", type=Type.GUARD)
	static public ENumber abs$guard(EObject v1) {
		ENumber num;
		if ((num = v1.asNumber()) != null) {
			return abs(num);
		}
		return null;
	}

	@BIF(name = "abs") @ErlFun(export=true)
	static public ENumber abs(EObject v1) {
		ENumber num;
		if ((num = v1.asNumber()) != null) {
			return abs(num);
		}
		throw ERT.badarg("erlang", "abs", v1);
	}

	@BIF(name = "abs")
	static public ENumber abs(ENumber v1) {
		return v1.asb();
	}

	@BIF(name = "now") @ErlFun(export=true)
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
	
	@BIF(name="is_eq",type=Type.GUARD)
	public static final EAtom is_eq$g(EObject a1, EObject a2) { return a1.equals(a2) ? ATOM_TRUE : null; }
	
	@BIF
	public static final EAtom is_eq(EObject a1, EObject a2) { return a1.equals(a2) ? ATOM_TRUE : ATOM_FALSE; }
	
}
