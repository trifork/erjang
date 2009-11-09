package org.erlang;

public class EBIF {

	@bif
	static public EPID self() {
		return null;
	}

	@bif
	static public EObject element(EInteger idx, EObject obj) {
		ETuple tup;
		if ((tup = obj.asTuple()) != null && tup.arity() >= idx.value) {
			return tup.nth(idx.value);
		}
		throw ERT.badarg("erlang", "element", idx, obj);
	}

	@bif
	static public EObject element(EInteger idx, ETuple tup) {
		if (tup.arity() >= idx.value) {
			return tup.nth(idx.value);
		}
		throw ERT.badarg("erlang", "element", idx, tup);
	}

	@bif
	static public EObject element(int idx, ETuple tup) {
		if (tup.arity() >= idx) {
			return tup.nth(idx);
		}
		throw ERT.badarg("erlang", "element", idx, tup);
	}

	@bif
	static public EObject element(int idx, EObject obj) {
		ETuple tup;
		if ((tup = obj.asTuple()) != null && tup.arity() >= idx) {
			return tup.nth(idx);
		}
		throw ERT.badarg("erlang", "element", idx, obj);
	}

	@bif
	static public EObject hd(ECons cell) {
		return cell.head();
	}

	@bif
	static public EObject hd(EObject cell) {
		ECons cons;
		if ((cons = cell.asCons()) != null) {
			return cons.head();
		}
		throw ERT.badarg("erlang", "hd", cell);
	}

	@bif
	static public int length(EObject list) {
		ESeq seq;
		if ((seq = list.asSeq()) != null) {
			return seq.length();
		}
		throw ERT.badarg("erlang", "length", list);
	}

	@bif
	static public int length(ESeq list) {
		return list.length();
	}

	@bif(name="length", guard=true)
	static public EInteger length$guard(EObject list) {
		ESeq seq;
		if ((seq = list.asSeq()) != null) {
			return new EInteger(seq.length());
		}
		return null;
	}

	@bif
	static public ENode node() {
		return null;
	}

	@bif
	static public ENode node(EObject name) {
		return null;
	}

	// process dict

	@bif
	static public ECons get() {
		return null;
	}

	@bif
	static public EObject get(EObject key) {
		return null;
	}

	// floats

	@bif
	static public double fdiv(double v1, double v2) {
		if (v2 == 0.0)
			throw ERT.badarith("erlang", "/", v1, v2);

		return v1 / v2;
	}

	static public double $minus$(double v1, double v2) {
		return v1 - v2;
	}

	static public double $plus$(double v1, double v2) {
		return v1 + v2;
	}

	@bif(name="fmul") 
	static public double $multiply$(double v1, double v2) {
		return v1 * v2;
	}

	// arithmetic

	@bif(name="-")
	static public ENumber $minus$(EObject v1, int v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			return n1.minus(v2);
		}
		throw ERT.badarg((Throwable) null, "erlang", "-", v1, v2);
	}

	@bif(name="-")
	static public ENumber $minus$(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.asNumber()) != null) {
				return n1.minus(n2);
			}
		}
		throw ERT.badarg((Throwable) null, "erlang", "-", v1, v2);
	}

	@bif(name="-", guard=true)
	static public ENumber $minus$$guard(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.asNumber()) != null) {
				return n1.minus(n2);
			}
		}
		return null;
	}

	@bif(name = "+")
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

	@bif(name = "*")
	static public ENumber $multiply$(EObject v1, EObject v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			ENumber n2;
			if ((n2 = v2.asNumber()) != null) {
				return n1.multiply(n2);
			}
		}
		throw ERT.badarg((Throwable) null, "erlang", "+", v1, v2);
	}

	@bif(name = "trunc")
	static public ENumber trunc(EObject v1) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			return n1.trunc();
		}
		throw ERT.badarg((Throwable) null, "erlang", "trunc", v1);
	}

	@bif(name = "trunc")
	static public double trunc(double d) {
		return Math.floor(d);
	}

	static public double round(double d) {
		return Math.round(d);
	}

	@bif(name = "rem")
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

	@bif(name="trunc", guard=true)
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

	@bif(name = "rem")
	static public ENumber rem(EObject v1, int v2) {
		ENumber n1;
		if ((n1 = v1.asNumber()) != null) {
			return n1.rem(v2);
		}
		throw ERT.badarg("erlang", "rem", v1, v2);
	}

	@bif(name="abs",guard=true)
	static public ENumber abs$guard(EObject v1) {
		ENumber num;
		if ((num = v1.asNumber()) != null) {
			return abs(num);
		}
		return null;
	}

	@bif(name = "abs")
	static public ENumber abs(EObject v1) {
		ENumber num;
		if ((num = v1.asNumber()) != null) {
			return abs(num);
		}
		throw ERT.badarg("erlang", "abs", v1);
	}

	@bif(name = "abs")
	static public ENumber abs(ENumber v1) {
		return v1.asb();
	}

	@bif(name = "now")
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
	
}
