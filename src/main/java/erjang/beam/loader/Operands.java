package erjang.beam.loader;

import erjang.EObject;
import erjang.EAtom;
import erjang.ESmall;
import erjang.EBig;
import erjang.ETuple;
import erjang.ESeq;
import erjang.ERT;

import java.util.ArrayList;
import java.math.BigInteger;

import static erjang.beam.CodeAtoms.*;

public class Operands {

    static abstract class Operand {
	public SourceOperand asSource() {
	    throw new IllegalArgumentException("Not a source: "+this);
	}
	public DestinationOperand asDestination() {
	    throw new IllegalArgumentException("Not a destination: "+this);
	}
	public Label asLabel() {
	    throw new IllegalArgumentException("Not a label: "+this);
	}
	public Atom asAtom() {
	    throw new IllegalArgumentException("Not an atom: "+this);
	}
	public List asList() {
	    throw new IllegalArgumentException("Not a list: "+this);
	}
	public YReg asYReg() {
	    throw new IllegalArgumentException("Not a Y register: "+this);
	}

	public abstract EObject toSymbolic(CodeTables ct);
    }
    static abstract class SourceOperand extends Operand {
	@Override
	public SourceOperand asSource() {return this;}
    }
    static abstract class DestinationOperand extends SourceOperand {
	@Override
	public DestinationOperand asDestination() {return this;}
    }
    static abstract class Literal extends SourceOperand {}

    static Literal makeInt(byte[] d) {
	BigInteger tmp = new BigInteger(d);
	return (d.length<=4)
	    ? new Int(tmp.intValue())
	    : new BigInt(tmp);
    }
    static class Int extends Literal {
	public final int value;
	public Int(int value) {this.value=value;}
	public EObject toSymbolic(CodeTables ct) {return new ESmall(value);}
    }

    static class BigInt extends Literal {
	public final BigInteger value;
	public BigInt(BigInteger value) {this.value=value;}
	public EObject toSymbolic(CodeTables ct) {return new EBig(value);}
    }

    static final Nil Nil = new Nil();
    static class Nil extends Literal {
	private Nil() {}
	public EObject toSymbolic(CodeTables ct) {return ERT.NIL;}
    }

    static class List extends Literal {
	Operand[] list;
	public List(Operand[] list) {this.list=list;}

	@Override
	public List asList() {return this;}
	public EObject toSymbolic(CodeTables ct) {
	    EObject[] elems = new EObject[list.length];
	    for (int i=0; i<list.length; i++) {
		elems[i] = list[i].toSymbolic(ct);
	    }
	    return ESeq.fromArray(elems);
	}
    }

    static class Atom extends Literal {
	private int idx;
	public Atom(int idx) {this.idx=idx;}

	@Override
	public Atom asAtom() {return this;}
	public EObject toSymbolic(CodeTables ct) {return ct.atom(idx);}
    }

    static class TableLiteral extends Literal {
	private int idx;
	public TableLiteral(int idx) {this.idx=idx;}
	public EObject toSymbolic(CodeTables ct) {return ct.literal(idx);}
    }

    static class Label extends Operand {
	public final int nr;
	public Label(int nr) {this.nr=nr;}
	@Override
	public Label asLabel() {return this;}

	public EObject toSymbolic(CodeTables ct) {
	    return ETuple.make(F_ATOM, new ESmall(nr));
	}
    }

    static class XReg extends DestinationOperand {
	public final int nr;
	public XReg(int nr) {this.nr=nr;}

	private static ArrayList<XReg> cache = new ArrayList();
	public static XReg get(int nr) {
	    while (cache.size() <= nr) {
		cache.add(new XReg(cache.size()));
	    }
	    return cache.get(nr);
	}

	public EObject toSymbolic(CodeTables ct) {
	    return ETuple.make(X_ATOM, new ESmall(nr));
	}
    }

    static class YReg extends DestinationOperand {
	public final int nr;
	public YReg(int nr) {this.nr=nr;}

	@Override
	public YReg asYReg() {return this;}

	private static ArrayList<YReg> cache = new ArrayList();
	public static YReg get(int nr) {
	    while (cache.size() <= nr) {
		cache.add(new YReg(cache.size()));
	    }
	    return cache.get(nr);
	}

	public EObject toSymbolic(CodeTables ct) {
	    return ETuple.make(Y_ATOM, new ESmall(nr));
	}
    }

}
