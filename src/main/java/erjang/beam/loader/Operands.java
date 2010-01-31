package erjang.beam.loader;

import erjang.EObject;
import erjang.EAtom;

import java.util.ArrayList;

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

    static class Int extends Literal {
	public final int value;
	public Int(int value) {this.value=value;}
    }

    static final Nil Nil = new Nil();
    static class Nil extends Literal {
	private Nil() {}
    }

    static class List extends Literal {
	Operand[] list;
	public List(Operand[] list) {this.list=list;}

	@Override
	public List asList() {return this;}
    }

    static class Atom extends Literal {
	private int idx;
	public EAtom value;
	public Atom(int idx) {this.idx=idx;}
	public Atom(EAtom value) {this.value=value;}

	@Override
	public Atom asAtom() {return this;}
    }

    static class TableLiteral extends Literal {
	private int idx;
	private EObject value;
	public TableLiteral(int idx) {this.idx=idx;}
	public TableLiteral(EObject value) {this.value=value;}
    }

    static class Label extends Operand {
	public final int nr;
	public Label(int nr) {this.nr=nr;}
	@Override
	public Label asLabel() {return this;}
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
    }



}
