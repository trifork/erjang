/** -*- tab-width: 4 -*-
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
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

package erjang.beam.repr;

import erjang.EObject;
import erjang.EAtom;
import erjang.ESmall;
import erjang.EBig;
import erjang.EDouble;
import erjang.ETuple;
import erjang.ESeq;
import erjang.EString;
import erjang.EBitString;
import erjang.ERT;

import java.util.ArrayList;
import java.math.BigInteger;

import static erjang.beam.CodeAtoms.*;

public class Operands {

    public static abstract class Operand {
		public SourceOperand asSource() {
			throw new IllegalArgumentException("Not a source: "+this);
		}
		public DestinationOperand asDestination() {
			throw new IllegalArgumentException("Not a destination: "+this);
		}
		public Label asLabel() {
			throw new IllegalArgumentException("Not a label: "+this);
		}
		public Literal asLiteral() {
			throw new IllegalArgumentException("Not a literal: "+this);
		}
		public Atom asAtom() {
			throw new IllegalArgumentException("Not an atom: "+this);
		}
		public SelectList asSelectList() {
			throw new IllegalArgumentException("Not a select list: "+this);
		}
		public AllocList asAllocList() {
			throw new IllegalArgumentException("Not a alloc list: "+this);
		}
		public YReg asYReg() {
			throw new IllegalArgumentException("Not a Y register: "+this);
		}

		public abstract EObject toSymbolic();
    }

    public static abstract class SourceOperand extends Operand {
		@Override
		public SourceOperand asSource() {return this;}
    }
    public static abstract class DestinationOperand extends SourceOperand {
		@Override
		public DestinationOperand asDestination() {return this;}
    }
    public static abstract class Literal extends SourceOperand {
		@Override
		public Literal asLiteral() {return this;}
    }

    public static Literal makeInt(byte[] d) {
		BigInteger tmp = new BigInteger(d);
		return (d.length<=4)
			? new Int(tmp.intValue())
			: new BigInt(tmp);
    }

    /** CodeInt is present in jump-table-by-arity (select_arity). */
    public static class CodeInt extends Operand {
		public final int value;
		public CodeInt(int value) {this.value=value;}
		public EObject toSymbolic() {
			return new ESmall(value);
		}
    }

    public static class Int extends Literal {
		public final int value;
		public Int(int value) {this.value=value;}
		public EObject toSymbolic() {
			return ETuple.make(INTEGER_ATOM, new ESmall(value));
		}
    }

    public static class BigInt extends Literal {
		public final BigInteger value;
		public BigInt(BigInteger value) {this.value=value;}
		public EObject toSymbolic() {
			return ETuple.make(INTEGER_ATOM, new EBig(value));
		}
    }

    public static class Float extends Literal {
		public final double value;
		public Float(double value) {this.value=value;}
		public EObject toSymbolic() {
			return ETuple.make(FLOAT_ATOM, new EDouble(value));
		}
    }

    public static final Nil Nil = new Nil();
    public static class Nil extends Literal {
		private Nil() {}
		public EObject toSymbolic() {return NIL_ATOM;}
    }

    public static class SelectList extends Operand {
		Operand[] list;
		public SelectList(Operand[] list) {this.list=list;}

		@Override
		public SelectList asSelectList() {return this;}
		public EObject toSymbolic() {
			EObject[] elems = new EObject[list.length];
			for (int i=0; i<list.length; i++) {
				elems[i] = list[i].toSymbolic();
			}
			return ETuple.make(LIST_ATOM, ESeq.fromArray(elems));
		}
    }

    public static class AllocList extends Operand {
		static final int WORDS  = 0;
		static final int FLOATS = 1;

		int[] list;
		public AllocList(int[] list) {this.list=list;}
		public AllocList(int words) {
			list=new int[2];
			list[0] = WORDS;
			list[1] = words;
		}

		@Override
		public AllocList asAllocList() {return this;}
		public EObject toSymbolic() {
			int len = list.length/2;

			if (len==1 && list[0] == WORDS)
				return new ESmall(list[1]); // Just words.

			EObject[] elems = new EObject[len];
			for (int i=0; i<len; i++) {
				elems[i] = ETuple.make(kindToSymbolic(list[2*i]),
									   new ESmall(list[2*i+1]));
			}
			return ETuple.make(ALLOC_ATOM, ESeq.fromArray(elems));
		}
		protected EAtom kindToSymbolic(int kind) {
			switch (kind) {
			case WORDS:  return WORDS_ATOM;
			case FLOATS: return FLOATS_ATOM;
			default:
				throw new IllegalArgumentException("Unknown alloc kind: "+kind);
			} // switch
		}
    }


    public static class Atom extends Literal {
		private EAtom value;
		public Atom(EAtom value) {this.value=value;}

		@Override
		public Atom asAtom() {return this;}
		public EAtom getEAtom() {return value;}
		public EObject toSymbolic() {
			return ETuple.make(ATOM_ATOM, value);
		}
    }

    public static class BitString extends Literal {
		protected final EBitString value;
		protected final int bits;
		public BitString(int start, int bits, CodeTables ct) {
			this.bits = bits;
			value = ct.bitstring(start,bits);
		}
		public int bitLength() {return bits;}

		@Override
		public EObject toSymbolic() {
			return value; // OBS: beam_disasm round up to nearest byte.
		}
    }

    public static class ByteString extends Literal {
		protected final EString value;
		public ByteString(int start, int bytes, CodeTables ct) {
			value = ct.string(start,bytes);
	}
		public int byteLength() {return value.length();}

		@Override
		public EObject toSymbolic() {
			return ETuple.make(STRING_ATOM, value);
		}
    }

    public static class TableLiteral extends Literal {
		private EObject value;
		public TableLiteral(EObject value) {this.value=value;}
		public EObject toSymbolic() {
			return ETuple.make(LITERAL_ATOM, value);
		}
    }

    public static class Label extends Operand {
		public final int nr;
		public Label(int nr) {this.nr=nr;}
		@Override
		public Label asLabel() {return this;}

		public EObject toSymbolic() {
			return ETuple.make(F_ATOM, new ESmall(nr));
		}
    }

    public static class XReg extends DestinationOperand {
		public final int nr;
		public XReg(int nr) {this.nr=nr;}

		private static ArrayList<XReg> cache = new ArrayList();
		public static XReg get(int nr) {
			while (cache.size() <= nr) {
				cache.add(new XReg(cache.size()));
			}
			return cache.get(nr);
		}

		public EObject toSymbolic() {
			return ETuple.make(X_ATOM, new ESmall(nr));
		}
    }

    public static class YReg extends DestinationOperand {
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

		public EObject toSymbolic() {
			return ETuple.make(Y_ATOM, new ESmall(nr));
		}
    }

    public static class FReg extends DestinationOperand {
		public final int nr;
		public FReg(int nr) {this.nr=nr;}

		private static ArrayList<FReg> cache = new ArrayList();
		public static FReg get(int nr) {
			while (cache.size() <= nr) {
				cache.add(new FReg(cache.size()));
			}
			return cache.get(nr);
		}

		public EObject toSymbolic() {
			return ETuple.make(FR_ATOM, new ESmall(nr));
		}
    }

}
