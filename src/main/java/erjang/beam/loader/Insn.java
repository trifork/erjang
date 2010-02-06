package erjang.beam.loader;

import erjang.beam.BeamInstruction;
import erjang.beam.BeamOpcode;

import erjang.EObject;
import erjang.EAtom;
import erjang.ETuple;
import erjang.ESeq;
import erjang.EList;
import erjang.ESmall;
import erjang.EBinary;
import erjang.ERT;

import static erjang.beam.loader.Operands.*;
import static erjang.beam.CodeAtoms.*;

public class Insn implements BeamInstruction {
	protected final BeamOpcode opcode;
	public Insn(BeamOpcode opcode) {this.opcode = opcode;}
	public BeamOpcode opcode() {return opcode;}

	public String toString() {return opcode+"(...)";}
	public EObject toSymbolic(CodeTables ct) {
	    return opcode.symbol;
	}


	private static EObject NOFAIL_REPR = ETuple.make(F_ATOM, new ESmall(0));
	private static EObject START_REPR = ETuple.make(ATOM_ATOM, START_ATOM);
	static EObject toSymbolic(Label label, CodeTables ct) {
		return label==null? NOFAIL_REPR : label.toSymbolic(ct);
	}
	static EObject toSymbolic_nf(Label label, CodeTables ct) {
		return label==null? NOFAIL_ATOM : label.toSymbolic(ct);
	}

	static EObject bsFieldFlagsToSymbolic(int flags) {
		return ETuple.make(FIELD_FLAGS_ATOM, new ESmall(flags));
	}
	/*============================================================
	 *                     Instruction formats
	 * Class names encode the format - the parameter types - as follows:
	 * I - Integer
	 * A - Atom
	 * Bi - Bitstring
	 * By - Bytestring
	 * S - Source operand: register or literal
	 * D - Destination operand: register
	 * L - Label
	 * K - Optional label (fail label)
	 * E - External function
	 * F - Anonymous function object
	 * W - Allocation size
	 */

	public static class I extends Insn { // E.g. 'deallocate'
		final int i1;
		public I(BeamOpcode opcode, int i1) {
			super(opcode);
			this.i1 = i1;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol, new ESmall(i1));
		}
	}

	public static class S extends Insn { // E.g. 'put'
		final SourceOperand src;
		public S(BeamOpcode opcode, SourceOperand src) {
			super(opcode);
			this.src = src;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol, src.toSymbolic(ct));
		}
	}

	public static class D extends Insn { // E.g. 'init'
		final DestinationOperand dest;
		public D(BeamOpcode opcode, DestinationOperand dest) {
			super(opcode);
			this.dest = dest;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol, dest.toSymbolic(ct));
		}
	}

	public static class L extends Insn { // E.g. 'jump'
		final Label label;
		public L(BeamOpcode opcode, Label label) {
			super(opcode);
			this.label = label;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol, label.toSymbolic(ct));
		}
	}

	public static class F extends Insn { // E.g. 'make_fun2'
		final int anon_fun_ref;
		public F(BeamOpcode opcode, int anon_fun_ref) {
			super(opcode);
			this.anon_fun_ref = anon_fun_ref;
		}
		public ETuple toSymbolic(CodeTables ct) {
			AnonFun f = ct.anonFun(anon_fun_ref);
			return ETuple.make(opcode.symbol,
					   f.toSymbolic(ct),
					   new ESmall(anon_fun_ref),
					   new ESmall(f.uniq),
					   new ESmall(f.free_vars));
		}
	}

	public static class Y extends Insn { // E.g. 'catch'
		final YReg y;
		public Y(BeamOpcode opcode, YReg y) {
			super(opcode);
			this.y = y;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol, y.toSymbolic(ct));
		}
	}

	public static class By extends Insn { // E.g. 'bs_put_string'
		final ByteString bin;
		public By(BeamOpcode opcode, ByteString bin) {
			super(opcode);
			this.bin = bin;
		}

		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   new ESmall(bin.byteLength()),
					   bin.toSymbolic(ct));
		}
	}

	public static class SI extends Insn { // E.g. 'bs_save2'
		final SourceOperand src;
		final int i2;
		final boolean is_saverestore;
		public SI(BeamOpcode opcode, SourceOperand src, int i2, boolean is_saverestore) {
			super(opcode);
			this.src = src;
			this.i2 = i2;
			this.is_saverestore = is_saverestore;
		}
		public ETuple toSymbolic(CodeTables ct) {
			if (is_saverestore)
				return ETuple.make(opcode.symbol,
						   src.toSymbolic(ct),
						   (i2==-1 ? START_REPR : new ESmall(i2)));
			else
				return ETuple.make(opcode.symbol,
						   src.toSymbolic(ct),
						   new ESmall(i2));
		}
	}

	public static class SD extends Insn { // E.g. 'move'
		final SourceOperand src;
		final DestinationOperand dest;
		public SD(BeamOpcode opcode, SourceOperand src, DestinationOperand dest) {
			super(opcode);
			this.src = src;
			this.dest = dest;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   src.toSymbolic(ct),
					   dest.toSymbolic(ct));
		}
	}

	public static class LD extends Insn { // E.g. 'loop_rec'
		final Label label;
		final DestinationOperand dest;
		public LD(BeamOpcode opcode, Label label, DestinationOperand dest) {
			super(opcode);
			this.label = label;
			this.dest = dest;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   label.toSymbolic(ct),
					   dest.toSymbolic(ct));
		}
	}

	public static class YL extends Insn { // E.g. 'catch'
		final YReg y;
		final Label label;
		public YL(BeamOpcode opcode, YReg y, Label label) {
			super(opcode);
			this.y = y;
			this.label = label;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   y.toSymbolic(ct),
					   label.toSymbolic(ct));
		}
	}

	public static class LS extends Insn { // E.g. 'is_nil'
		final Label label;
		final SourceOperand src;
		final boolean is_test;
		public LS(BeamOpcode opcode, Label label, SourceOperand src, boolean is_test) {
			super(opcode);
			this.label = label;
			this.src = src;
			this.is_test = is_test;
		}
		public ETuple toSymbolic(CodeTables ct) {
			if (is_test)
				return ETuple.make(TEST_ATOM,
						   opcode.symbol,
						   label.toSymbolic(ct),
						   EList.make(src.toSymbolic(ct)));
			else
				return ETuple.make(opcode.symbol,
						   label.toSymbolic(ct),
						   src.toSymbolic(ct));
		}
	}

	public static class II extends Insn { // E.g. 'allocate'
		final int i1, i2;
		public II(BeamOpcode opcode, int i1, int i2) {
			super(opcode);
			this.i1 = i1;
			this.i2 = i2;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   new ESmall(i1),
					   new ESmall(i2));
		}
	}

	public static class IL extends Insn { // E.g. 'call'
		final int i1;
		final Label label;
		final boolean is_call;
		public IL(BeamOpcode opcode, int i1, Label label,  boolean is_call) {
			super(opcode);
			this.i1 = i1;
			this.label = label;
			this.is_call = is_call;
		}
		public ETuple toSymbolic(CodeTables ct) {
			if (is_call)
				return ETuple.make(opcode.symbol,
						   new ESmall(i1),
						   ct.functionAtLabel(label.nr).toSymbolic());
			else
				return ETuple.make(opcode.symbol,
						   new ESmall(i1),
						   label.toSymbolic(ct));
		}
	}

	public static class SS extends Insn { // E.g. 'raise'
		final SourceOperand src1, src2;
		public SS(BeamOpcode opcode, SourceOperand src1, SourceOperand src2) {
			super(opcode);
			this.src1 = src1;
			this.src2 = src2;
		}
		public ETuple toSymbolic(CodeTables ct) {
			if (opcode == BeamOpcode.raise)
				return ETuple.make(opcode.symbol,
						   new Label(0).toSymbolic(ct),
						   EList.make(src1.toSymbolic(ct),
							      src2.toSymbolic(ct)),
						   XReg.get(0).toSymbolic(ct));
			else
				return ETuple.make(opcode.symbol,
						   src1.toSymbolic(ct),
						   src2.toSymbolic(ct));
		}
	}

	public static class AAI extends Insn { // E.g. 'func_info'
		final Atom a1,a2;
		final int i1;
		public AAI(BeamOpcode opcode, Atom a1, Atom a2, int i1) {
			super(opcode);
			this.a1 = a1;
			this.a2 = a2;
			this.i1 = i1;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   a1.toSymbolic(ct),
					   a2.toSymbolic(ct),
					   new ESmall(i1));
		}
	}

	public static class IE extends Insn { // E.g. 'call_ext'
		final int i1;
		final int ext_fun_ref;
		public IE(BeamOpcode opcode, int i1, int ext_fun_ref) {
			super(opcode);
			this.i1 = i1;
			this.ext_fun_ref = ext_fun_ref;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   new ESmall(i1),
					   ct.extFun(ext_fun_ref).toSymbolic(ct));
		}
	}

	public static class ID extends Insn { // E.g. 'put_tuple'
		final int i1;
		final DestinationOperand dest;
		public ID(BeamOpcode opcode, int i1, DestinationOperand dest) {
			super(opcode);
			this.i1 = i1;
			this.dest = dest;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   new ESmall(i1),
					   dest.toSymbolic(ct));
		}
	}

	public static class WI extends Insn { // E.g. 'test_heap'
		final AllocList alist;
		final int i2;
		public WI(BeamOpcode opcode, AllocList alist, int i2) {
			super(opcode);
			this.alist = alist;
			this.i2 = i2;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   alist.toSymbolic(ct),
					   new ESmall(i2));
		}
	}

	public static class IWI extends Insn { // E.g. 'allocate_heap'
		final int i1, i3;
		final AllocList al;
		public IWI(BeamOpcode opcode, int i1, AllocList al, int i3) {
			super(opcode);
			this.i1 = i1;
			this.al = al;
			this.i3 = i3;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   new ESmall(i1),
					   al.toSymbolic(ct),
					   new ESmall(i3));
		}
	}

	public static class LED extends Insn { // E.g. 'bif0'
		final Label label;
		final int ext_fun_ref;
		final DestinationOperand dest;
		final boolean is_bif;
		public LED(BeamOpcode opcode, Label label, int ext_fun_ref, DestinationOperand dest, boolean is_bif) {
			super(opcode);
			this.label = label;
			this.ext_fun_ref = ext_fun_ref;
			this.dest = dest;
			this.is_bif = is_bif;
		}
		public ETuple toSymbolic(CodeTables ct) {
			if (is_bif)
				return ETuple.make(BIF_ATOM,
						   ct.atom(ct.extFun(ext_fun_ref).fun),
						   toSymbolic_nf(label, ct),
						   ERT.NIL,
						   dest.toSymbolic(ct));
			else
				return ETuple.make(opcode.symbol,
						   toSymbolic(label, ct),
						   ct.extFun(ext_fun_ref).toSymbolic(ct),
						   dest.toSymbolic(ct));
		}
	}

	public static class LSI extends Insn { // E.g. 'test_arity'
		final Label label;
		final SourceOperand src;
		final int i1;
		final boolean is_test;
		public LSI(BeamOpcode opcode, Label label, SourceOperand src, int i1, boolean is_test) {
			super(opcode);
			this.label = label;
			this.src = src;
			this.i1 = i1;
			this.is_test = is_test;
		}
		public ETuple toSymbolic(CodeTables ct) {
			if (is_test)
				return ETuple.make(TEST_ATOM,
						   opcode.symbol,
						   label.toSymbolic(ct),
						   EList.make(src.toSymbolic(ct),
							      new ESmall(i1)));
			else
				return ETuple.make(opcode.symbol,
						   label.toSymbolic(ct),
						   src.toSymbolic(ct),
						   new ESmall(i1));
		}
	}

	public static class SID extends Insn { // E.g. 'get_tuple_element'
		final SourceOperand src;
		final int i1;
		final DestinationOperand dest;
		public SID(BeamOpcode opcode, SourceOperand src, int i1, DestinationOperand dest) {
			super(opcode);
			this.src = src;
			this.i1 = i1;
			this.dest = dest;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   src.toSymbolic(ct),
					   new ESmall(i1),
					   dest.toSymbolic(ct));
		}
	}

	public static class LSS extends Insn { // E.g. 'is_eq_exact'
		final Label label;
		final SourceOperand src1;
		final SourceOperand src2;
		final boolean is_test;
		public LSS(BeamOpcode opcode, Label label, SourceOperand src1, SourceOperand src2, boolean is_test) {
			super(opcode);
			this.label = label;
			this.src1 = src1;
			this.src2 = src2;
			this.is_test = is_test;
		}
		public ETuple toSymbolic(CodeTables ct) {
			if (is_test)
				return ETuple.make(TEST_ATOM,
						   opcode.symbol,
						   label.toSymbolic(ct),
						   EList.make(src1.toSymbolic(ct),
							      src2.toSymbolic(ct)));
			else
				return ETuple.make(opcode.symbol,
						   label.toSymbolic(ct),
						   src1.toSymbolic(ct),
						   src2.toSymbolic(ct));
		}
	}

	public static class LSD extends Insn { // E.g. 'bs_utf8_size'
		final Label label;
		final SourceOperand src;
		final DestinationOperand dest;
		public LSD(BeamOpcode opcode, Label label, SourceOperand src, DestinationOperand dest) {
			super(opcode);
			this.label = label;
			this.src = src;
			this.dest = dest;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   label.toSymbolic(ct),
					   src.toSymbolic(ct),
					   dest.toSymbolic(ct));
		}
	}

	public static class SDD extends Insn { // E.g. 'get_list'
		final SourceOperand src;
		final DestinationOperand dest1;
		final DestinationOperand dest2;
	public SDD(BeamOpcode opcode, SourceOperand src, DestinationOperand dest1, DestinationOperand dest2) {
			super(opcode);
			this.src = src;
			this.dest1 = dest1;
			this.dest2 = dest2;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   src.toSymbolic(ct),
					   dest1.toSymbolic(ct),
					   dest2.toSymbolic(ct));
		}
	}

	public static class SSD extends Insn { // E.g. 'move'
		final SourceOperand src1;
		final SourceOperand src2;
		final DestinationOperand dest;
		public SSD(BeamOpcode opcode, SourceOperand src1, SourceOperand src2, DestinationOperand dest) {
			super(opcode);
			this.src1 = src1;
			this.src2 = src2;
			this.dest = dest;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   src1.toSymbolic(ct),
					   src2.toSymbolic(ct),
					   dest.toSymbolic(ct));
		}
	}

	public static class SDI extends Insn { // E.g. 'set_tuple_element'
		final SourceOperand src;
		final DestinationOperand dest;
		int i;
		public SDI(BeamOpcode opcode, SourceOperand src, DestinationOperand dest, int i) {
			super(opcode);
			this.src = src;
			this.dest = dest;
			this.i = i;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   src.toSymbolic(ct),
					   dest.toSymbolic(ct),
					   new ESmall(i));
		}
	}


	public static class ILI extends Insn { // E.g. 'call'
		final int i1, i3;
		final Label label;
		final boolean is_call;
		public ILI(BeamOpcode opcode, int i1, Label label, int i3, boolean is_call) {
			super(opcode);
			this.i1 = i1;
			this.label = label;
			this.i3 = i3;
			this.is_call = is_call;
		}

		public ETuple toSymbolic(CodeTables ct) {
			if (is_call)
				return ETuple.make(opcode.symbol,
						   new ESmall(i1),
						   ct.functionAtLabel(label.nr).toSymbolic(),
						   new ESmall(i3));
			else
				return ETuple.make(opcode.symbol,
						   new ESmall(i1),
						   label.toSymbolic(ct),
						   new ESmall(i3));
		}
	}

	public static class IEI extends Insn { // E.g. 'call'
		final int i1, i3;
		final int ext_fun_ref;
		public IEI(BeamOpcode opcode, int i1, int ext_fun_ref, int i3) {
			super(opcode);
			this.i1 = i1;
			this.ext_fun_ref = ext_fun_ref;
			this.i3 = i3;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   new ESmall(i1),
					   ct.extFun(ext_fun_ref).toSymbolic(ct),
					   new ESmall(i3));
		}
	}

	public static class ByD extends Insn { // E.g. 'put_string'
		final ByteString bin;
		final DestinationOperand dest;
		public ByD(BeamOpcode opcode, ByteString bin, DestinationOperand dest) {
			super(opcode);
			this.bin = bin;
			this.dest = dest;
		}

		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   new ESmall(bin.byteLength()),
					   bin.toSymbolic(ct),
					   dest.toSymbolic(ct));
		}
	}

	public static class LSSD extends Insn { // E.g. 'fmul'
		final Label label;
		final SourceOperand src1;
		final SourceOperand src2;
		final DestinationOperand dest;
		final boolean is_arithfbif;
		public LSSD(BeamOpcode opcode, Label label,
			    SourceOperand src1, SourceOperand src2,
			    DestinationOperand dest,
			    boolean is_arithfbif)
		{
			super(opcode);
			this.label = label;
			this.src1 = src1;
			this.src2 = src2;
			this.dest = dest;
			this.is_arithfbif = is_arithfbif;
		}
		public ETuple toSymbolic(CodeTables ct) {
			if (is_arithfbif)
				return ETuple.make(ARITHFBIF_ATOM,
						   opcode.symbol,
						   label.toSymbolic(ct),
						   EList.make(src1.toSymbolic(ct),
							      src2.toSymbolic(ct)),
					   dest.toSymbolic(ct));
			else
				return ETuple.make(opcode.symbol,
						   label.toSymbolic(ct),
						   src1.toSymbolic(ct),
						   src2.toSymbolic(ct),
						   dest.toSymbolic(ct));
		}
	}

	public static class LSSID extends Insn { // E.g. 'bs_add'
		final Label label;
		final SourceOperand src1;
		final SourceOperand src2;
		final int i3;
		final DestinationOperand dest;
		public LSSID(BeamOpcode opcode, Label label,
			     SourceOperand src1, SourceOperand src2,
			     int i3, DestinationOperand dest)
		{
			super(opcode);
			this.label = label;
			this.src1 = src1;
			this.src2 = src2;
			this.i3 = i3;
			this.dest = dest;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   label.toSymbolic(ct),
					   EList.make(src1.toSymbolic(ct),
						      src2.toSymbolic(ct),
						      new ESmall(i3)),
					   dest.toSymbolic(ct));
		}
	}

	public static class LSBi extends Insn { // E.g. 'bs_match_string'
		final Label label;
		final SourceOperand src;
		final BitString bin;
		public LSBi(BeamOpcode opcode, Label label, SourceOperand src, BitString bin) {
			super(opcode);
			this.label = label;
			this.src = src;
			this.bin = bin;
		}

		public ETuple toSymbolic(CodeTables ct) {
			if (opcode == BeamOpcode.bs_match_string)
				return ETuple.make(TEST_ATOM,
						   opcode.symbol,
						   label.toSymbolic(ct),
						   EList.make(src.toSymbolic(ct),
							      new ESmall(bin.bitLength()),
							      bin.toSymbolic(ct)));
			else
				throw new erjang.NotImplemented();
		}
	}

	public static class LSII extends Insn { // E.g. 'bs_skip_utf8'
		final Label label;
		final SourceOperand src;
		final int i3, i4;
		public LSII(BeamOpcode opcode, Label label, SourceOperand src, int i3, int i4) {
			super(opcode);
			this.label = label;
			this.src = src;
			this.i3 = i3;
			this.i4 = i4;
		}

		public ETuple toSymbolic(CodeTables ct) {
			if (opcode == BeamOpcode.bs_skip_utf8 ||
			    opcode == BeamOpcode.bs_skip_utf16 ||
			    opcode == BeamOpcode.bs_skip_utf32)
				return ETuple.make(TEST_ATOM,
						   opcode.symbol,
						   label.toSymbolic(ct),
						   EList.make(src.toSymbolic(ct),
							      new ESmall(i3),
							      bsFieldFlagsToSymbolic(i4)));
			else
				throw new erjang.NotImplemented();
		}
	}


	public static class LSIIS extends Insn { // E.g. 'bs_put_integer'
		final Label label;
		final SourceOperand src2, src5;
		final int i3, i4;
		final boolean is_bs;
		public LSIIS(BeamOpcode opcode, Label label, SourceOperand src2, int i3, int i4, SourceOperand src5, boolean is_bs) {
			super(opcode);
			this.label = label;
			this.src2 = src2;
			this.i3 = i3;
			this.i4 = i4;
			this.src5 = src5;
			this.is_bs = is_bs;
		}

		public ETuple toSymbolic(CodeTables ct) {
			if (is_bs)
				return ETuple.make(opcode.symbol,
						   label.toSymbolic(ct),
						   src2.toSymbolic(ct),
						   new ESmall(i3),
						   bsFieldFlagsToSymbolic(i4),
						   src5.toSymbolic(ct));
			else
				throw new erjang.NotImplemented();
		}
	}

	public static class LIS extends Insn { // E.g. 'bs_put_utf8'
		final Label label;
		final int i2;
		final SourceOperand src;
		final boolean is_bsput;
		public LIS(BeamOpcode opcode, Label label, int i2, SourceOperand src, boolean is_bsput) {
			super(opcode);
			this.label = label;
			this.i2 = i2;
			this.src = src;
			this.is_bsput = is_bsput;
		}

		public ETuple toSymbolic(CodeTables ct) {
			if (is_bsput)
				return ETuple.make(opcode.symbol,
						   label.toSymbolic(ct),
						   bsFieldFlagsToSymbolic(i2),
						   src.toSymbolic(ct));
			else
				throw new erjang.NotImplemented();
		}
	}


	public static class LSIID extends Insn { // E.g. 'bs_start_match2'
		final Label label;
		final SourceOperand src;
		final int i3, i4;
		final DestinationOperand dest;
		final boolean is_test;
		final boolean i4_is_bsflags;
		public LSIID(BeamOpcode opcode, Label label, SourceOperand src, int i3, int i4, DestinationOperand dest, boolean is_test, boolean i4_is_bsflags) {
			super(opcode);
			this.label = label;
			this.src = src;
			this.i3 = i3;
			this.i4 = i4;
			this.dest = dest;
			this.is_test = is_test;
			this.i4_is_bsflags = i4_is_bsflags;
		}

		public ETuple toSymbolic(CodeTables ct) {
			if (is_test)
				return ETuple.make(TEST_ATOM,
						   opcode.symbol,
						   label.toSymbolic(ct),
						   EList.make(src.toSymbolic(ct),
							      new ESmall(i3),
							      i4_is_bsflags
							      ? bsFieldFlagsToSymbolic(i4)
							      : new ESmall(i4),
							      dest.toSymbolic(ct)));
			else
				throw new erjang.NotImplemented();
		}
	}

	public static class LIIIID extends Insn { // E.g. 'bs_init2'
		final Label label;
		final int i2, i3, i4,i5;
		final DestinationOperand dest;
		public LIIIID(BeamOpcode opcode, Label label,
			      int i2, int i3, int i4, int i5,
			      DestinationOperand dest)
		{
			super(opcode);
			this.label = label;
			this.i2 = i2;
			this.i3 = i3;
			this.i4 = i4;
			this.i5 = i5;
			this.dest = dest;
		}

		public ETuple toSymbolic(CodeTables ct) {
			if (opcode == BeamOpcode.bs_init2)
				return ETuple.make(opcode.symbol,
						   toSymbolic(label,ct),
						   new ESmall(i2),
						   new ESmall(i3),
						   new ESmall(i4),
						   bsFieldFlagsToSymbolic(i5),
						   dest.toSymbolic(ct));
			else
				throw new erjang.NotImplemented();
		}
	}

	public static class LSIIID extends Insn { // E.g. 'bs_init2' v2
		final Label label;
		final SourceOperand src2;
		final int i3, i4,i5;
		final DestinationOperand dest;
		final boolean is_bs;
		public LSIIID(BeamOpcode opcode, Label label,
			      SourceOperand src2,
			      int i3, int i4, int i5,
			      DestinationOperand dest,
			      boolean is_bs)
		{
			super(opcode);
			this.label = label;
			this.src2 = src2;
			this.i3 = i3;
			this.i4 = i4;
			this.i5 = i5;
			this.dest = dest;
			this.is_bs = is_bs;
		}

		public ETuple toSymbolic(CodeTables ct) {
			if (is_bs)
				return ETuple.make(opcode.symbol,
						   toSymbolic(label,ct),
						   src2.toSymbolic(ct),
						   new ESmall(i3),
						   new ESmall(i4),
						   bsFieldFlagsToSymbolic(i5),
						   dest.toSymbolic(ct));
			else
				throw new erjang.NotImplemented();
		}
	}

	public static class LESD extends Insn { // E.g. 'bif1'
		final Label label;
		final int ext_fun_ref;
		final SourceOperand src;
		final DestinationOperand dest;
		public LESD(BeamOpcode opcode, Label label, int ext_fun_ref, SourceOperand src, DestinationOperand dest) {
			super(opcode);
			this.label = label;
			this.ext_fun_ref = ext_fun_ref;
			this.src = src;
			this.dest = dest;
		}
		public ETuple toSymbolic(CodeTables ct) {
			if (opcode == BeamOpcode.bif1)
				return ETuple.make(BIF_ATOM,
						   ct.atom(ct.extFun(ext_fun_ref).fun),
						   label.toSymbolic(ct),
						   EList.make(src.toSymbolic(ct)),
						   dest.toSymbolic(ct));
			else
				return ETuple.make(opcode.symbol,
						   ct.atom(ct.extFun(ext_fun_ref).fun),
						   label.toSymbolic(ct),
						   src.toSymbolic(ct),
						   dest.toSymbolic(ct));
		}
	}

	public static class LESSD extends Insn { // E.g. 'bif2'
		final Label label;
		final int ext_fun_ref;
		final SourceOperand src1;
		final SourceOperand src2;
		final DestinationOperand dest;
		public LESSD(BeamOpcode opcode, Label label, int ext_fun_ref, SourceOperand src1, SourceOperand src2, DestinationOperand dest) {
			super(opcode);
			this.label = label;
			this.ext_fun_ref = ext_fun_ref;
			this.src1 = src1;
			this.src2 = src2;
			this.dest = dest;
		}
		public ETuple toSymbolic(CodeTables ct) {
			if (opcode == BeamOpcode.bif2)
				return ETuple.make(BIF_ATOM,
						   ct.atom(ct.extFun(ext_fun_ref).fun),
						   label.toSymbolic(ct),
						   EList.make(src1.toSymbolic(ct),
							      src2.toSymbolic(ct)),
						   dest.toSymbolic(ct));
			else
				return ETuple.make(opcode.symbol,
						   ct.atom(ct.extFun(ext_fun_ref).fun),
						   label.toSymbolic(ct),
						   src1.toSymbolic(ct),
						   src2.toSymbolic(ct),
						   dest.toSymbolic(ct));
		}
	}

	public static class LEISD extends Insn { // E.g. 'gc_bif1'
		final Label label;
		final int ext_fun_ref;
		final int i;
		final SourceOperand src;
		final DestinationOperand dest;
		public LEISD(BeamOpcode opcode, Label label, int ext_fun_ref, int i, SourceOperand src, DestinationOperand dest) {
			super(opcode);
			this.label = label;
			this.ext_fun_ref = ext_fun_ref;
			this.i = i;
			this.src = src;
			this.dest = dest;
		}
		public ETuple toSymbolic(CodeTables ct) {
			//TODO: If is_gc_bif...
			return ETuple.make(GCBIF_ATOM,
					   ct.atom(ct.extFun(ext_fun_ref).fun),
					   label.toSymbolic(ct),
					   new ESmall(i),
					   EList.make(src.toSymbolic(ct)),
					   dest.toSymbolic(ct));
		}
	}

	public static class LEISSD extends Insn { // E.g. 'gc_bif2'
		final Label label;
		final int ext_fun_ref;
		final int i;
		final SourceOperand src1;
		final SourceOperand src2;
		final DestinationOperand dest;
		public LEISSD(BeamOpcode opcode, Label label, int ext_fun_ref, int i, SourceOperand src1, SourceOperand src2, DestinationOperand dest) {
			super(opcode);
			this.label = label;
			this.ext_fun_ref = ext_fun_ref;
			this.i = i;
			this.src1 = src1;
			this.src2 = src2;
			this.dest = dest;
		}
		public ETuple toSymbolic(CodeTables ct) {
			if (opcode == BeamOpcode.gc_bif2)
				return ETuple.make(GCBIF_ATOM,
						   ct.atom(ct.extFun(ext_fun_ref).fun),
						   label.toSymbolic(ct),
						   new ESmall(i),
						   EList.make(src1.toSymbolic(ct),
							      src2.toSymbolic(ct)),
						   dest.toSymbolic(ct));
			else
				return ETuple.make(opcode.symbol,
						   ct.atom(ct.extFun(ext_fun_ref).fun),
						   label.toSymbolic(ct),
						   new ESmall(i),
						   src1.toSymbolic(ct),
						   src2.toSymbolic(ct),
						   dest.toSymbolic(ct));
		}
	}

	public static class LSSII extends Insn { // E.g. 'bs_skip_bits2'
		final Label label;
		final SourceOperand src2;
		final SourceOperand src3;
		final int i4, i5;

		public LSSII(BeamOpcode opcode, Label label,
			     SourceOperand src2, SourceOperand src3,
			     int i4, int i5)
		{
			super(opcode);
			this.label = label;
			this.src2 = src2;
			this.src3 = src3;
			this.i4 = i4;
			this.i5 = i5;
		}
		public ETuple toSymbolic(CodeTables ct) {
			if (opcode == BeamOpcode.bs_skip_bits2)
				return ETuple.make(TEST_ATOM,
						   opcode.symbol,
						   label.toSymbolic(ct),
						   EList.make(src2.toSymbolic(ct),
							      src3.toSymbolic(ct),
							      new ESmall(i4),
							      bsFieldFlagsToSymbolic(i5)));
			else
				throw new erjang.NotImplemented();
		}
	}

	public static class LSISIID extends Insn { // E.g. 'bs_get_integer2'
		final Label label;
		final SourceOperand src2;
		final int i3;
		final SourceOperand src4;
		final int i5, i6;
		final DestinationOperand dest;
		final boolean is_test;

		public LSISIID(BeamOpcode opcode, Label label,
			       SourceOperand src2, int i3,
			       SourceOperand src4, int i5, int i6,
			       DestinationOperand dest,
			       boolean is_test)
		{
			super(opcode);
			this.label = label;
			this.src2 = src2;
			this.i3 = i3;
			this.src4 = src4;
			this.i5 = i5;
			this.i6 = i6;
			this.dest = dest;
			this.is_test = is_test;
		}
		public ETuple toSymbolic(CodeTables ct) {
			if (is_test)
				return ETuple.make(TEST_ATOM,
						   opcode.symbol,
						   label.toSymbolic(ct),
						   EList.make(src2.toSymbolic(ct),
							      new ESmall(i3),
							      src4.toSymbolic(ct),
							      new ESmall(i5),
							      bsFieldFlagsToSymbolic(i6),
							      dest.toSymbolic(ct)));
			else
				throw new erjang.NotImplemented();
		}
	}

	//============================================================

	public static class Select extends Insn { // E.g. 'select_val'
		final SourceOperand src;
		final Label defaultLabel;
		final SelectList jumpTable; // TODO: Improve representation.
		public Select(BeamOpcode opcode,
			      SourceOperand src,
			      Label defaultLabel,
			      SelectList jumpTable)
		{
			super(opcode);
			this.src = src;
			this.defaultLabel = defaultLabel;
			this.jumpTable = jumpTable;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   src.toSymbolic(ct),
					   defaultLabel.toSymbolic(ct),
					   jumpTable.toSymbolic(ct));
		}
	}

	public static class BSAppend extends Insn { // E.g. 'bs_append'
		// LSIIISIS
		final Label label;
		final SourceOperand i2;
		final int i3, i4, i5, i7;
		final SourceOperand src6, src8;
		public BSAppend(BeamOpcode opcode, Label label, SourceOperand i2, int i3, int i4, int i5, SourceOperand src6, int i7, SourceOperand src8) {
			super(opcode);
			this.label = label;
			this.i2 = i2;
			this.i3 = i3;
			this.i4 = i4;
			this.i5 = i5;
			this.src6 = src6;
			this.i7 = i7;
			this.src8 = src8;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   label.toSymbolic(ct),
					   i2.toSymbolic(ct),
					   new ESmall(i3),
					   new ESmall(i4),
					   new ESmall(i5),
					   src6.toSymbolic(ct),
					   bsFieldFlagsToSymbolic(i7),
					   src8.toSymbolic(ct));
		}
	}

	public static class BSPrivateAppend extends Insn { // E.g. 'bs_private_append'
		//  // LSISID
		final Label label;
		final SourceOperand src2, src4;
		final int i3, i5;
		final DestinationOperand dest;
		public BSPrivateAppend(BeamOpcode opcode, Label label, SourceOperand src2, int i3, SourceOperand src4, int i5, DestinationOperand dest) {
			super(opcode);
			this.label = label;
			this.src2 = src2;
			this.i3 = i3;
			this.src4 = src4;
			this.i5 = i5;
			this.dest = dest;
		}
		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(opcode.symbol,
					   label.toSymbolic(ct),
					   src2.toSymbolic(ct),
					   new ESmall(i3),
					   src4.toSymbolic(ct),
					   bsFieldFlagsToSymbolic(i5),
					   dest.toSymbolic(ct));
		}
	}

}
