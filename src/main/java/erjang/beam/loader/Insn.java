package erjang.beam.loader;

import erjang.beam.BeamInstruction;
import erjang.beam.BeamOpcode;

import erjang.EObject;
import erjang.EAtom;
import erjang.ETuple;
import erjang.ESmall;

import static erjang.beam.loader.Operands.*;


public class Insn implements BeamInstruction {
	protected final BeamOpcode opcode;
	public Insn(BeamOpcode opcode) {this.opcode = opcode;}
	public BeamOpcode opcode() {return opcode;}

	public String toString() {return opcode+"(...)";}
	public EObject toSymbolic(CodeTables ct) {
	    return opcode.symbol;
	}

	/*============================================================
	 *                     Instruction formats
	 * Class names encode the format - the parameter types - as follows:
	 * I - Integer
	 * A - Atom
	 * S - Source operand: register or literal
	 * D - Destination operand: register
	 * L - Label
	 * K - Optional label (fail label)
	 * E - External function
	 * F - Anonymous function object
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
		final int fun_ref;
		public F(BeamOpcode opcode, int fun_ref) {
			super(opcode);
			this.fun_ref = fun_ref;
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
	}

	public static class LS extends Insn { // E.g. 'is_nil'
		final Label label;
		final SourceOperand src;
		public LS(BeamOpcode opcode, Label label, SourceOperand src) {
			super(opcode);
			this.label = label;
			this.src = src;
		}
	}

	public static class II extends Insn { // E.g. 'allocate'
		final int i1, i2;
		public II(BeamOpcode opcode, int i1, int i2) {
			super(opcode);
			this.i1 = i1;
			this.i2 = i2;
		}
	}

	public static class IL extends Insn { // E.g. 'call'
		final int i1;
		final Label label;
		public IL(BeamOpcode opcode, int i1, Label label) {
			super(opcode);
			this.i1 = i1;
			this.label = label;
		}
	}

	public static class SS extends Insn { // E.g. 'raise'
		final SourceOperand src1, src2;
		public SS(BeamOpcode opcode, SourceOperand src1, SourceOperand src2) {
			super(opcode);
			this.src1 = src1;
			this.src2 = src2;
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
	}

	public static class IE extends Insn { // E.g. 'call_ext'
		final int i1;
		final int ext_fun_ref;
		public IE(BeamOpcode opcode, int i1, int ext_fun_ref) {
			super(opcode);
			this.i1 = i1;
			this.ext_fun_ref = ext_fun_ref;
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
	}

	public static class III extends Insn { // E.g. 'allocate'
		final int i1, i2, i3;
		public III(BeamOpcode opcode, int i1, int i2, int i3) {
			super(opcode);
			this.i1 = i1;
			this.i2 = i2;
			this.i3 = i3;
		}
	}

	public static class LED extends Insn { // E.g. 'bif0'
		final Label label;
		final int ext_fun_ref;
		final DestinationOperand dest;
		public LED(BeamOpcode opcode, Label label, int ext_fun_ref, DestinationOperand dest) {
			super(opcode);
			this.label = label;
			this.ext_fun_ref = ext_fun_ref;
			this.dest = dest;
		}
	}

	public static class LSI extends Insn { // E.g. 'test_arity'
		final Label label;
		final SourceOperand src;
		final int i1;
		public LSI(BeamOpcode opcode, Label label, SourceOperand src, int i1) {
			super(opcode);
			this.label = label;
			this.src = src;
			this.i1 = i1;
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
	}

	public static class LSS extends Insn { // E.g. 'is_eq_exact'
		final Label label;
		final SourceOperand src1;
		final SourceOperand src2;
		public LSS(BeamOpcode opcode, Label label, SourceOperand src1, SourceOperand src2) {
			super(opcode);
			this.label = label;
			this.src1 = src1;
			this.src2 = src2;
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
	}


	public static class ILI extends Insn { // E.g. 'call'
		final int i1, i3;
		final Label label;
		public ILI(BeamOpcode opcode, int i1, Label label, int i3) {
			super(opcode);
			this.i1 = i1;
			this.label = label;
			this.i3 = i3;
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
	}


	public static class LSIID extends Insn { // E.g. 'bs_start_match2'
		final Label label;
		final SourceOperand src;
		final int i3, i4;
		final DestinationOperand dest;
		public LSIID(BeamOpcode opcode, Label label, SourceOperand src, int i3, int i4, DestinationOperand dest) {
			super(opcode);
			this.label = label;
			this.src = src;
			this.i3 = i3;
			this.i4 = i4;
			this.dest = dest;
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
	}

	//============================================================

	public static class Select extends Insn { // E.g. 'select_val'
		SourceOperand src;
		Label defaultLabel;
		Operands.List jumpTable; // TODO: Improve representation.
		public Select(BeamOpcode opcode,
			      SourceOperand src,
			      Label defaultLabel,
			      Operands.List jumpTable)
		{
			super(opcode);
			this.src = src;
			this.defaultLabel = defaultLabel;
			this.jumpTable = jumpTable;
		}
	}

}
