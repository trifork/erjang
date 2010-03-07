package erjang.beam.repr;

import erjang.EObject;
import erjang.EAtom;
import erjang.ETuple;
import erjang.ESmall;
import erjang.EBitString;
import erjang.EBinary;
import erjang.EString;

import java.util.Map;
import java.util.HashMap;

import static erjang.beam.CodeAtoms.EXTFUNC_ATOM;

public class CodeTables {
	protected EAtom[]   atoms;
	protected EObject[] literals;
	protected ExtFun[]  externalFuns;
	protected AnonFun[] anonymousFuns;
	protected byte[]    stringpool;

 	protected Map<Integer,FunctionInfo> functionMap = new HashMap();

	public EAtom atom(int nr) {return atoms[nr-1];}		// 1-based
	public EObject literal(int nr) {return literals[nr];}		// 0-based
	public ExtFun extFun(int nr) {return externalFuns[nr];}	// 0-based
	public AnonFun anonFun(int nr) {return anonymousFuns[nr];}
	public EBitString bitstring(int start, int bits) {
		return EBitString.make(stringpool, start, bits/8, bits%8);
	}
	public EString string(int start, int bytes) {
		return EString.make(stringpool, start, bytes);
	}

	public EAtom moduleName() { return atom(1); }

	protected void addFunctionAtLabel(int label, int fun, int arity) {
		assert(! functionMap.containsKey(label));
		functionMap.put(label, new FunctionInfo(fun, arity, label));
	}

	public FunctionInfo functionAtLabel(int label) {
		return functionMap.get(label);
	}

    //========== Function representations: ====================

	public class FunctionInfo {
		public final int fun, arity, label;
		public FunctionInfo(int fun, int arity, int label) {
			this.fun = fun;
			this.arity = arity;
			this.label = label;
		}
		public String toString() {
			return moduleName()+":"+atom(fun)+"/"+arity;
		}
		public EAtom name()   {return atom(fun);}

		public ETuple toSymbolic() {
			return ETuple.make(moduleName(),
					   atom(fun),
					   new ESmall(arity));
		}
	}

	public class AnonFun {
		public final int fun;
		public final int total_arity, free_vars;
		public final int label;
		public final int occur_nr;
		public final int uniq;

		public AnonFun(int fun, int total_arity,
					   int free_vars, int label, int occur_nr, int uniq)
		{
			this.fun=fun;
			this.total_arity=total_arity;
			this.free_vars=free_vars;
			this.label=label;
			this.occur_nr=occur_nr;
			this.uniq=uniq;
		}

		public String toString() {
			return fun+"/"+total_arity;
		}

		public ETuple toSymbolic(CodeTables ct) {
			return ETuple.make(moduleName(),
							   ct.atom(fun),
							   new ESmall(total_arity));
		}
	}
}
