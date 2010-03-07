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

	protected void addFunctionAtLabel(FunctionInfo fi) {
		assert(! functionMap.containsKey(fi.label));
		functionMap.put(fi.label, fi);
	}

	public FunctionInfo functionAtLabel(int label) {
		return functionMap.get(label);
	}
}
