package erjang.beam.loader;

import erjang.EObject;
import erjang.EAtom;
import erjang.ETuple;
import erjang.ESmall;
import erjang.EBitString;
import erjang.EBinary;
import erjang.EString;

import java.util.Map;
import java.util.HashMap;

public class CodeTables {
	protected EAtom[]   atoms;
	protected EObject[] literals;
	protected ExtFun[]  externalFuns;
	protected AnonFun[] anonymousFuns;
	protected byte[]    stringpool;

 	protected Map<Integer,FunctionInfo> functionMap = new HashMap();

	EAtom atom(int nr) {return atoms[nr-1];}		// 1-based
	EObject literal(int nr) {return literals[nr];}		// 0-based
	ExtFun extFun(int nr) {return externalFuns[nr];}	// 0-based
	AnonFun anonFun(int nr) {return anonymousFuns[nr];}
	EBitString bitstring(int start, int bits) {
		return EBitString.make(stringpool, start, bits/8, bits%8);
	}
	EString string(int start, int bytes) {
		return EString.make(stringpool, start, bytes);
	}

	EAtom moduleName() { return atom(1); }

	protected void addFunctionAtLabel(int label, int fun, int arity) {
		assert(! functionMap.containsKey(label));
		functionMap.put(label, new FunctionInfo(fun, arity, label));
	}

	public FunctionInfo functionAtLabel(int label) {
		return functionMap.get(label);
	}

	class FunctionInfo {
		int fun, arity, label;
		public FunctionInfo(int fun, int arity, int label) {
			this.fun = fun;
			this.arity = arity;
			this.label = label;
		}
		public String toString() {
			return moduleName()+":"+atom(fun)+"/"+arity;
		}

		public ETuple toSymbolic() {
			return ETuple.make(moduleName(),
					   atom(fun),
					   new ESmall(arity));
		}
	}
}
