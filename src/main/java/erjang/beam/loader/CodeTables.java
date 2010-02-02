package erjang.beam.loader;

import erjang.EObject;
import erjang.EAtom;
import erjang.EString;
import erjang.ETuple;
import erjang.ESmall;

import java.util.Map;
import java.util.HashMap;

public class CodeTables {
	protected EAtom[]   atoms;
	protected EString[] strings;
	protected EObject[] literals;
	protected ExtFun[]  externalFuns;
	protected AnonFun[] anonymousFuns;

 	protected Map<Integer,FunctionInfo> functionMap = new HashMap();

	EAtom atom(int nr) {return atoms[nr-1];}		// 1-based
	EObject literal(int nr) {return literals[nr];}		// 0-based
	ExtFun extFun(int nr) {return externalFuns[nr];}	// 0-based
	AnonFun anonFun(int nr) {return anonymousFuns[nr];}

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
