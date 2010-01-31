package erjang.beam.loader;

import erjang.EObject;
import erjang.EAtom;
import erjang.EString;

public class CodeTables {
	EAtom[] atoms;
	EString[] strings;
	EObject[] literals;
	ExtFun[] externalFuns;

	EAtom atom(int nr) {return atoms[nr-1];}		// 1-based
	ExtFun extFun(int nr) {return externalFuns[nr];}	// 0-based
}
