/**
 * 
 */
package erjang.jbeam;

import erjang.EAtom;

public enum BeamFileTag {

	// reserved words we use
	
	K_return,
	K_case,
	
	// these are in no particular order...
	
	x, put_tuple, y, move, function, file, put, exports, 
	module, attributes, comp_info, label, func_info, atom, 
	f, test, integer, allocate_zero, gc_bif, call, deallocate, 
	call_ext_only, extfunc, call_only, call_ext;

	
	BeamFileTag get(EAtom sym) {
		String name = sym.getName();
		if ("return".equals(name)) {
			name = "K_return";
		}
		if ("case".equals(name)) {
			name = "K_case";
		}
		return BeamFileTag.valueOf(name);
	}


}