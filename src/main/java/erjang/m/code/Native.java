package erjang.m.code;

import erjang.BIF;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;

public class Native extends ENative {
	@BIF
	public static EObject is_module_native(EObject module) {
		// TODO determine whether module is native, e.g. by calling
		// EModuleManager#get_module_info(EAtom) and adding a field native
		// to EModuleManager.ModuleInfo
		return ERT.FALSE;
		//return ERT.TRUE;
		//return ERT.am_undefined
	}
}
