package erjang.m.erl_ddll;

import erjang.BIF;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;
import erjang.ETuple2;
import erjang.NotImplemented;
import erjang.driver.Drivers;

public class Native extends ENative {

	@BIF public static 
	ETuple2 loaded_drivers() {
		return new ETuple2(ERT.am_ok, Drivers.getLoaded());
	}
	
	@BIF public static 
	EObject info(EObject driver, EObject what) {
		// TODO: implement
		return ERT.am_undefined;
	}
	
	@BIF public static 
	EObject try_load(EObject path, EObject driver, EObject options) {
		// TODO: implement
		throw new NotImplemented();
	}
	
	@BIF public static 
	EObject try_unload(EObject driver, EObject options) {
		// TODO: implement
		throw new NotImplemented();
	}
	
	@BIF public static 
	EObject format_error_int(EObject err) {
		// TODO: implement
		throw new NotImplemented();
	}
	
	
	
	
}
