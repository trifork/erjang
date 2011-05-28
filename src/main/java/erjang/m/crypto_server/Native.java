package erjang.m.crypto_server;

import erjang.BIF;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;
import erjang.ETuple2;

public class Native extends ENative {

	/** fake that the crypto_server launched */
	
	@BIF
	public static EObject init(EObject arg) {
		return new ETuple2(ERT.am_ok, ERT.NIL);
	}

}
