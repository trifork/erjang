package erjang.m.file;

import erjang.BIF;
import erjang.EAtom;
import erjang.ENative;
import erjang.EObject;

public class Native extends ENative {
	public static final EAtom am_latin1 = EAtom.intern("latin1");
	public static final EAtom am_utf8 = EAtom.intern("utf8");
	public static final EAtom am_unicode = EAtom.intern("unicode");
	
	
	@BIF
	public static EObject native_name_encoding() {
		return am_utf8;
	}
}
