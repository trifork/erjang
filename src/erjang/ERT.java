package org.erlang;

public class ERT {

	public static final EAtom AM_BADARG = EAtom.intern("badarg");
	public static final EAtom AM_BADMATCH = EAtom.intern("badmatch");
	public static final EAtom AM_BADARITH = EAtom.intern("badarith");

	public static ETerm cons(EObject h, ETerm t) {
		if (t instanceof ECons) {
			return ((ECons) t).cons(h);
		} else {
			return new EPair(h, t);
		}
	}

	public static ETerm cons(ETerm h, ECons t) {
		return ((ECons) t).cons(h);
	}

	public static ErlangException badarg(String module,
			String function, Object... args) {
			throw new ErlangException(AM_BADARG, module, function, args);
	}

	public static ErlangException badarg(Throwable cause, String module,
			String function, Object... args) {
			throw new ErlangException(AM_BADARG, module, function, args, cause);
	}

	public static ErlangException badarith(ArithmeticException cause,
			String module, String function, Object... args) {
		throw new ErlangException(AM_BADARITH, module, function, args, cause);
	}

	public static ErlangException badarith(String module, String function, Object... args) {
		throw new ErlangException(AM_BADARITH, module, function, args);
	}

}
