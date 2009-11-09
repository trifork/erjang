package org.erlang;

public class ErlangException extends RuntimeException {

	public final EAtom reason;
	private final EAtom module;
	private final EAtom function;
	private final ESeq args;

	public ErlangException(EAtom reason, String module, String function,
			Object[] args, Throwable cause) {
		super(reason.getName(), cause);
		this.reason = reason;
		this.module = EAtom.intern(module);
		this.function = EAtom.intern(function);
		this.args = listify(args);
	}

	public ErlangException(EAtom reason, String module, String function,
			Object[] args) {
		super(reason.getName());
		this.reason = reason;
		this.module = EAtom.intern(module);
		this.function = EAtom.intern(function);
		this.args = listify(args);
	}

	private ESeq listify(Object[] args) {
		ESeq res = ESeq.EMPTY;
		for (int i = args.length-1; i >= 0; i--) {
			EObject h = erl_value(args[i]);
			res = res.cons(h);
		}
		return res;
	}

	private EObject erl_value(Object object) {
		// TODO Auto-generated method stub
		return null;
	}

}
