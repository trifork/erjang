package org.erlang.jbeam.ops;

public class RegisterX extends Register {

	public RegisterX(int reg) {
		super(reg);
	}

	@Override
	int assigned(FunctionAdapter fa) {
		return fa.get_xreg(reg);
	}

}
