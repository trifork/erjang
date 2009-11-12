package erjang.jbeam.ops;

public class RegisterY extends Register {

	public RegisterY(int reg) {
		super(reg);
	}

	@Override
	int assigned(FunctionAdapter fa) {
		return fa.get_yreg(reg);
	}
}
