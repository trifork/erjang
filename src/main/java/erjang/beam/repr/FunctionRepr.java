package erjang.beam.repr;

import java.util.List;

import erjang.beam.CodeAtoms;
import erjang.EObject;
import erjang.ETuple;
import erjang.ESmall;
import erjang.ESeq;


public class FunctionRepr {
	protected CodeTables.FunctionInfo sig;
	protected List<Insn> body;

	public FunctionRepr(CodeTables.FunctionInfo sig,
						List<Insn> body)
	{
		this.sig = sig;
		this.body = body;
	}

	public ETuple toSymbolic(CodeTables ct) {
		EObject[] symBody = new EObject[body.size()];
		int i = 0;
		for (Insn insn : body) {
			symBody[i++] = insn.toSymbolic(ct);
		}

		ETuple fun = ETuple.make(CodeAtoms.FUNCTION_ATOM,
								 ct.atom(sig.fun),
								 new ESmall(sig.arity),
								 new ESmall(sig.label),
								 ESeq.fromArray(symBody));
		return fun;
	}

}
