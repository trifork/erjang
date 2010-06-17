/** -*- tab-width: 4 -*-
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/

package erjang.beam.repr;

import java.util.List;

import erjang.beam.BeamOpcode;
import erjang.beam.ModuleVisitor;
import erjang.beam.FunctionVisitor;
import erjang.beam.BlockVisitor;

import erjang.beam.CodeAtoms;
import erjang.EObject;
import erjang.ETuple;
import erjang.ESmall;
import erjang.ESeq;


public class FunctionRepr {
	protected FunctionInfo sig;
	protected List<Insn> body;

	public FunctionRepr(FunctionInfo sig, List<Insn> body) {
		this.sig = sig;
		this.body = body;
	}

	//==================== Visitation ====================
	public void declare(ModuleVisitor mv) {
		mv.declareFunction(sig.fun, sig.arity, sig.label);
	}
	
	public void accept(ModuleVisitor mv) {
		FunctionVisitor fv =
			mv.visitFunction(sig.fun, sig.arity, sig.label);
		accept(fv);
	}

	public void accept(FunctionVisitor fv) {
		BlockVisitor bv = null;
		for (Insn insn : body) {
			if (insn.opcode == BeamOpcode.label) {
				if (bv != null) bv.visitEnd();
				bv = fv.visitLabeledBlock(((Insn.I)insn).i1);
			} else {
				bv.visitInsn(insn);
			}
		}
		if (bv != null) bv.visitEnd();
		fv.visitEnd();
	}

	//==================== Symbolic form ====================
	public ETuple toSymbolic() {
		EObject[] symBody = new EObject[body.size()];
		int i = 0;
		for (Insn insn : body) {
			symBody[i++] = insn.toSymbolic();
		}

		ETuple fun = ETuple.make(CodeAtoms.FUNCTION_ATOM,
								 sig.fun,
								 new ESmall(sig.arity),
								 new ESmall(sig.label),
								 ESeq.fromArray(symBody));
		return fun;
	}

}
