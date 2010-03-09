/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2009 by Trifork
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

package erjang.beam;

import erjang.EAtom;
import erjang.EList;
import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.ETuple;
import erjang.NotImplemented;

import erjang.beam.repr.CodeTables;
import erjang.beam.repr.Insn;

public class SymbolicBeamFileData implements BeamFileData {

	private static final EAtom BEAM_FILE = EAtom.intern("beam_file");
	private static final EAtom MODULE = EAtom.intern("module");
	private static final EAtom EXPORTS = EAtom.intern("exports");
	private static final EAtom ATTRIBUTES = EAtom.intern("attributes");
	private static final EAtom COMP_INFO = EAtom.intern("comp_info");
	private static final EAtom CODE = EAtom.intern("code");

	private EAtom module;
	private ESeq exports;
	private ESeq attributes;
	private ESeq comp_info;
	private ESeq code;

	public SymbolicBeamFileData(ETuple data) {
		assert (data.elm(1) == BEAM_FILE);

		module = data.elm(2).testAtom();
		exports = data.elm(3).testSeq();
		attributes = data.elm(4).testSeq();
		comp_info = data.elm(5).testSeq();
		code = data.elm(6).testSeq();
	}

	public void accept(ModuleVisitor v) {
		v.visitModule(module);

		visit_exports(v);

		visit_attributes(v);

		try {
			for (ESeq exp = (ESeq) code; exp != ERT.NIL; exp = exp.tail()) {
				ETuple fun = (ETuple) exp.head();

				visit_function(v, fun);
			}
		} finally {
			v.visitEnd();
		}
	}

	private void visit_function(ModuleVisitor v, ETuple fun) {

		EAtom name = (EAtom) fun.elm(2);
		int ary = fun.elm(3).asInt();
		int entry = fun.elm(4).asInt();
		EList insns = (EList) fun.elm(5);

		FunctionVisitor fv = v.visitFunction(name, ary, entry);

		visit_insns(insns, fv);

		fv.visitEnd();
	}

	private void visit_insns(EList insns, FunctionVisitor fv) {
		throw new NotImplemented();
		/*
		BlockVisitor bbv = null;

		for (ESeq insn = (ESeq) insns; insn != ERT.NIL; insn = insn.tail()) {

			EObject i = insn.head();

			if (i instanceof EAtom) {
				BeamOpcode opcode = BeamOpcode.get((EAtom) i);
				bbv.visitInsn(opcode, ETuple.make(new EObject[] { i }));

			} else if (i instanceof ETuple) {
				ETuple et = (ETuple) i;
				BeamOpcode opcode = BeamOpcode.get((EAtom) et.elm(1));

				if (opcode == BeamOpcode.label) {
					ESmall label = (ESmall) et.elm(2);

					if (bbv != null) {
						bbv.visitEnd();
					}

					bbv = fv.visitLabeledBlock(label.asInt());
				} else {
					bbv.visitInsn(opcode, et);
				}
			} else {
				throw new IllegalArgumentException();
			}
		}
		if (bbv != null) {
			bbv.visitEnd();
		}
		*/
	}

	private void visit_attributes(ModuleVisitor v) {
		for (ESeq exp = (ESeq) attributes; exp != ERT.NIL; exp = exp.tail()) {
			ETuple one = (ETuple) exp.head();
			v.visitAttribute((EAtom) one.elm(1), one.elm(2));
		}
	}

	private void visit_exports(ModuleVisitor v) {
		for (ESeq exp = (ESeq) exports; exp != ERT.NIL; exp = exp.tail()) {
			ETuple one = (ETuple) exp.head();
			v.visitExport((EAtom) one.elm(1), one.elm(2).asInt(), one.elm(3)
					.asInt());
		}
	}

}
