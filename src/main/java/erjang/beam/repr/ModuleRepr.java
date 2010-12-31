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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import erjang.EAtom;
import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.ETuple;
import erjang.beam.BeamFileData;
import erjang.beam.CodeAtoms;
import erjang.beam.ModuleVisitor;

public class ModuleRepr implements BeamFileData {
	private EAtom moduleName;
	private final CodeTables ct;
	private final FunctionInfo[] exports;
	private final FunctionRepr[] functions;
	private ESeq attributes;
	private ESeq compilation_info;

	public ModuleRepr(CodeTables ct,
					  EAtom moduleName, FunctionInfo[] exports,
					  ESeq attributes, ESeq comp_info, FunctionRepr[] functions)
    {
		this.moduleName = moduleName;
		this.ct = ct;
		this.exports = exports;
		this.attributes = attributes;
		this.compilation_info = comp_info;
		this.functions = functions;
	}

	public void accept(ModuleVisitor v) {
		v.visitModule(moduleName);

		visit_exports(v);

		visit_attributes(v);

		try {
			for (FunctionRepr fun : functions) fun.declare(v);
			for (FunctionRepr fun : functions) fun.accept(v);
		} catch (RuntimeException e) {
			System.err.println("ModuleRepr: Error in traversal: "+e);
			e.printStackTrace(System.err);
			throw(e);
		} finally {
			v.visitEnd();
		}
	}

	private void visit_attributes(ModuleVisitor v) {
		for (ESeq exp = (ESeq) attributes; exp != ERT.NIL; exp = exp.tail()) {
			ETuple attr = (ETuple) exp.head();
			v.visitAttribute((EAtom) attr.elm(1), attr.elm(2));
		}
	}

	private void visit_exports(ModuleVisitor v) {
		for (FunctionInfo exp : exports) {
			v.visitExport(exp.fun, exp.arity, exp.label);
		}
	}


	//==================== Symbolic form ====================
    public ETuple toSymbolic() {
		return ETuple.make(CodeAtoms.BEAM_FILE_ATOM,
						   moduleName,
						   symbolicExportList(),
						   symbolicAttributes(),
						   compilation_info,
						   symbolicCode());
    }

    public EObject symbolicAttributes() {
		// Sort the attributes to make them comparable to beam_disasm's output:
		EObject[] attrs = attributes.toArray();
		Arrays.sort(attrs, EObject.ERLANG_ORDERING);
		return ESeq.fromArray(attrs);
    }

    public ESeq symbolicExportList() {
		ArrayList<EObject> symExports = new ArrayList<EObject>(exports.length);
		int i=0;
		for (FunctionInfo f : exports) {
			symExports.add(ETuple.make(f.fun,
									   new ESmall(f.arity),
									   new ESmall(f.label)));
		}
		Collections.sort(symExports, EObject.ERLANG_ORDERING);
		return ESeq.fromList(symExports);
    }

    public ESeq symbolicCode() {
		ArrayList<ETuple> symFunctions =
			new ArrayList<ETuple>(functions.length);
		for (FunctionRepr fun : functions) {
			symFunctions.add(fun.toSymbolic());
		}

		return ESeq.fromList(symFunctions);
    }

}
