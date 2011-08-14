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

package erjang.beam.loader;

import erjang.beam.BeamOpcode;
import erjang.beam.repr.Insn;
import erjang.beam.repr.Operands.Label;
import erjang.beam.repr.FunctionInfo;
import erjang.EAtom;

import java.util.List;
import java.util.ListIterator;

public class Rewriter {
    public void rewriteFunctionBody(List<Insn> body, FunctionInfo sig) {
	ListIterator<Insn> it = body.listIterator();
	while (it.hasNext()) {
	    final Insn insn = it.next();
	    switch (insn.opcode()) {
	    case call_fun: {
		int save_pos = savePos(it);
		boolean trigger = (it.hasNext() &&
				   it.next().opcode() == BeamOpcode.deallocate &&
				   it.hasNext() &&
				   it.next().opcode() == BeamOpcode.K_return);
		restorePos(it, save_pos);
		if (trigger) {
		    Insn.I old = (Insn.I)insn;
		    it.set(new Insn.I(BeamOpcode.i_call_fun_last, old.i1)); // We don't care about deallocating.
		    it.next();
		    it.remove();
		    it.next();
		    it.remove();
		}
	    }//switch
	    }
	}
    }

    private int savePos(ListIterator<Insn> it) {return it.nextIndex() - 1;}

    private void restorePos(ListIterator<Insn> it, int save_pos) {
	// Only works for restoring backwards.
	int i=0;
	while (it.previousIndex() >= save_pos) {
	    int pi = it.previousIndex();
	    Insn insn = it.previous(); i++;
	}
	if (it.nextIndex() <= save_pos) it.next(); // Turn iterator.
    }

    private Insn nop() {return new Insn.I(BeamOpcode.label, -1);}
}