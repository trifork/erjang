/**
 * This file is part of Erjang - A JVM-based Erlang VM
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
package erjang.beam.analysis;

import erjang.beam.BeamExceptionHandler;
import erjang.ErlangException;

/* Representation of the stack of exception handlers active at a given
 * program point.
 * 'null' is the end of the stack chain.
 */
public class ExceptionHandler implements BeamExceptionHandler {
	private final int handlerLabel;
	private final ExceptionHandler parent;

	public ExceptionHandler(int handlerLabel, ExceptionHandler parent) {
		this.handlerLabel = handlerLabel;
		this.parent = parent;
	}

	public int getHandlerLabel() {return handlerLabel;}
	public ExceptionHandler getParent() {return parent;}

	public static ExceptionHandler push(ExceptionHandler org, int newHandlerLabel) {
		return new ExceptionHandler(newHandlerLabel, org);
	}

	public static ExceptionHandler pop(ExceptionHandler org) {
		if (org==null)
			throw new IllegalArgumentException("Exception handler structure in beam code is not sound");
		return org.parent;
	}

	public static ExceptionHandler merge(ExceptionHandler e1, ExceptionHandler e2) throws ErlangException {
		// Simple cases first:
		if (e1 == e2) return e1;

		if (e1 == null || e2 == null ||
		    e1.handlerLabel != e2.handlerLabel)
			throw new IllegalArgumentException("Exception handler structure in beam code is not sound");

		// 'this' and 'other' are not null, and labels are known to match.
		// Compare parents:

		ExceptionHandler mergedParent = merge(e1.parent, e2.parent);
		if (mergedParent == e1.parent) return e1; // Save memory if possible.
		if (mergedParent == e2.parent) return e2; // Save memory if possible.
		return new ExceptionHandler(e1.handlerLabel, mergedParent);
	}

	public String toString() {
		return "lbl" + handlerLabel + "+" +parent;
	}
}
