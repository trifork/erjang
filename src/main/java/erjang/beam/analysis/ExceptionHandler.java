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

import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;

/* Representation of the stack of exception handlers active at a given
 * program point.
 * 'null' is the end of the stack chain.
 */
public class ExceptionHandler implements BeamExceptionHandler {
	/** Special value, for use in exception handler set. */
	public static final BeamExceptionHandler NULL = new BeamExceptionHandler() {
		public int getHandlerLabel() {throw new InternalError();}
		public BeamExceptionHandler getParent() {throw new InternalError();}
		public String toString() {return "NULL";}
	};

	private final int handlerLabel;
	private final ExceptionHandler parent;

	public ExceptionHandler(int handlerLabel, ExceptionHandler parent) {
		this.handlerLabel = handlerLabel;
		this.parent = parent;
	}

	public int getHandlerLabel() {return handlerLabel;}
	public ExceptionHandler getParent() {return parent;}
	public List<BeamExceptionHandler> ambiguousities() {return null;}

	public static ExceptionHandler push(ExceptionHandler org, int newHandlerLabel) {
		assert(! (org instanceof Ambiguous));
		return new ExceptionHandler(newHandlerLabel, org);
	}

	public static ExceptionHandler pop(ExceptionHandler org) {
		if (org==null)
			throw new IllegalArgumentException("Exception handler structure in beam code is not sound");
		assert(! (org instanceof Ambiguous));
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

	public boolean equals(Object other) {
		return other instanceof ExceptionHandler &&
			equals((ExceptionHandler) other);
	}
	public boolean equals(ExceptionHandler other) {
		return equals(this,other);
	}
	public static boolean equals(ExceptionHandler e1, ExceptionHandler e2) {
	    if (e1==e2) return true;
		if (e1==null || e2==null) return false;
		if (e1.handlerLabel != e2.handlerLabel) return false;

		if (e1 instanceof Ambiguous && e2 instanceof Ambiguous) {
			Ambiguous a1 = (Ambiguous) e1;
			Ambiguous a2 = (Ambiguous) e2;
			return a1.exhs.equals(a2.exhs);
		}
		if (e1 instanceof Ambiguous || e2 instanceof Ambiguous)
			return false;

		return equals(e1.parent, e2.parent);
	}

	public int hashCode() {
		return handlerLabel;
	}

	public int compareTo(Object other) {
		if (! (other instanceof ExceptionHandler)) return 1;
		return compareTo((ExceptionHandler) other);
	}
	public int compareTo(ExceptionHandler other) {
		return this.handlerLabel - other.handlerLabel;
	}

	public String toString() {
		return "lbl" + handlerLabel + "+" +parent;
	}

	public static class Ambiguous extends ExceptionHandler {
		final Set<BeamExceptionHandler> exhs;
		public Ambiguous(ExceptionHandler src1, ExceptionHandler src2) {
			super(-1, null);
			exhs = new HashSet();
			add_to_set(exhs, src1);
			add_to_set(exhs, src2);
		}
		public Ambiguous(Set<BeamExceptionHandler> exhs) {
			super(-1, null);
			this.exhs = exhs;
		}

		public static Ambiguous make(ExceptionHandler src1, ExceptionHandler src2) {
			HashSet<BeamExceptionHandler> exhs = new HashSet();
			add_to_set(exhs, src1);
			add_to_set(exhs, src2);
			Ambiguous a;
			if (src1 instanceof Ambiguous && exhs.equals((a=(Ambiguous)src1).exhs))
				return a;
			if (src2 instanceof Ambiguous && exhs.equals((a=(Ambiguous)src2).exhs))
				return a;
			return new Ambiguous(exhs);
		}

		private static void add_to_set(Set<BeamExceptionHandler> exhs,
									   ExceptionHandler src)
		{
			if (src instanceof Ambiguous)
				exhs.addAll(((Ambiguous)src).exhs);
			else if (src==null)
				exhs.add(NULL);
			else
				exhs.add(src);
		}

	    public int hashCode() {
			return exhs.hashCode();
	    }

		public List<BeamExceptionHandler> ambiguousities() {
			List<BeamExceptionHandler> result = new ArrayList<BeamExceptionHandler>(exhs.size());
			for (BeamExceptionHandler x: exhs)
				result.add(x==NULL? null : x);
			return result;
		}

		public String toString() {
			StringBuffer res = new StringBuffer("ambi(");
			for (BeamExceptionHandler e : exhs) {
				res.append(e).append(" ");
			}
			res.append(")");
			return res.toString();
		}
	}
}
