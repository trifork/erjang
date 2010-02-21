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

package erjang;

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import java.io.IOException;
import java.util.List;
import java.util.Set;

import erjang.m.ets.EMatchContext;
import erjang.m.ets.EPattern;
import erjang.m.ets.ETermPattern;

public abstract class ECons extends EObject {

	@Override
	int cmp_order() {
		return CMP_ORDER_LIST;
	}

	public boolean match(ETermPattern matcher, EMatchContext r) {
		return matcher.match(this, r);
	}

	@Override
	public ETermPattern compileMatch(Set<Integer> out) {
		return EPattern.compilePattern(this, out);
	}


	@Override
	int compare_same(EObject rhs) {
		ECons ths = this;
		ECons other = rhs.testCons();
		do {
			// This would have been nicer if not ENil <: ECons...:
			if (other.isNil())
				return ths.isNil() ? 0 : 1;
			if (ths.isNil()) return -1;

			int cmp1 = ths.head().compareTo(other.head());
			if (cmp1 != 0)
				return cmp1;

			// Iterate instead of recurse if possible:
			EObject thisTail = ths.tail();
			EObject otherTail = other.tail();
			if (! (thisTail instanceof ECons &&
			       otherTail instanceof ECons))
				return thisTail.compareTo(otherTail);
			else {
				ths = thisTail.testCons();
				other = otherTail.testCons();
			}
		} while (true);
	}

	public abstract EObject head();

	public abstract EObject tail();

	public ECons testCons() {
		return this;
	}

	// generic count function that also works for non-well-formed lists
	public int count() {
		EObject cell = tail();
		int count = 1;
		while (cell != ERT.NIL && (cell instanceof ECons)) {
			cell = ((ECons) cell).tail();
			count += 1;
		}
		return count;
	}

	/**
	 * @param c1
	 * @return
	 */
	public ECons prepend(ECons list) {
		if (list.testNil() != null) {
			return this;
		}
		return prepend(list.tail()).cons(list.head());
	}

	private ECons prepend(EObject o) {
		if (o.testNil() != null) {
			return this;
		}
		ECons list = o.testCons();
		if (list == null) {
			throw ERT.badarg();
		}
		return prepend(list.tail()).cons(list.head());
	}

	public boolean collectIOList(List<ByteBuffer> out) {

		EObject tail = null;
		EBinary binary;

		ECons list = this;
		while (!list.isNil()) {

			EObject head = list.head();

			ESmall intval;
			if ((intval = head.testSmall()) != null) {
				ByteArrayOutputStream barr = new ByteArrayOutputStream();

				byte_loop: do {

					int byteValue = intval.value & 0xff;
					if (intval.value != byteValue) {
						return false;
					}

					barr.write(byteValue);

					tail = list.tail();
					if ((list = tail.testCons()) != null) {
						head = list.head();
					} else {
						break byte_loop;
					}
					
				} while ((intval = head.testSmall()) != null);

				out.add(ByteBuffer.wrap(barr.toByteArray()));

			} else if ((binary = head.testBinary()) != null) {
				binary.collectIOList(out);

			} else {
				// not a well-formed iolist
				return false;
			}

			tail = list.tail();
			if ((list = tail.testCons()) == null)
				break;
		}

		if (!tail.collectIOList(out)) {
			return false;
		}

		return true;
	}


	public void collectCharList(CharCollector out)
		throws CharCollector.CollectingException,
		CharCollector.InvalidElementException,
		IOException
	{
		ECons list;
		EObject tail;
		for (tail=this; (list = tail.testCons()) != null; tail = list.tail()) {
			EObject head = list.head();

			ESmall intval;
			if ((intval = head.testSmall()) != null) {
				try {
					out.addInteger(intval.value);
				} catch (CharCollector.DecodingException e) {
					throw new CharCollector.CollectingException(list);
				}
			} else {
				head.collectCharList(out);
			}
		}

		if (tail.testNumber() != null) {
			// Only nil and binaries are allowed as tail
			throw new CharCollector.InvalidElementException();
		} else tail.collectCharList(out);
	}

}
