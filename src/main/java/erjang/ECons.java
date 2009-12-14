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
import java.util.List;

public abstract class ECons extends EObject {

	@Override
	int cmp_order() {
		return CMP_ORDER_LIST;
	}

	@Override
	int compare_same(EObject rhs) {
		if (rhs.isNil()) return 1;
		ECons other = rhs.testCons();
		int cmp1 = head().compareTo(other.head());
		if (cmp1 != 0)
			return cmp1;
		return tail().compareTo(other.tail());
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

}
