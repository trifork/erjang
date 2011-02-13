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

			int cmp1 = ths.head().erlangCompareTo(other.head());
			if (cmp1 != 0)
				return cmp1;

			// Iterate instead of recurse if possible:
			EObject thisTail = ths.tail();
			EObject otherTail = other.tail();
			if (! (thisTail instanceof ECons &&
			       otherTail instanceof ECons))
				return thisTail.erlangCompareTo(otherTail);
			else {
				ths = thisTail.testCons();
				other = otherTail.testCons();
			}
		} while (true);
	}


	/* 
	 * Here is a generic list-oriented equalsExactly.  If need be,
	 * we can implement optimized versions for subclasses.
	 */
	@Override
	public boolean equalsExactly(EObject rhs) {
		if (this==rhs) 
			return true;
		
		ECons ths = this;
		ECons other = rhs.testCons();
		if (other == null) return false;
		do {
			// This would have been nicer if not ENil <: ECons...:
			boolean other_is_nil = other.isNil();
			boolean this_is_nil = ths.isNil();
			if (other_is_nil)
				return this_is_nil;
			
			if (this_is_nil) 
				return false;

			if (!ths.head().equalsExactly(other.head())) {
				return false;
			}

			// Iterate instead of recurse if possible:
			EObject thisTail = ths.tail();
			EObject otherTail = other.tail();

			ths = thisTail.testCons();
			other = otherTail.testCons();

			if (ths==null || other==null)
				return thisTail.equalsExactly(otherTail);
			
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
	public ECons prepend(ESeq list) {
		
		// first, rlist=lists:reverse(list)
		ESeq rlist = ERT.NIL;
		while (!list.isNil()) {
			rlist = rlist.cons(list.head());
			list = list.tail();
		}

		// then, prepend rlist on this
		ECons r = this;
		while(!rlist.isNil()) {
			r = r.cons(rlist.head());
			rlist = rlist.tail();
		} 
		
		return r;
	}

	public boolean collectIOList(List<ByteBuffer> out) {
		ECons list;
		EObject tail;
		list_loop: for (tail=this; (list = tail.testNonEmptyList()) != null; tail = list.tail()) {
			EObject head = list.head();

			ESmall intval;
			EBinary binary;
			ECons sublist;
			if ((intval = head.testSmall()) != null) {
				ByteArrayOutputStream barr = new ByteArrayOutputStream();

				byte_loop: do {
					int byteValue = intval.value & 0xff;
					if (intval.value != byteValue) {
						return false;
					}

					barr.write(byteValue);

					tail = list.tail();
					if ((list = tail.testNonEmptyList()) != null) {
						head = list.head();
					} else {
						out.add(ByteBuffer.wrap(barr.toByteArray()));
						break list_loop;
					}
				} while ((intval = head.testSmall()) != null);

				out.add(ByteBuffer.wrap(barr.toByteArray()));
			}

			if ((binary = head.testBinary()) != null) {
				binary.collectIOList(out);
			} else if ((sublist = head.testNonEmptyList()) != null) {
				if (!sublist.collectIOList(out)) return false;
			} else if (head.isNil()) {
				// do nothing //
			} else {
				
				System.err.println("head is "+head+"::"+head.getClass());
				
				// not a well-formed iolist
				return false;
			}
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
		for (tail=this; (list = tail.testNonEmptyList()) != null; tail = list.tail()) {
			EObject head = list.head();

			ESmall intval;
			if ((intval = head.testSmall()) != null) {
				try {
					out.addInteger(intval.value);
				} catch (CharCollector.DecodingException e) {
					throw new CharCollector.CollectingException(list);
				}
			} else {
				try {
					head.collectCharList(out);
				} catch (CharCollector.CollectingException e) {
					throw new CharCollector.CollectingException(list.tail().cons(e.restOfInput));
				}
			}
		}

		if (tail.testNumber() != null) {
			// Only nil and binaries are allowed as tail
			throw new CharCollector.InvalidElementException();
		} else tail.collectCharList(out);
	}

}
