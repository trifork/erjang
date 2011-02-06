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
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;


public abstract class ESeq extends ECons {

	public ESeq testSeq() {
		return this;
	}	
	
	public String stringValue() {
		throw ERT.badarg(this);
	}

	@Override
	public abstract ESeq tail();

	public abstract ESeq cons(EObject h);

	public EObject[] toArray() {
		List<EObject> out = new ArrayList<EObject>();
		ESeq curr = this;
		while (!curr.isNil()) {
			out.add(curr.head());
			curr = curr.tail();
		}
		return out.toArray(new EObject[out.size()]);
	}

	public int length() {
		int count = 0;
		ESeq curr = this;
		while (!curr.isNil()) {
			count += 1;
			curr = curr.tail();
		}
		return count;
	}

	/**
	 * @param args
	 * @return
	 */
	public static ESeq fromArray(EObject[] args) {
		ESeq res = ERT.NIL;
		for (int i = args.length-1; i >= 0; i--) {
			res = res.cons(args[i]);
		}
		return res;
	}

	public static ESeq fromArraySkippingNulls(EObject[] args, int start, int end) {
		ESeq res = ERT.NIL;
		for (int i = end-1; i >= start; i--) {
			if (args[i] != null) res = res.cons(args[i]);
		}
		return res;
	}

	public static ESeq fromList(List<? extends EObject> args) {
		ESeq res = ERT.NIL;

		ListIterator<? extends EObject> iter = args.listIterator(args.size());
		while (iter.hasPrevious()) {
			res = res.cons(iter.previous());
		}
		return res;
	}


	/**
	 * @param o1
	 * @return
	 */
	public ESeq cons(int o1) {
		if ((o1&0xff)==o1) {
			return new EBinList((byte)o1, this).testSeq();
		} else {
			return cons(ERT.box(o1));
		}
	}

	/**
	 * @param o1
	 * @return
	 */
	public ESeq cons(double o1) {
		return cons(ERT.box(o1));
	}

	/**
	 * @param o1
	 * @return
	 */
	public ESeq cons(BigInteger o1) {
		return cons(ERT.box(o1));
	}
	
	public EString testString() {
		
		ByteArrayOutputStream barr = new ByteArrayOutputStream();
		
		ESeq list = this;
		while (!list.isNil()) {
			
			EObject head = list.head();
			
			if (head == null) {
				// BAD!
				
				System.err.println("null head in "+list.getClass());
			}
			
			ESmall intval;
			if ((intval = head.testSmall()) == null) {
				return null;
			}
			
			int byteValue = intval.value & 0xff;
			if (intval.value != byteValue) {
				return null;
			}
			
			barr.write( byteValue );
			list = list.tail();
		}
		
		return new EString(barr.toByteArray(), 0);
	}

	/**
	 * @return
	 */
	public ESeq reverse() {
		ESeq result = ERT.NIL;
		if (this.isNil())
			return result;
		
		EObject[] val = this.toArray();
		for (int i = 0; i < val.length; i++) {
			result = result.cons(val[i]);
		}
		return result;
	}


	/**
	 * @param c1
	 * @return
	 */
	public ESeq prepend(ESeq list) {
		
		// first, rlist=lists:reverse(list)
		ESeq rlist = ERT.NIL;
		while (!list.isNil()) {
			rlist = rlist.cons(list.head());
			list = list.tail();
		}

		// then, prepend rlist on this
		ESeq r = this;
		while(!rlist.isNil()) {
			r = r.cons(rlist.head());
			rlist = rlist.tail();
		} 
		
		return r;
	}

	@Override
	public int hashCode() {
		int h = 0;
		ESeq curr = this;
		do {
			h = 31*h + curr.head().hashCode();
			curr = curr.tail();
		} while (curr != ERT.NIL);

		return h;
	}

}
