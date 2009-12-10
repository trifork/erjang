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


public abstract class ESeq extends ECons {

	public ESeq testWellformedList() {
		return this;
	}	

	public ESeq testSeq() {
		return this;
	}

	@Override
	public abstract ESeq tail();

	public abstract ESeq cons(EObject h);

	public EObject[] toArray() {
		List<EObject> out = new ArrayList<EObject>();
		ESeq curr = this;
		while (curr != ERT.NIL) {
			out.add(curr.head());
			curr = curr.tail();
		}
		return out.toArray(new EObject[out.size()]);
	}

	public int length() {
		int count = 0;
		ESeq curr = this;
		while (curr != ERT.NIL) {
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
		EObject[] val = this.toArray();
		for (int i = 0; i < val.length; i++) {
			result = result.cons(val[i]);
		}
		return result;
	}

}
