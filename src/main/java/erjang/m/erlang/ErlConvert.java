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

package erjang.m.erlang;

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;

import erjang.BIF;
import erjang.EBinary;
import erjang.EBitString;
import erjang.ECons;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.EString;
import erjang.ETuple;
import erjang.NotImplemented;

/**
 * 
 */
public class ErlConvert {

	@BIF
	public static EObject binary_to_term(EObject bin) {
		throw new NotImplemented();
	}

	@BIF
	public static EBitString term_to_binary(EObject bin) {
		throw new NotImplemented();
	}

	@BIF
	public static ETuple list_to_tuple(EObject obj) {
		ESeq seq;
		if ((seq = obj.testSeq()) == null)
			throw ERT.badarg(obj);
		return ETuple.make(seq.toArray());
	}

	@BIF
	public static EString binary_to_list(EObject obj) {
		EBinary bin = obj.testBinary();
		if (bin == null)
			throw ERT.badarg(obj);
		return EString.fromBinary(bin);
	}

	static private class BARR extends ByteArrayOutputStream {
		EBinary asBinary() {
			return new EBinary(super.buf, 0, super.count);
		}
	}

	@BIF
	public static EBinary iolist_to_binary(EObject list) {
		ECons iol = list.testCons();
		if (iol == null)
			throw ERT.badarg(list);

		if (iol.isNil()) {
			return EBinary.EMPTY;
		}
		
		BARR barr = new BARR();

		while (true) {
			EObject hd = iol.head();

			ESmall sm;
			EBinary bi;
			if ((sm = hd.testSmall()) != null) {
				if (sm.value < 0 || sm.value > 255)
					throw ERT.badarg(list);
				barr.write(sm.value);

			} else if ((bi = hd.testBinary()) != null) {
				bi.writeTo(barr);

			} else {
				throw ERT.badarg(list);
			}

			EObject next = iol.tail();

			if (next.isNil()) {
				break;
			} else if ((iol = next.testCons()) != null) {
				continue;
			} else if ((bi = next.testBinary()) != null) {
				bi.writeTo(barr);
				break;
			} else {
				throw ERT.badarg(list);
			}
		}
		
		return barr.asBinary();
	}

}
