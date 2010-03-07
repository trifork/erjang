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
import java.io.IOException;
import java.math.BigInteger;

import erjang.BIF;
import erjang.EAtom;
import erjang.EBinary;
import erjang.EBitString;
import erjang.ECons;
import erjang.EInputStream;
import erjang.EInteger;
import erjang.EDouble;
import erjang.EObject;
import erjang.EOutputStream;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.EString;
import erjang.ETuple;
import erjang.ERef;
import erjang.ErlangError;
import erjang.NotImplemented;

/**
 * 
 */
public class ErlConvert {

	@BIF
	public static EObject binary_to_term(EObject arg) {
		EBinary bin;
		if ((bin=arg.testBinary()) == null) throw ERT.badarg(arg);
		EInputStream in = bin.getInputStream();
		try {
			EObject val = in.read_any();
			//System.out.println("DECODED:"+val);
			return val;
		} catch (IOException e) {
			throw new ErlangError(ERT.am_badarg, e, arg);
		}
	}

	@BIF
	public static EBinary term_to_binary(EObject obj) {
		EOutputStream eos = new EOutputStream();
		eos.write_any(obj);
		return eos.getBinaryContent();		
	}

	@BIF
	public static EAtom list_to_existing_atom(EObject obj) {
		EString seq;
		if ((seq = obj.testString()) == null)
			throw ERT.badarg(obj);
		
		return EAtom.existing_atom(seq);
	}
	
	@BIF
	public static EInteger list_to_integer(EObject obj) {
		EString seq;
		if ((seq = obj.testString()) == null)
			throw ERT.badarg(obj);
		
		try {
			
			BigInteger val = new BigInteger(seq.stringValue());
			return ERT.box(val);
			
		} catch (NumberFormatException e) {
			throw ERT.badarg(obj);
		}
		
	}

	@BIF
	public static EDouble list_to_float(EObject obj) {
		EString seq;
		if ((seq = obj.testString()) == null)
			throw ERT.badarg(obj);

		try {
			double val = Double.parseDouble(seq.stringValue());
			return ERT.box(val);
		} catch (NumberFormatException e) {
			throw ERT.badarg(obj);
		}
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
		return EString.make(bin);
	}

	static private class BARR extends ByteArrayOutputStream {
		EBinary asBinary() {
			return new EBinary(super.buf, 0, super.count);
		}
	}

	@BIF
	public static EBitString iolist_to_binary(EObject list) {
		EBitString bin = list.testBitString();
		if (bin != null)
			return bin;

		ECons iol = list.testCons();
		if (iol == null)
			throw ERT.badarg(list);

		if (iol.isNil()) {
			return EBinary.EMPTY;
		}
		
		BARR barr = new BARR();

		collectList(list, iol, barr);
		
		return barr.asBinary();
	}

	private static void collectList(EObject list, ECons iol, BARR barr) {
		EObject tail;
		ECons cons;
		for (tail=iol; (cons = tail.testNonEmptyList()) != null; tail = cons.tail()) {
			EObject hd = cons.head();

			ESmall sm;
			EBinary bi;
			ECons co;
			if ((sm = hd.testSmall()) != null) {
				if (sm.value < 0 || sm.value > 255)
					throw ERT.badarg(list);
				barr.write(sm.value);
			} else if ((bi = hd.testBinary()) != null) {
				bi.writeTo(barr);
			} else if ((co = hd.testNonEmptyList()) != null) {
				collectList(list, co, barr);
			} else if (hd.isNil()) {
			} else {
				throw ERT.badarg(list);
			}
		}

		// Process tail:
		EBinary bi;
		if ((bi = tail.testBinary()) != null) {
			bi.writeTo(barr);
		} else if (! tail.isNil()) {
			throw ERT.badarg(list);
		}
	}

	@BIF
	public static EObject tuple_to_list(EObject tup) {
		ETuple t;
		if ((t=tup.testTuple()) == null) { throw ERT.badarg(tup); }

		ESeq res = ERT.NIL;
		for (int i = t.arity(); i > 0; i--) {
			res = res.cons(t.elm(i));
		}
		
		return res;
	}

	@BIF
	public static EString ref_to_list(EObject obj) {
		ERef ref = obj.testReference();
		if (ref == null)
			throw ERT.badarg(obj);
		return new EString(ref.toString());
	}

}
