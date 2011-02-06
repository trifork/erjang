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
import erjang.EExternal;
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
import erjang.ETuple2;
import erjang.ErlangError;
import erjang.NotImplemented;
import erjang.driver.IO;

/**
 * 
 */
public class ErlConvert {

	/**
	 * 
	 */
	private static final ESmall PLUS_SIGN = ERT.box((int)'+');
	private static final EAtom am_compressed = EAtom.intern("compressed");
	private static final EAtom am_minor_version = EAtom.intern("minor_version");

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
		eos.write(EExternal.versionTag);
		eos.write_any(obj);
		return eos.getBinaryContent();		
	}
	
	@BIF
	public static ESmall external_size(EObject obj) {
		EBinary bin = term_to_binary(obj);
		return ERT.box(bin.byteSize());
	}

	@BIF static ETuple2 split_binary(EObject bin, EObject idx) {
		EBitString b;
		ESmall i;
		if ((b=bin.testBitString()) == null
			|| ((i=idx.testSmall()) == null)
			|| i.value < 0
			|| i.value > b.byteSize()) {
			throw ERT.badarg(bin);
		}

		long split = i.value*8;
		return new ETuple2(b.substring(0, split),
						   b.substring(split));

		/* more efficient, but works only for EBinary */

		// return new ETuple2(b.sub_binary(0, i.value), 
		//    b.sub_binary(i.value, b.byteSize()-i.value));		
	}

	@BIF
	public static ESeq fun_to_list(EObject fun) {
		return EString.fromString(fun.toString());
	}

	@BIF
	public static EBinary term_to_binary(EObject obj, EObject spec) {
		int compression = 0;
		int minor = 0;
		
		ESeq opts;
		if ((opts=spec.testSeq()) == null) {
			throw ERT.badarg(obj, spec);
		}
		
		while (!opts.isNil()) {
			
			EObject val = opts.head();
			ETuple2 tup;
			if (val == am_compressed) {
				compression = 6;
			} else if ((tup=ETuple2.cast(val)) != null) {
				if (tup.elem1 == am_compressed) {
					ESmall sm;
					if ((sm=tup.elem2.testSmall()) != null) {
						compression = sm.value;
					} else {
						throw ERT.badarg(obj, spec);
					}
				} else if (tup.elem1 == am_minor_version) {
					ESmall sm;
					if ((sm=tup.elem2.testSmall()) != null) {
						minor = sm.value;
					} else {
						throw ERT.badarg(obj, spec);
					}
				}
			} else {
				throw ERT.badarg(obj, spec);
			}
			
			opts = opts.tail();
		}
		
		if (compression < 0 || compression > 9 || minor < 0 || minor > 1) {
			throw ERT.badarg(obj, spec);
		}
		
		if (minor == 0) {
			throw new NotImplemented("encoding with minor_version=0");
		}
		
		EOutputStream eos = new EOutputStream();
		if (compression != 0) {
			eos.write_compressed(obj, compression);
		} else {
			eos.write_any(obj);
		}
		return eos.getBinaryContent();		
	}

	@BIF
	public static EAtom list_to_existing_atom(EObject obj) {
		EString seq;
		if ((seq = obj.testString()) == null)
			throw ERT.badarg(obj);
		
		return EAtom.existing_atom(seq.stringValue());
	}
	
	@BIF
	public static EInteger list_to_integer(EObject obj) {
		EString seq;
		if ((seq = obj.testString()) == null)
			throw ERT.badarg(obj);

		// remove leading +
		if (!seq.isNil()) {
			if (seq.head().equalsExactly(PLUS_SIGN)) {
				seq = seq.tail().testString();
				
				if (seq == null) {
					throw ERT.badarg(obj);
				}
			}
		}		

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
		
		String string = seq.stringValue();
		if (string.length() == 0 
			|| string.charAt(0) == '.'
			|| string.charAt(string.length()-1) == '.'
			|| string.indexOf('.') == -1) {
			throw ERT.badarg(obj);
		}

		try {
			double val = Double.parseDouble(string);
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

	@BIF
	public static EString binary_to_list(EObject obj, EObject start, EObject stop) {
		EBinary bin = obj.testBinary();
		ESmall s = start.testSmall();
		ESmall e = stop.testSmall();
		if (bin == null || s==null || e==null)
			throw ERT.badarg(obj,start,stop);

		int idx0start = s.value-1;
		int len = e.value-s.value+1;

		if (idx0start < 0 || len < 0 || (idx0start + len) > bin.byteSize())
			throw ERT.badarg(obj, start, stop);

		return EString.make(bin, idx0start, len);
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
				try {
					bi.writeTo(barr);
				} catch (IOException e) {
					// should not happen, barr is an byte array stream
					throw new InternalError();
				}
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
			try {
				bi.writeTo(barr);
			} catch (IOException e) {
				throw new InternalError("should not happen");
			}
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
			EObject e = t.elm(i);
			res = res.cons(e==null?ERT.NIL:e);
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
	
	@BIF
	public static EBinary atom_to_binary(EObject obj, EObject enc) {
		EAtom am = obj.testAtom();
		EAtom en = enc.testAtom();
		if (am == null || en == null) {
			throw ERT.badarg(obj, enc);
		}

		byte[] data;
		String str = am.getName();
		if (en == ERT.am_latin1) {
			data = str.getBytes(IO.ISO_LATIN_1); 
		} else if (en == ERT.am_utf8 || en == ERT.am_unicode) {
			data = str.getBytes(IO.UTF8); 			
		} else {
			throw ERT.badarg(obj, enc);			
		}
		
		return new EBinary(data);
	}

}
