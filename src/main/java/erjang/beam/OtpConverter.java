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

package erjang.beam;

import java.util.HashMap;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangBitstr;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erjang.EAtom;
import erjang.EBinary;
import erjang.EBitString;
import erjang.EObject;
import erjang.ERT;
import erjang.EString;
import erjang.ETuple;

abstract class Converter<T> {
	abstract EObject conv(T obj);
}

/**
 * Convert terms to/from jinterface's classes.
 * 
 * Until I rewrite jinterface, this will have to do.  
 * 
 * @author krab
 */
public class OtpConverter {

	static Map<Class, Converter> conv = new HashMap<Class, Converter>();

	static <T> void add(Class<T> c, Converter<T> co) {
		conv.put(c, co);
	}

	static {
		add(OtpErlangTuple.class, new Converter<OtpErlangTuple>() {
			EObject conv(OtpErlangTuple obj) {
				EObject[] vals = new EObject[obj.arity()];
				for (int i = 0; i < obj.arity(); i++) {
					vals[i] = convert(obj.elementAt(i));
				}
				return ETuple.make(vals);
			}
		});

		add(OtpErlangList.class, new Converter<OtpErlangList>() {
			@Override
			EObject conv(OtpErlangList obj) {
				EObject tail = obj.getLastTail() == null ? ERT.NIL
						: convert(obj.getLastTail());
				for (int i = obj.arity() - 1; i >= 0; i--) {
					tail = ERT.cons(convert(obj.elementAt(i)), tail);
				}
				return tail;
			}
		});

		add(OtpErlangAtom.class, new Converter<OtpErlangAtom>() {
			EObject conv(OtpErlangAtom obj) {
				return EAtom.intern(obj.atomValue());
			}
		});

		add(OtpErlangLong.class, new Converter<OtpErlangLong>() {
			EObject conv(OtpErlangLong obj) {
				return (obj.isLong())
					? ERT.box(obj.longValue())
					: ERT.box(obj.bigIntegerValue());
			}
		});

		add(OtpErlangString.class, new Converter<OtpErlangString>() {
			EObject conv(OtpErlangString obj) {
				return new EString(obj.stringValue());
			}
		});

		add(OtpErlangDouble.class, new Converter<OtpErlangDouble>() {
			EObject conv(OtpErlangDouble obj) {
				return ERT.box(obj.doubleValue());
			}
		});

		add(OtpErlangBinary.class, new Converter<OtpErlangBinary>() {
			EObject conv(OtpErlangBinary obj) {
				return new EBinary(obj.binaryValue());
			}
		});

		add(OtpErlangBitstr.class, new Converter<OtpErlangBitstr>() {
			EObject conv(OtpErlangBitstr obj) {
				return EBitString.make(obj.binaryValue(), 0, obj.size(), obj.pad_bits() );
			}
		});

	
	}

	public static EObject convert(OtpErlangObject value) {

		Class<? extends OtpErlangObject> c = value.getClass();
		Converter cc = conv.get(c);
		if (cc == null) {
			throw new Error("cannot convert " + c);
		} else {
			return cc.conv(value);
		}
	}

}
