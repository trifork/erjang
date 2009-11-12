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
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erjang.EAtom;
import erjang.EBinary;
import erjang.EDouble;
import erjang.EInteger;
import erjang.EList;
import erjang.ERT;
import erjang.EString;
import erjang.ETerm;
import erjang.ETuple;

abstract class Converter<T> {
	abstract ETerm conv(T obj);
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
			ETerm conv(OtpErlangTuple obj) {
				ETerm[] vals = new ETerm[obj.arity()];
				for (int i = 0; i < obj.arity(); i++) {
					vals[i] = convert(obj.elementAt(i));
				}
				return ETuple.make(vals);
			}
		});

		add(OtpErlangList.class, new Converter<OtpErlangList>() {
			@Override
			ETerm conv(OtpErlangList obj) {
				ETerm tail = obj.getLastTail() == null ? EList.EMPTY
						: convert(obj.getLastTail());
				for (int i = obj.arity() - 1; i >= 0; i--) {
					tail = ERT.cons(convert(obj.elementAt(i)), tail);
				}
				return tail;
			}
		});

		add(OtpErlangAtom.class, new Converter<OtpErlangAtom>() {
			ETerm conv(OtpErlangAtom obj) {
				return EAtom.intern(obj.atomValue());
			}
		});

		add(OtpErlangLong.class, new Converter<OtpErlangLong>() {
			ETerm conv(OtpErlangLong obj) {
				return EInteger.parseInt(obj.toString());
			}
		});

		add(OtpErlangString.class, new Converter<OtpErlangString>() {
			ETerm conv(OtpErlangString obj) {
				return new EString(obj.stringValue());
			}
		});

		add(OtpErlangDouble.class, new Converter<OtpErlangDouble>() {
			ETerm conv(OtpErlangDouble obj) {
				return new EDouble(obj.doubleValue());
			}
		});

		add(OtpErlangBinary.class, new Converter<OtpErlangBinary>() {
			ETerm conv(OtpErlangBinary obj) {
				return new EBinary(obj.binaryValue());
			}
		});
}

	public static ETerm convert(OtpErlangObject value) {

		Class<? extends OtpErlangObject> c = value.getClass();
		Converter cc = conv.get(c);
		if (cc == null) {
			throw new Error("cannot convert " + c);
		} else {
			return cc.conv(value);
		}
	}

}
