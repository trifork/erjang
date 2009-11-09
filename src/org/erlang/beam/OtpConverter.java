package org.erlang.beam;

import java.util.HashMap;
import java.util.Map;

import org.erlang.EAtom;
import org.erlang.ECons;
import org.erlang.EDouble;
import org.erlang.EInteger;
import org.erlang.EList;
import org.erlang.ERT;
import org.erlang.EString;
import org.erlang.ETerm;
import org.erlang.ETuple;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

abstract class Converter<T> {
	abstract ETerm conv(T obj);
}

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
