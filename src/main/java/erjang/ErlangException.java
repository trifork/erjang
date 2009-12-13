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

import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import erjang.beam.EUtil;

// TODO: set proc.execption on catch to last known
public abstract class ErlangException extends RuntimeException {

	private static final EAtom am_java_exception = EAtom
			.intern("java_exception");
	static final EAtom am_error = EAtom.intern("error");
	static final EAtom am_throw = EAtom.intern("throw");
	static final EAtom am_exit = EAtom.intern("exit");
	private EObject reason;

	public abstract EAtom getExClass();
	
	public ErlangException(EObject reason) {
		this.reason = reason;
		//System.err.println(this.getClass().getName() + ": " + getMessage());
	}

	public ErlangException(EObject reason, Throwable cause) {
		super(cause);
		this.reason = reason;
		//System.err.println(this.getClass().getName() + ": " + getMessage());
	}

	public ErlangException(Throwable cause) {
		super(cause);
		this.reason = am_java_exception;
		
		//System.err.println(this.getClass().getName() + ": " + getMessage());
	}

	public EObject reason() {
		return reason;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Throwable#toString()
	 */
	@Override
	public String getMessage() {
		return String.valueOf(reason());
	}

	/**
	 * @return
	 */
	public EObject getCatchValue() {
		return new ExceptionTuple(this);
	}

	/**
	 * 
	 */
	private static final class ExceptionTuple extends EObject {
		/**
		 * 
		 */
		private final ErlangException e;

		/**
		 * @param e
		 */
		private ExceptionTuple(ErlangException e) {
			this.e = e;
		}

		@Override
		public String toString() {
			return testTuple().toString();
		}

		@Override
		int cmp_order() {
			return testTuple().cmp_order();
		}

		@Override
		int compare_same(EObject rhs) {
			return testTuple().compare_same(rhs);
		}

		@Override
		public ETuple testTuple() {
			ETuple2 t = new ETuple2(ERT.EXIT, e.reason);
			return t;
		}
	}

	//
	// the rest of this file is a big hack to reconstruct erlang traces...
	//

	static Map<StackTraceElement, ETuple3> cache = Collections
			.synchronizedMap(new WeakHashMap<StackTraceElement, ETuple3>());

	ESeq getTrace() {
		return decodeTrace(getStackTrace());
	}

	public static ESeq decodeTrace(StackTraceElement[] st) {

		ESeq trace = ERT.NIL;

		for (int i = st.length - 1; i >= 0; i--) {

			StackTraceElement st2 = st[i];

			ETuple3 elem;

			if ((elem = cache.get(st2)) != null) {
				trace = trace.cons(elem);
				continue;
			}

			if ((elem = decodeTraceElem(st2)) != null) {
				trace = trace.cons(elem);
			}

		}

		return trace;
	}

	private static final String ERJANG_MODULES_DOT = "erjang.m.";
	private static final Pattern ENDS_WITH_NUM = Pattern
			.compile(".*(__[0-9]+)$");

	/**
	 * @param st
	 * @return
	 */
	private static ETuple3 decodeTraceElem(StackTraceElement st) {

		String cname = st.getClassName();
		String mname = st.getMethodName();

		EAtom module = null;
		if (cname.startsWith(ERJANG_MODULES_DOT)) {

			int last = cname.lastIndexOf('.');
			module = EAtom.intern(cname.substring(ERJANG_MODULES_DOT.length(),
					last));
		}

		EAtom function = null;
		int arity = -1;
		Matcher matcher = ENDS_WITH_NUM.matcher(mname);
		if (matcher.matches()) {
			String method_name = mname.substring(0, matcher.start(1));

			function = EAtom.intern(EUtil.decodeJavaName(method_name));
			arity = Integer.parseInt(mname.substring(matcher.start(1) + 2));
		}

		if (module != null && function != null && arity != -1) {
			ETuple3 res = new ETuple3();
			res.elem1 = module;
			res.elem2 = function;
			res.elem3 = new ESmall(arity);
			return res;
		}

		Class clazz = null;
		try {
			clazz = Class.forName(cname);
		} catch (ClassNotFoundException e) {
			return null;
		}

		Method m = find_method(clazz, mname);
		if (m == null) return null;
		BIF bif = m.getAnnotation(BIF.class);
		if (bif == null)
			return null;

		return resolve(clazz, m, bif);
	}

	/**
	 * @param c
	 * @param m
	 * @param ann
	 * @return
	 */
	private static ETuple3 resolve(Class<?> c, Method m, BIF ann1) {

		if (ann1 == null)
			return null;

		String module = get_class_module(c);

		String fun = null;
		if (ann1 != null && !("__SELFNAME__".equals(ann1.name()))) {
			fun = ann1.name();
		}

		if (fun == null) {
			fun = m.getName();
		}

		Class<?>[] parameterTypes = m.getParameterTypes();
		int arity = parameterTypes.length;
		if (arity > 0) {
			if (parameterTypes[0].equals(EProc.class))
				arity -= 1;
		}

		if (module != null && fun != null) {
			ETuple3 res = new ETuple3();
			res.elem1 = EAtom.intern(module);
			res.elem2 = EAtom.intern(fun);
			res.elem3 = ESmall.make(arity);
			return res;
		} else {
			return null;
		}

	}

	/**
	 * @param c
	 * @return
	 */
	private static String get_class_module(Class<?> c) {
		Module m = c.getAnnotation(Module.class);
		if (m != null) {
			return m.value();
		} else {
			String cname = c.getName();
			if (cname.startsWith(ERJANG_MODULES_DOT)) {

				int last = cname.lastIndexOf('.');
				return cname.substring(ERJANG_MODULES_DOT.length(), last);
			}
		}
		return null;
	}

	/**
	 * @param c
	 * @param mname
	 * @return
	 */
	private static Method find_method(Class<?> c, String mname) {

		Method[] methods = c.getDeclaredMethods();
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(mname))
				return methods[i];
		}
		return null;
	}

	/**
	 * 
	 */
	private static final EAtom UNKNOWN = EAtom.intern("unknown");

	private EObject erl_value(Object val) {
		if (val instanceof EObject)
			return (EObject) val;
		if (val instanceof String)
			return EString.fromString((String) val);
		if (val instanceof Integer)
			return ESmall.make(((Integer) val).intValue());
		throw new Error();
	}

	/**
	 * @return
	 */
	public ETuple3 getTryValue() {
		ETuple3 result = new ETuple3();
		result.elem1 = getExClass();
		result.elem2 = reason;
		result.elem3 = getTrace();
		return result;
	}


}
