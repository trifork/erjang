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
import java.security.PrivilegedAction;
import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;


public class ErlangException extends RuntimeException {

	/**
	 * 
	 */
	private static final String ERJANG_MODULES_DOT = "erjang.modules.";
	private static final EAtom UNKNOWN = EAtom.intern("unknown");
	public final EAtom reason;
	ESeq trace;

	public ErlangException(EAtom reason, Throwable cause) {
		super(reason.getName(), cause);
		this.reason = reason;
		this.trace = init_trace();
	}

	public ErlangException(EAtom reason, Object... args) {
		super(reason.getName());
		this.reason = reason;
		this.trace = init_trace();
		this.trace = fix_args(trace, args);
	}

	public ErlangException(EAtom reason, Throwable cause, Object... args) {
		super(reason.getName(), cause);
		this.reason = reason;
		this.trace = init_trace();
		this.trace = fix_args(trace, args);
	}

	public ErlangException(EAtom reason) {
		super(reason.getName());
		this.reason = reason;
		this.trace = init_trace();
	}

	private ESeq fix_args(ESeq trace, Object... args) {
		if (trace == ENil.NIL) {
			trace = trace.cons(ETuple.make(UNKNOWN, UNKNOWN, listify(args)));
		} else {
			ETuple t = trace.head().testTuple();
			trace = trace.tail().cons(
					ETuple.make(t.elm(1), t.elm(2), listify(args)));
		}
		return trace;
	}

	/**
	 * @param e
	 */
	public ErlangException(Throwable e) {
		super(e);

		reason = EAtom.intern("error");
	}

	private ESeq listify(Object[] args) {
		ESeq res = ESeq.NIL;
		for (int i = args.length - 1; i >= 0; i--) {
			EObject h = erl_value(args[i]);
			res = res.cons(h);
		}
		return res;
	}

	private EObject erl_value(Object val) {
		if (val instanceof EObject)
			return (EObject) val;
		if (val instanceof String)
			return EString.fromString((String) val);
		if (val instanceof Integer)
			return EInt32.make(((Integer) val).intValue());
		throw new Error();
	}

	//
	// the rest of this file is a big hack to reconstruct erlang traces...
	//

	Map<StackTraceElement, ETuple> cache = Collections
			.synchronizedMap(new WeakHashMap<StackTraceElement, ETuple>());

	private static XSM xsm = new XSM();

	ESeq init_trace() {
		ESeq list = ENil.NIL;
		Class<?>[] class_trace = null;
		StackTraceElement[] stack_trace = this.getStackTrace();

		for (int i = stack_trace.length - 1; i <= 0; i++) {
			StackTraceElement elm = stack_trace[i];

			ETuple t = cache.get(elm);
			if (t != null) {
				list = list.cons(t);
				continue;
			}

			if (class_trace == null) {
				class_trace = xsm.context();
			}

			String cname = elm.getClassName();
			String mname = elm.getMethodName();

			Class<?> c = find_class(class_trace, cname);
			if (c == null)
				continue;

			Method m = find_method(c, mname);
			if (m == null)
				continue;

			ETuple spec = resolve(c, m, m.getAnnotation(BIF.class), m
					.getAnnotation(ErlFun.class));
			cache.put(elm, spec);

			list = list.cons(spec);
		}

		return list;
	}

	/**
	 * @param c
	 * @param m
	 * @param ann
	 * @return
	 */
	private ETuple resolve(Class<?> c, Method m, BIF ann1, ErlFun ann2) {

		String module;
		if (ann2 == null) {
			module = get_class_module(c);
		} else if (ann2 != null && "__SELFNAME__".equals(ann2.module())) {
			module = get_class_module(c);
		} else {
			module = ann2.module();
		}

		String fun = null;
		if (ann1 != null && !("__SELFNAME__".equals(ann1.name()))) {
			fun = ann1.name();
		}

		if (fun == null && ann2 != null
				&& !("__SELFNAME__".equals(ann2.name()))) {
			fun = ann2.name();
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

		return ETuple.make(EAtom.intern(module), EAtom.intern(fun), EInt32
				.make(arity));
	}

	/**
	 * @param c
	 * @return
	 */
	private String get_class_module(Class<?> c) {
		Module m = c.getAnnotation(Module.class);
		if (m != null) {
			return m.value();
		} else {
			String cname = c.getName();
			if (cname.startsWith(ERJANG_MODULES_DOT)) {
				return cname.substring(ERJANG_MODULES_DOT.length());
			} else {
				return cname; // what else?
			}
		}
	}

	/**
	 * @param c
	 * @param mname
	 * @return
	 */
	private Method find_method(Class<?> c, String mname) {

		Method[] methods = c.getDeclaredMethods();
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(mname))
				return methods[i];
		}
		return null;
	}

	/**
	 * @param classTrace
	 * @param cname
	 * @return
	 */
	private Class<?> find_class(Class<?>[] trace, String cname) {
		for (int i = 0; i < trace.length; i++) {
			if (cname.equals(trace[i].getName()))
				return trace[i];
		}

		return null;
	}

	static class XSM extends SecurityManager {
		Class<?>[] context() {
			return java.security.AccessController
					.doPrivileged(new PrivilegedAction<Class<?>[]>() {
						public Class<?>[] run() {
							return XSM.this.getClassContext();
						}
					});
		}
	}

}
