/**
 * 
 */
package erjang.m.java;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import junit.extensions.TestDecorator;

import kilim.Mailbox;
import kilim.Pausable;
import erjang.EAtom;
import erjang.EBinary;
import erjang.EBitString;
import erjang.ECons;
import erjang.EDouble;
import erjang.EFun;
import erjang.EFunHandler;
import erjang.EInteger;
import erjang.EList;
import erjang.ENil;
import erjang.ENumber;
import erjang.EObject;
import erjang.EProc;
import erjang.EPseudoTerm;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.EString;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ErlangError;
import erjang.ErlangException;
import erjang.driver.IO;
import erjang.m.erlang.ErlProc;

public class JavaObject extends EPseudoTerm {

	public JavaObject testJavaObject() {
		return this;
	}

	static final EAtom am_none = EAtom.intern("none");
	static final EAtom am_java_object = EAtom.intern("java_object");
	static final EAtom am_badaccess = EAtom.intern("badaccess");
	static final EAtom am_badfun = EAtom.intern("badfun");
	static final EAtom am_null_pointer_exception = EAtom
			.intern("null_pointer_exception");
	protected static final EAtom am_erlang = EAtom.intern("erlang");
	protected static final EAtom am_apply = EAtom.intern("apply");

	final Object real_object;
	final EProc owner;
	
	public Object realObject() {
		return real_object;
	}

	@Override
	public int hashCode() {
		return real_object.hashCode();
	}
	//TODO: equality operators

	@Override
	public EBinary testBinary() {
		if (real_object instanceof byte[]) {
			return new EBinary((byte[]) real_object);
		}
		if (real_object instanceof String) {
			return new EBinary(((String) real_object).getBytes(IO.UTF8));
		}
		return null;
	}

	@Override
	public EBitString testBitString() {
		EBinary bi;
		if ((bi=testBinary()) != null) {
			return bi;
		}
		
		if (testCons() == null
				&& testNumber() == null
				&& testTuple() == null
				&& testAtom() == null
				&& testHandle() == null
				)
			return EBinary.EMPTY;
		
		return null;
		
	}
	
	@Override
	public ENil testNil() {
		if (real_object == null)
			return ERT.NIL;
		if (testSeq() == ERT.NIL)
			return ERT.NIL;
		return null;
	}

	@Override
	public EFun testFunction() {
		EFun fun;

		if ((fun = testFunction2(0)) != null) {
			return fun;
		}

		if ((fun = testFunction2(1)) != null) {
			return fun;
		}

		return null;
	}

	@Override
	public EFun testFunction2(int nargs) {

		/** a java.lang.Runnable can be used as a function of 0 arguments */
		if ((nargs == 0) && (real_object instanceof Runnable)) {
			final Runnable r = (Runnable) real_object;
			return EFun.get_fun_with_handler(0, new EFunHandler() {
				@Override
				public EObject invoke(EProc proc, EObject[] args)
						throws Pausable {
					if (proc != owner)
						throw new ErlangError(ERT.am_badfun, args);

					r.run();
					return ERT.am_ok;
				}
			}, getClass().getClassLoader());

		}

		/*
		 * a java.util.Map can be used as a function with 1 argument to get a
		 * value
		 */
		if ((nargs == 1) && (real_object instanceof java.util.Map<?, ?>)) {
			final java.util.Map<?, ?> r = (java.util.Map<?, ?>) real_object;
			return EFun.get_fun_with_handler(0, new EFunHandler() {
				@Override
				public EObject invoke(EProc self, EObject[] args)
						throws Pausable {
					if (self != owner)
						throw new ErlangError(ERT.am_badfun, args);

					Object key = JavaObject.unbox(self, Object.class, args[0]);
					if (r.containsKey(key)) {
						return new ETuple2(args[0], JavaObject.box(self, r
								.get(key)));
					} else {
						return am_none;
					}
				}
			}, getClass().getClassLoader());

		}

		return null;
	}
	
	@Override
	public ECons testCons() {
		return testSeq();
	}

	@Override
	public ECons testNonEmptyList() {
		ESeq seq = testSeq();
		if (seq == null || seq.isNil())
			return null;
		return seq;
	}

	@Override
	public EAtom testBoolean() {
		if (real_object == Boolean.TRUE)
			return ERT.TRUE;
		if (real_object == Boolean.FALSE)
			return ERT.FALSE;
		return null;
	}

	@Override
	public EString testString() {
		if (real_object instanceof String) {
			return EString.fromString((String) real_object);
		}

		ESeq seq;
		if ((seq = testSeq()) != null) {
			return seq.testString();
		}

		return null;
	}

	@Override
	public EAtom testAtom() {
		if (real_object instanceof String) {
			String s = (String) real_object;
			return EAtom.existing_atom(s);
		}
		if (real_object instanceof Class) {
			return EAtom.intern(((Class) real_object).getName());
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public ESeq testSeq() {
		if (real_object instanceof CharSequence) {
			return JavaCharSeq.box(owner, (CharSequence) real_object, 0);
		}

		if (real_object instanceof Iterable<?>) {
			Iterable<?> it = (Iterable<?>) real_object;
			return JavaIterator.box(owner, it.iterator());
		}

		if (real_object instanceof Iterator<?>) {
			return JavaIterator.box(owner, ((Iterator<?>) real_object));
		}

		if (real_object instanceof Map<?, ?>) {
			return JavaMapIterator.box(owner, ((Map) real_object).entrySet()
					.iterator());
		}

		if (real_object != null && real_object.getClass().isArray()) {
			return JavaArray.box(owner, real_object, 0);
		}

		return null;
	}

	@Override
	public ESmall testSmall() {
		EInteger iv;
		if ((iv = testInteger()) != null) {
			return iv.testSmall();
		}

		return null;
	}
	
	@Override
	public ENumber testNumber() {

		EInteger i;
		if ((i = testInteger()) != null)
				return i;

		EDouble d;
		if ((d = testFloat()) != null)
				return d;

		return null;
	}

	@Override
	public EInteger testInteger() {

		if (real_object instanceof BigInteger) {
			return ERT.box(((BigInteger) real_object));
		}

		if (real_object instanceof Integer) {
			return ERT.box(((Integer) real_object).intValue());
		}

		if (real_object instanceof Short) {
			return ERT.box(((Short) real_object).shortValue());
		}

		if (real_object instanceof Character) {
			return ERT.box(((Character) real_object).charValue());
		}

		if (real_object instanceof Byte) {
			return ERT.box(((Byte) real_object).byteValue());
		}

		if (real_object instanceof Long) {
			return ERT.box(((Long) real_object).longValue());
		}

		return null;
	}

	@Override
	public EDouble testFloat() {
		if (real_object instanceof Double) {
			return new EDouble(((Double) real_object).doubleValue());
		}
		return null;
	}

	public JavaObject(EProc self, Object object) {
		real_object = object;
		owner = self;
	}

	public String toString() {
		return String.valueOf(real_object);
	};

	public static EObject box(EProc self, Object object) {
		if (object instanceof EObject) {
			return (EObject) object;
		}
		return new JavaObject(self, object);
	}

	public static Object[] convert_args(EProc self, Class<?>[] arg_types,
			ESeq arg_seq) throws IllegalArgumentException {
		Object[] out = new Object[arg_types.length];

		ESeq as_iter = arg_seq;
		for (int i = 0; i < arg_types.length; i++) {
			out[i] = JavaObject.unbox(self, arg_types[i], as_iter.head());
			as_iter = as_iter.tail();
		}

		return out;
	}

	public static Object[] convert_args(EProc self, Class<?>[] arg_types,
			EObject[] args) throws IllegalArgumentException {
		Object[] out = new Object[arg_types.length];

		for (int i = 0; i < arg_types.length; i++) {
			out[i] = JavaObject.unbox(self, arg_types[i], args[i]);
		}

		return out;
	}

	/** this logic is used to map an EObject back to a Java object */
	static Object unbox(final EProc self, Class<?> type, EObject val)
			throws IllegalArgumentException {
		if (val instanceof JavaObject) {
			JavaObject jo = (JavaObject) val;
			if (type.isInstance(jo.real_object)) {
				return jo.real_object;
			} else {
				throw new IllegalArgumentException("cannot convert "
						+ val.getClass().getName() + " to " + type);
			}
		}

		if (type == Object.class || type.isInstance(val)) {
			// TODO: give each erlang term a "natural" conversion to
			// java.lang.Object
			return val;
		}

		if (type == Void.class || type == void.class) {
			return null;
		}
		
		/** unbox to array type */
		if (type.isArray()) {
			ESeq seq;
			if ((seq = val.testSeq()) != null) {
				int length = seq.length();
				Class<?> componentType = type.getComponentType();
				Object arr = Array.newInstance(componentType, length);

				int index = 0;
				while (!seq.isNil()) {
					Object value = JavaObject.unbox(self, componentType, seq
							.head());
					Array.set(arr, index++, value);
					seq = seq.tail();
				}

				return arr;
			}

			ETuple tup;
			if ((tup = val.testTuple()) != null) {
				int length = tup.arity();
				Class<?> componentType = type.getComponentType();
				Object arr = Array.newInstance(componentType, length);

				for (int index = 0; index < length; index++) {
					Object value = JavaObject.unbox(self, componentType, tup
							.elm(index + 1));
					Array.set(arr, index, value);
				}

				return arr;

			}
		}

		Mapper mapper = type_mapper.get(type);
		if (mapper != null) {
			return mapper.map(val);
		}

		final EFun ifun;
		if (type.isInterface() && (ifun = val.testFunction2(3)) != null) {

			final ClassLoader loader = JavaObject.class
					.getClassLoader();
			return 
			java.lang.reflect.Proxy.newProxyInstance(loader, new Class[] { type }, new InvocationHandler() {
						@Override
						public Object invoke(Object proxy, final Method method, final Object[] args)
								throws Throwable {
							
							final Mailbox<Object> reply = new Mailbox<Object>(1);
							
							EFun job = EFun.get_fun_with_handler(0, new EFunHandler() {
								@Override
								public EObject invoke(EProc proc, EObject[] _) throws Pausable {

									EObject aa = JavaObject.box(proc, args);
									EObject at = JavaObject.box(proc, method.getParameterTypes());
									
									EObject[] call_args = new EObject[] {
										EAtom.intern(method.getName()), at, aa
									};
									
									EObject result = ifun.invoke(proc, call_args);
									
									Object jresult = JavaObject.unbox(proc,
											method.getReturnType(), 
											result);
									
									if (method.getReturnType() == void.class) {
										reply.put(ERT.am_ok);
									} else {
										reply.put(  jresult );
									}
									
									return ERT.am_ok;
								}
							}, loader);

							EProc proc = new EProc(self.group_leader(), 
									am_erlang, am_apply, EList.make(job, ERT.NIL));
							ERT.run(proc);
							
							return reply.getb();
						}
					});
		}

		throw new IllegalArgumentException("cannot convert "
				+ val.getClass().getName() + " to " + type);
	}

	static Map<Class<?>, Mapper> type_mapper = new HashMap<Class<?>, Mapper>();

	interface Mapper {
		Object map(EObject val);
	}

	static {
		Mapper string_mapper = new Mapper() {
			public Object map(EObject val) {

				EString str;
				if ((str = val.testString()) != null)
					return str.stringValue();

				EAtom am;
				if ((am = val.testAtom()) != null)
					return am.getName();

				throw new IllegalArgumentException();
			}
		};
		type_mapper.put(String.class, string_mapper);
		type_mapper.put(CharSequence.class, string_mapper);

		Mapper byte_mapper = new Mapper() {

			public Object map(EObject val) {
				ESmall num;
				// special case: allow converting values -127 ... 155.
				if ((num = val.testSmall()) != null
						&& num.value >= Byte.MIN_VALUE && num.value <= 255) {
					return new Byte((byte) num.intValue());
				}
				throw new IllegalArgumentException("cannot convert " + val
						+ " to byte");
			}
		};
		type_mapper.put(Byte.class, byte_mapper);
		type_mapper.put(byte.class, byte_mapper);

		Mapper short_mapper = new Mapper() {

			public Object map(EObject val) {
				ESmall num;
				if ((num = val.testSmall()) != null
						&& num.value >= Short.MIN_VALUE
						&& num.value <= Short.MAX_VALUE) {
					return new Short((byte) num.intValue());
				}
				throw new IllegalArgumentException("cannot convert " + val
						+ " to short");
			}
		};
		type_mapper.put(Short.class, short_mapper);
		type_mapper.put(short.class, short_mapper);

		Mapper int_mapper = new Mapper() {

			public Object map(EObject val) {
				ESmall num;
				if ((num = val.testSmall()) != null) {
					return new Integer(num.intValue());
				}
				throw new IllegalArgumentException("cannot convert " + val
						+ " to int");
			}
		};
		type_mapper.put(Integer.class, int_mapper);
		type_mapper.put(int.class, int_mapper);

		Mapper long_mapper = new Mapper() {

			public Object map(EObject val) {
				EInteger num;
				if ((num = val.testInteger()) != null) {
					return new Long(num.longValue());
				}
				throw new IllegalArgumentException("cannot convert " + val
						+ " to long");
			}
		};
		type_mapper.put(Long.class, long_mapper);
		type_mapper.put(long.class, long_mapper);

		Mapper double_mapper = new Mapper() {

			public Object map(EObject val) {
				ENumber num;
				if ((num = val.testNumber()) != null) {
					return new Double(num.doubleValue());
				}
				throw new IllegalArgumentException("cannot convert " + val
						+ " to double");
			}
		};
		type_mapper.put(Double.class, double_mapper);
		type_mapper.put(double.class, double_mapper);

		Mapper float_mapper = new Mapper() {

			public Object map(EObject val) {
				ENumber num;
				if ((num = val.testNumber()) != null) {
					return new Float(num.doubleValue());
				}
				throw new IllegalArgumentException("cannot convert " + val
						+ " to float");
			}
		};
		type_mapper.put(Float.class, float_mapper);
		type_mapper.put(float.class, float_mapper);

		Mapper bool_mapper = new Mapper() {

			public Object map(EObject val) {

				EAtom bool;
				if ((bool = val.testBoolean()) != null) {
					if (bool == ERT.TRUE)
						return Boolean.TRUE;
					if (bool == ERT.FALSE)
						return Boolean.FALSE;
				}

				throw new IllegalArgumentException();
			}
		};
		type_mapper.put(boolean.class, bool_mapper);
		type_mapper.put(Boolean.class, bool_mapper);
	}

	/**
	 * return a fun that will call a Java method; used when calling
	 * Object:doThis(a,b,c)
	 */
	public EFun resolve_fun(final EAtom f, final int arity) {

		// TODO: we can make this much much faster!

		return EFun.get_fun_with_handler(arity,

		new EFunHandler() {
			@Override
			public EObject invoke(EProc proc, EObject[] args) throws Pausable {

				if (real_object == null) {
					throw new ErlangError(am_null_pointer_exception);
				}

				Method[] methods = real_object.getClass().getMethods();

				return choose_and_invoke_method(proc, real_object, f, args,
						methods, false);
			}
		}, JavaObject.class.getClassLoader());

	}

	public static EObject choose_and_invoke_method(EProc self, Object target,
			final EAtom method_name, EObject[] args, Method[] methods,
			boolean static_only) {
		for (int i = 0; i < methods.length; i++) {
			// TODO: handle reflective invocation of Pausable methods

			Method m = methods[i];
			int modifier = m.getModifiers();
			boolean is_static = (Modifier.STATIC & modifier) == Modifier.STATIC;
			if (is_static != static_only)
				continue;

			if (!m.getName().equals(method_name.getName()))
				continue;
			Class<?>[] pt = m.getParameterTypes();
			if (pt.length != args.length)
				continue;
			Object[] a;
			try {
				a = convert_args(self, pt, args);
			} catch (IllegalArgumentException e) {
				// TODO: make this a null check
				continue;
			}

			// point-of-no-return
			Object result;
			try {

				result = m.invoke(target, a);
			} catch (IllegalArgumentException e) {
				throw ERT.badarg(args);
			} catch (IllegalAccessException e) {
				throw new ErlangError(am_badaccess, args);
			} catch (InvocationTargetException e) {
				Throwable te = e.getTargetException();
				if (te instanceof ErlangException) {
					throw (ErlangException) te;
				} else {
					ETuple reason = ETuple.make(EAtom.intern(te.getClass()
							.getName()), EString.fromString(te.getMessage()));
					throw new ErlangError(reason, args);
				}
			}

			if (m.getReturnType() == Void.TYPE) {
				return ERT.am_ok;
			}

			return JavaObject.box(self, result);
		}

		throw new ErlangError(am_badfun, args);
	}

	public static EObject choose_and_invoke_constructor(EProc self,
			EObject[] args, Constructor<?>[] cons) {
		for (int i = 0; i < cons.length; i++) {
			// TODO: handle reflective invocation of Pausable methods

			Constructor<?> m = cons[i];

			Class<?>[] pt = m.getParameterTypes();
			if (pt.length != args.length)
				continue;
			Object[] a;
			try {
				a = convert_args(self, pt, args);
			} catch (IllegalArgumentException e) {
				// TODO: make this a null check
				continue;
			}

			// point-of-no-return
			Object result;
			try {

				result = m.newInstance(a);
			} catch (InstantiationException e) {
				throw ERT.badarg(args);
			} catch (IllegalArgumentException e) {
				throw ERT.badarg(args);
			} catch (IllegalAccessException e) {
				throw new ErlangError(am_badaccess, args);
			} catch (InvocationTargetException e) {
				Throwable te = e.getTargetException();
				if (te instanceof ErlangException) {
					throw (ErlangException) te;
				} else {
					ETuple reason = ETuple.make(EAtom.intern(te.getClass()
							.getName()), EString.fromString(te.getMessage()));
					throw new ErlangError(reason, args);
				}
			}

			return JavaObject.box(self, result);
		}

		throw new ErlangError(am_badfun, args);
	}
}
