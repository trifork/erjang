package erjang.m.java;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import erjang.BIF;
import erjang.EAtom;
import erjang.EBig;
import erjang.EBinary;
import erjang.EDouble;
import erjang.EInteger;
import erjang.EList;
import erjang.ENative;
import erjang.ENumber;
import erjang.EObject;
import erjang.EPseudoTerm;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.EString;
import erjang.EStringList;
import erjang.ErlangError;

public class Native extends ENative {

	/** Wrap an Object[] and present it as an ESeq */
	static class JavaArray extends ESeq {

		private final int idx;
		private final Object[] arr;

		static ESeq box(Object[] arr, int idx) {
			if (arr.length == idx)
				return ERT.NIL;
			return new JavaArray(arr, idx);
		}

		private JavaArray(Object[] arr, int idx) {
			this.arr = arr;
			this.idx = idx;
		}

		@Override
		public ESeq cons(EObject h) {
			return new EList(h, this);
		}

		@Override
		public ESeq tail() {
			return box(arr, idx + 1);
		}

		@Override
		public EObject head() {
			return JavaObject.box(arr[idx]);
		}
	}

	/** Wrap an java.util.Iterator and present it as an ESeq */
	static class JavaIterator extends ESeq {
		Iterator<?> rest;
		EObject head;

		static ESeq box(Iterator<?> it) {
			if (it.hasNext())
				return new JavaIterator(it);
			return ERT.NIL;
		}

		private JavaIterator(Iterator<?> it) {
			this.head = JavaObject.box(it.next());
			this.rest = it;
		}

		@Override
		public ESeq cons(EObject h) {
			return new EList(h, this);
		}

		@Override
		public ESeq tail() {
			return box(rest);
		}

		@Override
		public EObject head() {
			return JavaObject.box(head);
		}
	}

	/** Wrap an java.util.Iterator and present it as an ESeq */
	static class JavaCharSeq extends ESeq {
		private final CharSequence seq;
		private final int pos;

		@Override
		public EString testString() {
			return EString.make(seq, pos, seq.length());
		}

		static ESeq box(CharSequence cs, int pos) {
			if (cs.length() == pos)
				return ERT.NIL;
			return new JavaCharSeq(cs, pos);
		}

		private JavaCharSeq(CharSequence cs, int pos) {
			this.seq = cs;
			this.pos = pos;
		}

		@Override
		public ESeq cons(EObject h) {
			ESmall s;
			if ((s = h.testSmall()) != null) {
				if ((s.value & 0xff) == s.value) {
					return new EStringList((byte) s.value, this);
				}
			}
			return new EList(h, this);
		}

		@Override
		public ESeq tail() {
			return box(seq, pos + 1);
		}

		@Override
		public EObject head() {
			return ERT.box(seq.charAt(pos));
		}
	}

	static class JavaObject extends EPseudoTerm {

		Object real_object;

		@Override
		public EBinary testBinary() {
			return EBinary.fromString(this.toString());
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
			return null;
		}

		@Override
		public ESeq testSeq() {
			if (real_object instanceof Object[]) {
				return JavaArray.box((Object[]) real_object, 0);
			}

			if (real_object instanceof Collection<?>) {
				return JavaIterator.box(((Collection<?>) real_object)
						.iterator());
			}

			if (real_object instanceof CharSequence) {
				return JavaCharSeq.box((CharSequence) real_object, 0);
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

		public JavaObject(Object object) {
			real_object = object;
		}

		public String toString() {
			return String.valueOf(real_object);
		};

		public static EObject box(Object object) {
			if (object instanceof EObject) {
				return (EObject) object;
			}
			return new JavaObject(object);
		}

		public static Object unbox(EObject obj) {

			if (obj instanceof JavaObject) {
				return ((JavaObject) obj).real_object;
			} else {
				return obj;
			}

		}

		public static Object[] convert_args(Class<?>[] arg_types, ESeq arg_seq) {
			Object[] out = new Object[arg_types.length];

			ESeq as_iter = arg_seq;
			for (int i = 0; i < arg_types.length; i++) {
				out[i] = JavaObject.unbox(arg_types[i], as_iter.head());
				as_iter = as_iter.tail();
			}

			return out;
		}

		private static Object unbox(Class<?> type, EObject val)
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

			Mapper mapper = type_mapper.get(type);
			if (mapper == null) {
				throw new IllegalArgumentException("cannot convert "
						+ val.getClass().getName() + " to " + type);
			}
			return mapper.map(val);
		}

		static Map<Class<?>, Mapper> type_mapper = new HashMap<Class<?>, Mapper>();

		interface Mapper {
			Object map(EObject val);
		}

		static {
			type_mapper.put(String.class, new Mapper() {

				public Object map(EObject val) {

					EString str;
					if ((str = val.testString()) != null)
						return str.stringValue();

					EAtom am;
					if ((am = val.testAtom()) != null) {
						return am.getName();
					}

					throw new IllegalArgumentException();
				}
			});

			Mapper int_mapper = new Mapper() {

				public Object map(EObject val) {

					ENumber num;
					if ((num = val.testNumber()) != null) {
						return new Integer(num.intValue());
					}

					throw new IllegalArgumentException();
				}
			};
			type_mapper.put(Integer.class, int_mapper);
			type_mapper.put(int.class, int_mapper);

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
	}

	private static final EObject am_void = EAtom.intern("void");

	@BIF
	static EObject get_static(EObject clzz, EObject member) {

		EAtom clz_am = clzz.testAtom();
		EAtom mem_am = member.testAtom();

		if (clz_am == null || mem_am == null)
			throw ERT.badarg(clzz, member);

		try {

			Class<?> c = Class.forName(clz_am.getName());
			Field f = c.getField(mem_am.getName());

			if (java.lang.reflect.Modifier.isStatic(f.getModifiers())) {
				return JavaObject.box(f.get(null));
			} else {
				throw new ErlangError(EString.fromString("not a static field"),
						clzz, member);
			}

		} catch (Exception e) {
			throw new ErlangError(EString.fromString(e.getMessage()), clzz,
					member);
		}
	}

	@BIF
	static EObject call(EObject obj, EObject member, EObject typez, EObject argz) {

		EAtom mem_am = member.testAtom();
		ESeq type_seq = typez.testSeq();
		ESeq arg_seq = argz.testSeq();

		Object receiver = JavaObject.unbox(obj);

		if (mem_am == null || type_seq == null || arg_seq == null
				|| type_seq.length() != arg_seq.length() || receiver == null)
			throw ERT.badarg(obj, member, typez, argz);

		try {
			Class<?> c = receiver.getClass();
			Class<?>[] arg_types = new Class<?>[type_seq.length()];
			EObject[] at = type_seq.toArray();

			for (int i = 0; i < at.length; i++) {
				EAtom am = at[i].testAtom();
				if (am == null) {
					throw ERT.badarg(obj, member, typez, argz);
				}

				arg_types[i] = Class.forName(am.getName());
			}

			Method m = c.getMethod(mem_am.getName(), arg_types);

			Object res = m.invoke(receiver, JavaObject.convert_args(arg_types,
					arg_seq));

			if (m.getReturnType() == Void.TYPE) {
				return am_void;
			}

			return JavaObject.box(res);

			/*
			 * } catch (ClassNotFoundException e) { // TODO Auto-generated catch
			 * block e.printStackTrace(); } catch (SecurityException e) { //
			 * TODO Auto-generated catch block e.printStackTrace(); } catch
			 * (NoSuchMethodException e) { // TODO Auto-generated catch block
			 * e.printStackTrace();
			 * 
			 * 
			 * } catch (IllegalArgumentException e) { // TODO Auto-generated
			 * catch block e.printStackTrace(); } catch (IllegalAccessException
			 * e) { // TODO Auto-generated catch block e.printStackTrace(); }
			 * catch (InvocationTargetException e) { // TODO Auto-generated
			 * catch block e.printStackTrace();
			 */

		} catch (Exception e) {
			throw new ErlangError(EString.fromString(e.getMessage()), obj,
					member, typez, argz);
		} finally {

		}

	}

}
