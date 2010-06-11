package erjang.m.java;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import erjang.BIF;
import erjang.EAtom;
import erjang.EBinary;
import erjang.ENative;
import erjang.EObject;
import erjang.EPseudoTerm;
import erjang.ERT;
import erjang.ESeq;
import erjang.EString;
import erjang.ErlangError;

public class Native extends ENative {

	static class JavaObject extends EPseudoTerm {
		
		@Override
		public EBinary testBinary() {
			return EBinary.fromString(this.toString());
		}
		
		public JavaObject(Object object) {
			real_object = object;
		}

		public String toString() {
			return String.valueOf(real_object);
		};

		Object real_object;

		public static EObject box(Object object) {
			if (object instanceof EObject) {
				return (EObject) object;
			}
			return new JavaObject(object);
		}

		public static Object unbox(EObject obj) {

			if (obj instanceof JavaObject) {
				return ((JavaObject)obj).real_object;
			} else {
				return obj;
			}
			
		}

		public static Object[] convert_args(Class<?>[] arg_types, ESeq arg_seq) {
			Object[] out = new Object[arg_types.length];
			
			ESeq as_iter = arg_seq;
			for (int i=0; i < arg_types.length; i++) {
				out[i] = JavaObject.unbox(arg_types[i], as_iter.head());
				as_iter = as_iter.tail();
			}
			
			return out;
		}

		private static Object unbox(Class<?> type, EObject val) {
			Mapper mapper = type_mapper.get(type);
			if (mapper == null) {
				throw new IllegalArgumentException();
			}
			return mapper.map(val);			
		}
		
		static Map<Class<?>,Mapper> type_mapper = new HashMap<Class<?>,Mapper>();
		
		interface Mapper {
			Object map(EObject val);
		}
		
		static {
			type_mapper.put(String.class, new Mapper() {

				public Object map(EObject val) {
					
					EString str;
					if ((str  = val.testString()) != null)
						return str.stringValue();

					EAtom am;
					if ((am = val.testAtom()) != null) {
						return am.getName();
					}
					
					throw new IllegalArgumentException();
				}});
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
				throw new ErlangError(EString.fromString("not a static field"), clzz,member);
			}
			
			
		} catch (Exception e) {
			throw new ErlangError(EString.fromString(e.getMessage()), clzz,member);
		}
	}

	@BIF
	static EObject call(EObject obj, EObject member, EObject typez,
			EObject argz) {

		EAtom mem_am = member.testAtom();
		ESeq type_seq = typez.testSeq();
		ESeq arg_seq = argz.testSeq();

		Object real = JavaObject.unbox(obj);		
		
		if (mem_am == null || type_seq == null
				|| arg_seq == null || type_seq.length() != arg_seq.length()
				|| real == null)
			throw ERT.badarg(obj, member, typez, argz);

		try {
			Class<?> c = real.getClass();
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

			Object res = m.invoke(real, JavaObject.convert_args(arg_types, arg_seq));
			
			if (m.getReturnType() == Void.TYPE) {
				return am_void;
			}
			
			return JavaObject.box(res);
			
			/*
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			
			
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			*/
			
		} catch (Exception e) {
			throw new ErlangError(EString.fromString(e.getMessage()), obj, member, typez, argz);
		} finally {

		}

	}

}
