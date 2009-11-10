package org.erlang.beam;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.erlang.EBIF;
import org.erlang.EObject;
import org.erlang.bif;
import org.objectweb.asm.Type;

public class BIFUtil {

	static class Args {
		private static final Type EOBJECT_TYPE = null;
		Class[] args;
		private Args generic;

		Args(Type[] types) {
			args = new Class[types.length];
			for (int i = 0; i < types.length; i++) {
				if (types[i] == Type.DOUBLE_TYPE) {
					args[i] = double.class;
					continue;
				}
				if (types[i] == Type.INT_TYPE) {
					args[i] = int.class;
					continue;
				}
				try {
					args[i] = Class.forName(types[i].getClassName());
				} catch (ClassNotFoundException e) {
					throw new Error(e);
				}
			}
		}

		public Args(Class[] a) {
			this.args = a;
		}

		@Override
		public int hashCode() {
			int code = 0;
			for (Class c : args) {
				code += c.hashCode();
			}
			return code;
		}

		@Override
		public boolean equals(Object obj) {
			if (obj instanceof Args) {
				Args other = (Args) obj;
				if (other.args.length == args.length) {

					for (int i = 0; i < args.length; i++) {
						if (!args[i].equals(other.args[i])) {
							return false;
						}
					}
					
					return true;
				}
			}
			return false;
		}

		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder("(");
			boolean first = true;
			for (Class c : args) {
				if (!first)
					sb.append(",");
				else
					first = false;
				sb.append(c.getName());
			}
			sb.append(")");
			return sb.toString();
		}

		public Args generic() {

			if (generic == null) {
				Class[] a = new Class[args.length];
				for (int i = 0; i < args.length; i++) {
					a[i] = EObject.class;
				}
				generic = new Args(a);
			}

			return generic;
		}
	}

	static class BIFHandler {

		Map<Args, Method> found = new HashMap<Args, Method>();

		private final String name;
		private final String javaName;

		public BIFHandler(String name) {
			this.name = name;
			this.javaName = name;
		}

		public BIFHandler(String name, String javaName) {
			this.name = name;
			this.javaName = javaName;
		}

		public Type getResult(Type[] parmTypes) {
			Args args = new Args(parmTypes);
			Method m = found.get(args);
			if (m == null) {
				
				if ((m = found.get(args.generic())) == null) {
					throw new Error("no bif " + javaName + "" + args);
				}

				System.err.println("missed opportunity: "+name+"/"+parmTypes.length+" "+args);
			}

			return Type.getType(m.getReturnType());
		}

		public void registerMethod(Method method) {
			Args a = new Args(method.getParameterTypes());

			found.put(a, method);
		}

	}

	static Map<String, BIFHandler> bifs = new HashMap<String, BIFHandler>();
	static Map<String, BIFHandler> guard_bifs = new HashMap<String, BIFHandler>();

	public static Type getBifResult(String name, Type[] parmTypes,
			boolean isGuard) {

		Map<String, BIFHandler> tab = isGuard ? guard_bifs : bifs;

		BIFHandler bif = null;
		if (tab.containsKey(name)) {
			bif = tab.get(name);
		} else {
			throw new Error("no bif named '"+name+"'");
		}

		return bif.getResult(parmTypes);
	}

	static {
		Class<?> clazz = EBIF.class;
		registerBifs(clazz);
	}

	private static void registerBifs(Class<?> clazz) {
		Method[] m = clazz.getMethods();
		for (int i = 0; i < m.length; i++) {
			Method method = m[i];
			bif ann = method.getAnnotation(bif.class);
			if (ann != null) {
				Map<String, BIFHandler> tab = ann.type()==org.erlang.bif.Type.GUARD ? guard_bifs : bifs;
				
				String bifName = ann.name();
				if (bifName.equals("__SELF__")) {
					bifName = method.getName();
				}
				BIFHandler h = tab.get(bifName);
				if (h == null) {
					tab.put(bifName, h=new BIFHandler(bifName));
				}
				
				h.registerMethod(method);
			}
		}
	}

}
