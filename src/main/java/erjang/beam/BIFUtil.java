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

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.objectweb.asm.Type;

import erjang.EAtom;
import erjang.EBig;
import erjang.EDouble;
import erjang.ENative;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.ESmall;
import erjang.m.erlang.BinOps;
import erjang.m.erlang.ErlBif;

/**
 * Used by the compiler to find and mange BIF definitions.
 * 
 * To add bifs, add the classes to the first "static" block below.
 * 
 * @author krab
 */
public class BIFUtil {
	public static final Type EOBJECT_TYPE = Type.getType(EObject.class);

	public static final Type EPROC_TYPE = Type.getType(EProc.class);
	static Map<String, BIFHandler> bifs = new HashMap<String, BIFHandler>();
	static Map<String, BIFHandler> guard_bifs = new HashMap<String, BIFHandler>();

	static {
		try {
			loadBIFs(new String[]{"erlang", "error_logger", "ets", "lists", "math", 
								  "net_kernel", "os", "re", "unicode", 
								  "crypto"});
		} catch (Exception e) {
			throw new Error("Missing native module", e);
		}
	}

	static class Args {
		Class[] args;
		private Args generic;
		private int hashCode;
		private int code;

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

					if (types[i] == Type.BOOLEAN_TYPE) {
						args[i] = boolean.class;
					} else if (types[i] == Type.INT_TYPE) {
						args[i] = int.class;
					} else if (types[i] == Type.DOUBLE_TYPE) {
						args[i] = double.class;
					} else {
						throw new Error(e);
					}
				}
			}
		}

		public Args(Class[] a) {
			this.args = a;
		}

		@Override
		public int hashCode() {
			if (code != 0)
				return code;
			for (int i = 0; i < args.length; i++) {
				code += args[i].getName().hashCode();
			}
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

		/**
		 * @return
		 */
		public List<Args> generalize() {
			ArrayList<Args> res = new ArrayList<Args>();

			Class[] aa = this.args.clone();

			for (int i = 0; i < args.length; i++) {
				// aa = this.args.clone();

				// vary over arg[i]'s super types
				for (Class c = args[i]; !c.equals(Object.class); c = super_class(c)) {
					aa[i] = c;
					res.add(new Args(aa.clone()));

					for (int j = i + 1; j < args.length; j++) {

						for (Class cc = args[j]; !cc.equals(Object.class); cc = super_class(cc)) {
							aa[j] = cc;
							res.add(new Args(aa.clone()));
						}
					}
				}
			}

			return res;
		}

		private Class super_class(Class c) {
			if (c.isPrimitive()) {
				if (c == double.class)
					return EDouble.class;
				if (c == int.class)
					return ESmall.class;
				if (c == long.class)
					return EBig.class;
				if (c == boolean.class)
					return EAtom.class;
				return EObject.class;
			} else {
				return c.getSuperclass();
			}
		}
	}

	static class BIFHandler {

		Map<Args, BuiltInFunction> found = new HashMap<Args, BuiltInFunction>();

		private final String name;
		private final String javaName;

		@Override
		public int hashCode() {
			return name.hashCode() + javaName.hashCode()
				+ found.values().hashCode();
		}
		
		public BIFHandler(String name) {
			this.name = name;
			this.javaName = name;
		}

		public BIFHandler(String name, String javaName) {
			this.name = name;
			this.javaName = javaName;
		}

		public Type getResult(Type[] parmTypes) {
			BuiltInFunction method = getMethod(parmTypes);
			if (method == null) {
				throw new Error("no bif name " + this.name + " for parms "
						+ new Args(parmTypes));
			}
			return method.getReturnType();
		}

		public void registerMethod(Method method) {
			Args a;
			
			Class<?>[] pt = method.getParameterTypes();
			
			if (!Modifier.isStatic(method.getModifiers())) {
				Class[] all = new Class[pt.length + 1];
				all[0] = method.getDeclaringClass();
				System.arraycopy(pt, 0, all, 1, pt.length);
				a = new Args(all);
			} else {
				a = new Args(pt);
			}
			
			found.put(a, new BuiltInFunction(method));
		}

		/**
		 * @param parmTypes
		 * @return
		 */
		public BuiltInFunction getMethod(Type[] parmTypes) {

			BuiltInFunction m = find_bif(parmTypes);
			if (m != null)
				return m;

			// try with EProc as first argument
			if (parmTypes.length == 0 || !EPROC_TYPE.equals(parmTypes[0])) {
				Type[] extra = new Type[parmTypes.length + 1];
				extra[0] = EPROC_TYPE;
				for (int i = 0; i < parmTypes.length; i++) {
					extra[i + 1] = parmTypes[i];
				}

				m = find_bif(extra);
				if (m != null)
					return m;
			}

			return m;

		}

		private BuiltInFunction find_bif(Type[] parmTypes) {
			Args args = new Args(parmTypes);
			BuiltInFunction m = found.get(args);
			if (m != null) {
				return m;
			}

			for (Args a : args.generalize()) {
				m = found.get(a);
				if (m != null) {
					if (ERT.DEBUG2)
						System.err.println("missed opportunity erlang:"
							+ EAtom.intern(name) + "/" + parmTypes.length + " "
							+ args + ", \n\tusing " + m);

					return m;
				}
			}
			return null;
		}

	}

	public static Type getBifResult(String module, String name, Type[] parmTypes,
			boolean isGuard) {

		Map<String, BIFHandler> tab = isGuard ? guard_bifs : bifs;

		BIFHandler bif = null;
		String key = module + ":" + name;
		if (tab.containsKey(key)) {
			bif = tab.get(key);
		} else {
			throw new Error("no " + (isGuard ? "guard" : "normal")
					+ " bif named "+module+":'" + name + "'/" + parmTypes.length );
		}

		return bif.getResult(parmTypes);
	}

	@SuppressWarnings("unchecked")
	private static void loadBIFs(String[] mods) throws Exception {
		for (String mod : mods) {
			Class<ENative> en = 
				(Class<ENative>) Class.forName("erjang.m." + mod + ".Native");
			registerBifs(mod, en);
			ENative enative = en.newInstance();
			for (Class c : enative.getNativeClasses()) {
				if (c != en) {
					registerBifs(mod, c);
				}
			}
		}

		registerBifs("erlang", ERT.class);
		registerBifs("erlang", EObject.class);
		registerBifs("erlang", ESmall.class);
		registerBifs("erlang", EBig.class);
	}
	
	public static void registerBifs(String module, Class<?> clazz) {
		Method[] m = clazz.getMethods();
		for (int i = 0; i < m.length; i++) {
			Method method = m[i];
			erjang.BIF ann = method.getAnnotation(erjang.BIF.class);
			if (ann != null) {

				if ((method.getModifiers() & Modifier.STATIC) != Modifier.STATIC) { 
				//	System.out.println("non-static BIF "+method);
				}

				Map<String, BIFHandler> tab = ann.type() == erjang.BIF.Type.GUARD ? guard_bifs
						: bifs;

				String bifName = ann.name();
				if (bifName.equals("__SELFNAME__")) {
					bifName = method.getName();
				}
				
				String key = module + ":" + bifName;
				
				BIFHandler h = tab.get(key);
				if (h == null) {
					tab.put(key, h = new BIFHandler(bifName));
				}

				h.registerMethod(method);
			}
		}
	}

	public static BuiltInFunction getMethod(String module, String name, int arity,
			boolean isGuard, boolean fail_when_missing)
	{
		return getMethod(module, name, eobjectParmTypes(arity),
						 isGuard, fail_when_missing);
	}

	private static Type[] eobjectParmTypes(int length) {
		// TODO: cache these!
		Type[] res = new Type[length];
		for (int i = 0; i < length; i++) {
			res[i] = EOBJECT_TYPE;
		}
		return res;
	}

	/**
	 * @param name
	 * @param parmTypes
	 * @param fail_when_missing TODO
	 * @param b
	 * @return
	 */
	public static BuiltInFunction getMethod(String module, String name, Type[] parmTypes,
			boolean isGuard, boolean fail_when_missing) {

		Map<String, BIFHandler> tab = isGuard ? guard_bifs : bifs;

		BIFHandler bif = null;
		String key = module + ":" + name;
		if (tab.containsKey(key)) {
			bif = tab.get(key);
		} else if (fail_when_missing) {
			throw new Error("no " + (isGuard ? "guard" : "normal")
					+ " bif named " + module+ ":'" + name + "'/" + parmTypes.length);
		} else {
			return null;
		}

		return bif.getMethod(parmTypes);
	}

	/**
	 * @param module TODO
	 * @param name
	 * @param args
	 * @param isGuard
	 * @param fail_when_missing TODO
	 * @return
	 */
	public static BuiltInFunction getMethod(String module, String name,
			Arg[] args, boolean isGuard, boolean fail_when_missing) {
		Type[] parms = new Type[args.length];
		for (int i = 0; i < args.length; i++) {
			parms[i] = args[i].type;
		}
		return getMethod(module, name, parms, isGuard, fail_when_missing);
	}

	public static BuiltInFunction getMethod(EAtom module, EAtom function,
			int arity, boolean isGuard, boolean failWhenMissing) {
		return getMethod(module.getName(), function.getName(), arity, isGuard, failWhenMissing);
	}

	static long all_bif_hash = 0;
	public static long all_bif_hash() {
		if (all_bif_hash == 0) {
			for (BIFHandler b : bifs.values()) {
				all_bif_hash += b.hashCode();
			}

			for (BIFHandler b : guard_bifs.values()) {
				all_bif_hash += b.hashCode();
			}
		}
		
		return all_bif_hash;
	}

}
