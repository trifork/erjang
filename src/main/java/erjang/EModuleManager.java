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

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.util.Collection;
import java.util.Comparator;
import java.util.Map;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import erjang.beam.Compiler;

import kilim.Pausable;

/**
 * 
 */
public class EModuleManager {

	static Logger log = Logger.getLogger(EModuleManager.class.getName());
	
	static private Map<EAtom, ModuleInfo> infos = new ConcurrentHashMap<EAtom, ModuleInfo>();

	static FunctionInfo undefined_function = null;

	static {
		FunID uf = new FunID("error_handler", "undefined_function", 3);
		undefined_function = get_module_info(uf.module).get_function_info(uf);
	}

	static class FunctionInfo {
		private final FunID fun;

		/**
		 * @param fun
		 */
		public FunctionInfo(FunID fun) {
			this.fun = fun;
		}

		EModule defining_module;
		EFun resolved_value;
		Collection<Field> resolve_points = new TreeSet<Field>(
				new Comparator<Field>() {

					@Override
					public int compare(Field o1, Field o2) {
						if (o1 == o2)
							return 0;
						int c1 = o1.getDeclaringClass().getName().compareTo(
								o2.getDeclaringClass().getName());
						if (c1 != 0)
							return c1;
						int c2 = o1.getName().compareTo(o2.getName());
						return c2;
					}
				});
		private EFun error_handler;

		/**
		 * @param ref
		 * @throws IllegalAccessException
		 * @throws IllegalArgumentException
		 */
		synchronized void add_import(final Field ref) throws Exception {
			resolve_points.add(ref);
			if (resolved_value != null) {
				// System.out.println("binding "+fun+" "+resolved_value+" -> "+ref);
				ref.set(null, resolved_value);
			} else {
				EFun h = getFunErrorHandler();
				ref.set(null, h);
			}
		}

		private EFun getFunction() {
			if (resolved_value != null) {
				return resolved_value;
			} else {
				return getFunErrorHandler();
			}
		}

		private EFun getFunErrorHandler() {
			if (error_handler != null) {
				return error_handler;
			}
			
			error_handler = makeErrorHandler();
			return error_handler;
		}

		private EFun makeErrorHandler() {
			return EFun.get_fun_with_handler(fun.arity,
					new EFunHandler() {
						public EObject invoke(EProc proc, EObject[] args)
								throws Pausable {
							
							/** Get reference to error_handler:undefined_function/3 */
							EFun uf = undefined_function.resolved_value;

							/** this is just some debugging info to help understand downstream errors */
							if (get_module_info(fun.module).is_loaded()) {
								log.log(Level.INFO, "MISSING "+fun);
							} else {
								log.log(Level.FINE, "resolving"+fun);
							}
								
							if (uf == null) {
								if (!module_loaded(fun.module)) {
									try {
										File f = Compiler.find_and_compile(fun.module.getName());
										EModuleManager.load_module(fun.module, f.toURI().toURL());										
									} catch (IOException e) {
										// we don't report this separately; it ends up causing an undefined below...
									}
								}
								
								if (resolved_value != null) {
									return resolved_value.invoke(proc, args);
								}
								
								/** this is just some debugging info to help understand downstream errors */
								log.log(Level.INFO, "failed to load "+fun+" (error_handler:undefined_function/3 not found)");
								
								throw new ErlangUndefined(fun.module,
										fun.function, fun.arity);
							} else {
								ESeq arg_list = ESeq.fromArray(args);
								ESeq ufa = ESeq.fromArray(new EObject[] {
										fun.module, fun.function, arg_list });
								return uf.apply(proc, ufa);
							}
						}
					});
		}

		/**
		 * @param fun2
		 * @param value
		 */
		synchronized void add_export(EModule definer, FunID fun2, EFun value)
				throws Exception {
			this.resolved_value = value;
			this.defining_module = definer;

			for (Field f : resolve_points) {
				// System.out.println("binding " + fun2 + " " + value + " -> " + f);
				f.set(null, value);
			}
		}

		/**
		 * @return
		 * 
		 */
		public EFun resolve() {
			return getFunction();
		}

		/**
		 * @return
		 */
		public boolean exported() {
			return resolved_value != null;
		}
	}

	static class ModuleInfo {

		private final EAtom name;
		private EModule resident;

		/**
		 * @param module
		 */
		public ModuleInfo(EAtom module) {
			this.name = module;
		}

		Map<FunID, FunctionInfo> binding_points = new ConcurrentHashMap<FunID, FunctionInfo>();

		/**
		 * @param fun
		 * @param ref
		 * @return
		 * @throws Exception
		 */
		public void add_import(FunID fun, Field ref) throws Exception {
			FunctionInfo info = get_function_info(fun);
			info.add_import(ref);
		}

		private synchronized FunctionInfo get_function_info(FunID fun) {
			FunctionInfo info = binding_points.get(fun);
			if (info == null) {
				binding_points.put(fun, info = new FunctionInfo(fun));
			}
			return info;
		}

		/**
		 * @param fun
		 * @param value
		 * @throws Exception
		 */
		public void add_export(EModule definer, FunID fun, EFun value)
				throws Exception {
			get_function_info(fun).add_export(definer, fun, value);
		}

		/**
		 * @param eModule
		 */
		public void setModule(EModule eModule) {
			this.resident = eModule;
		}

		/**
		 * @param start
		 * @return
		 */
		public EFun resolve(FunID fun) {
			return get_function_info(fun).resolve();
		}

		/**
		 * @param fun
		 * @return
		 */
		public boolean exports(FunID fun) {
			return get_function_info(fun).exported();
		}

		/**
		 * @return
		 */
		public boolean is_loaded() {
			return resident != null;
		}

		/**
		 * @return
		 */
		public synchronized ESeq get_attributes() {
			ESeq res;
			
			if (resident == null)
				res = ERT.NIL;
			else
				res = resident.attributes();
			
			return res;
		}

	}

	static void add_import(FunID fun, Field ref) throws Exception {
		get_module_info(fun.module).add_import(fun, ref);
	}

	private static ModuleInfo get_module_info(EAtom module) {
		ModuleInfo mi;
		synchronized (infos) {
			mi = infos.get(module);
			if (mi == null) {
				infos.put(module, mi = new ModuleInfo(module));
			}
		}
		return mi;
	}

	static void add_export(EModule mod, FunID fun, EFun value) throws Exception {
		get_module_info(fun.module).add_export(mod, fun, value);
	}

	// static private Map<EAtom, EModule> modules = new HashMap<EAtom,
	// EModule>();

	private static final EAtom AM_BADARG = EAtom.intern("badarg");

	static void setup_module(EModule mod_inst) throws Error {
		get_module_info(EAtom.intern(mod_inst.module_name())).setModule(
				mod_inst);

		try {
			read_annotations(mod_inst);
		} catch (Exception e) {
			throw new Error(e);
		}
	}

	@SuppressWarnings("unchecked")
	private static void read_annotations(EModule mod_inst) throws Exception {
		Class<? extends EModule> module_class = mod_inst.getClass();
		Field[] fields = module_class.getDeclaredFields();

		for (Field field : fields) {
			if (!Modifier.isStatic(field.getModifiers()))
				continue;

			/*
			 * Annotation[] ann = field.getAnnotations(); for (Annotation a :
			 * ann) { System.err.println(" -> "+field.getName()+" : "+a); }
			 */

			Import imp = field.getAnnotation(Import.class);
			if (imp != null) {
				field.setAccessible(true);
				FunID f;

				add_import(f = new FunID(imp), field);

				// System.out.println("  import " + f
				// + (resolved ? "resolved" : ""));

				continue;
			}

			Export exp = field.getAnnotation(Export.class);
			if (exp != null) {
				field.setAccessible(true);
				EFun value;
				try {
					value = (EFun) field.get(null);
				} catch (Exception e) {
					throw new Error(e);
				}

				if (value == null)
					throw new Error("field " + field + " not initialized");

				FunID f;
				add_export(mod_inst, f = new FunID(exp), value);

				// System.out.println("  export " + f);

				continue;
			}
		}

		process_native_annotations(module_class, mod_inst);

		String cname = module_class.getName();
		String nname = cname.substring(0, cname.lastIndexOf('.')) + ".Native";

		Class<? extends ENative> natives;

		try {
			natives = (Class<? extends ENative>) Class.forName(nname, true,
					module_class.getClassLoader());
		} catch (ClassNotFoundException e) {
			// System.out.println("no native: " + nname);
			return;
		}

		ENative en;
		try {
			en = natives.newInstance();
		} catch (Exception e) {
			throw new Error("cannot instantiate Natives for module "
					+ mod_inst.module_name(), e);
		}

		for (Class nat : en.getNativeClasses()) {
			process_native_annotations(nat, mod_inst);
		}

	}

	private static void process_native_annotations(Class nat, EModule mod_inst)
			throws Exception {
		for (Field field : nat.getDeclaredFields()) {
			if (!Modifier.isStatic(field.getModifiers()))
				continue;

			Import imp = field.getAnnotation(Import.class);
			if (imp != null) {
				field.setAccessible(true);
				FunID f;
				add_import(f = new FunID(imp), field);

				// System.out.println("N import " + f);

				continue;
			}

		}

		// for native methods
		Method[] methods = nat.getDeclaredMethods();

		next_method: for (Method method : methods) {

			BIF efun = method.getAnnotation(BIF.class);
			if (efun != null && efun.type().export()) {
				String mod = mod_inst.module_name();

				String name = efun.name();
				if (name.equals("__SELFNAME__"))
					name = method.getName();

				Class<?>[] parameterTypes = method.getParameterTypes();
				int arity = parameterTypes.length;
				if (arity > 0 && parameterTypes[0].equals(EProc.class)) {
					arity -= 1;
				}

				for (int i = 0; i < parameterTypes.length; i++) {
					if (i == 0 && parameterTypes[i].equals(EProc.class))
						continue;
					if (parameterTypes[i].equals(EObject.class))
						continue;

					// we only allow EProc as zero'th and EObject as other args
					// in exported functions
					continue next_method;
				}

				FunID f = new FunID(mod, name, arity);

				// System.out.println("N export " + f);

				add_export(mod_inst, f, EFun.make(method));
			}

		}
	}

	public static EModule load_module(EAtom mod, URL url) {
		String internalName = erjang.beam.Compiler.moduleClassName(mod
				.getName());
		String java_name = internalName.replace('/', '.');
		EModuleLoader loader = new EModuleLoader(url);
		Class<? extends EModule> clazz;
		try {
			clazz = (Class<? extends EModule>) loader.loadClass(java_name);
		} catch (ClassNotFoundException e1) {
			throw new ErlangError(e1);
		}
		EModule mi;
		try {
			mi = clazz.newInstance();
		} catch (Exception e) {
			throw new ErlangError(e);
		}

		// mi.read_annotations();

		return mi;
	}

	/**
	 * @param start
	 * @return
	 */
	public static EFun resolve(FunID start) {
		return get_module_info(start.module).resolve(start);
	}

	/**
	 * @param m
	 * @param f
	 * @param value
	 * @return
	 */
	public static boolean function_exported(EAtom m, EAtom f, int a) {
		FunID fun = new FunID(m, f, a);
		return get_module_info(m).exports(fun);
	}

	/**
	 * @param m
	 * @return
	 */
	public static boolean module_loaded(EAtom m) {
		ModuleInfo mi = get_module_info(m);
		return mi.is_loaded();
	}

	/**
	 * @param mod
	 * @return
	 */
	public static ESeq get_attributes(EAtom mod) {
		ModuleInfo mi = get_module_info(mod);
		return mi.get_attributes();
	}

}
