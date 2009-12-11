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
import java.math.BigInteger;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import kilim.Pausable;

import sun.tools.java.Imports;

import erjang.beam.ClassRepo;
import erjang.beam.Compiler;
import erjang.beam.DirClassRepo;

public abstract class EModule {

	static private Map<EAtom, ModuleInfo> infos = new HashMap<EAtom, ModuleInfo>();

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
		Collection<Field> resolve_points = new TreeSet<Field>(new Comparator<Field>() {

			@Override
			public int compare(Field o1, Field o2) {
				if (o1==o2) return 0;
				int c1 = o1.getDeclaringClass().getName().compareTo(o2.getDeclaringClass().getName());
				if (c1 != 0) return c1;
				int c2 = o1.getName().compareTo(o2.getName());
				return c2;
			}});

		/**
		 * @param ref
		 * @throws IllegalAccessException
		 * @throws IllegalArgumentException
		 */
		synchronized boolean add_import(final Field ref) throws Exception {
			resolve_points.add(ref);
			if (resolved_value != null) {
				//System.out.println("binding "+fun+" "+resolved_value+" -> "+ref);
				ref.set(null, resolved_value);
				return true;
			} else {
				Object h = EFun.get_fun_with_handler(fun.arity,
						new EFunHandler() {
							public EObject invoke(EProc proc, EObject[] args) 
								throws Pausable
							{
								EFun found = null;
								boolean has_loaded_module =
									EModule.get_module_info(fun.module).resident != null;
								
								if (!has_loaded_module) {
									try {
										ERT.load_module(fun.module);
										has_loaded_module = true;
									} catch (Throwable ex) {
										System.out.println("unable to load module for "+fun);
									}
								}
								
								if (has_loaded_module)
								{
									try {
										found = EModule.resolve(fun);
									} catch (Throwable ex) {
										System.out.println("unable to resolve function "+fun);
									}
								}
								
								if (found == null) {
									throw new ErlangUndefined(fun.module, fun.function, ESmall.make(fun.arity));
								} else {
									return found.invoke(proc, args);
								}
							}
						});
				ref.set(null, h);
			}
			return false;
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
				//System.out.println("binding "+fun2+" "+value+" -> "+f);
				f.set(null, value);
			}
		}

		/**
		 * @return
		 * 
		 */
		public EFun resolve() {
			return resolved_value;
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

		Map<FunID, FunctionInfo> binding_points = new HashMap<FunID, FunctionInfo>();

		/**
		 * @param fun
		 * @param ref
		 * @return
		 * @throws Exception
		 */
		public boolean add_import(FunID fun, Field ref) throws Exception {
			FunctionInfo info = get_function_info(fun);
			return info.add_import(ref);
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

	}

	boolean add_import(FunID fun, Field ref) throws Exception {
		return get_module_info(fun.module).add_import(fun, ref);
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

	void add_export(FunID fun, EFun value) throws Exception {
		get_module_info(fun.module).add_export(this, fun, value);
	}

	// static private Map<EAtom, EModule> modules = new HashMap<EAtom,
	// EModule>();

	private static final EAtom AM_BADARG = EAtom.intern("badarg");

	public EModule() {
		// TODO: handle if there is a module of this name already!
		// modules.put(EAtom.intern(this.module_name()), this);

		get_module_info(EAtom.intern(module_name())).setModule(this);

		try {
			read_annotations();
		} catch (Exception e) {
			throw new Error(e);
		}
	}

	@SuppressWarnings("unchecked")
	private void read_annotations() throws Exception {
		Class<? extends EModule> module = getClass();
		Field[] fields = module.getDeclaredFields();

		System.out.println("loading " + module_name());

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

				boolean resolved = add_import(f = new FunID(imp), field);

		//		System.out.println("  import " + f
		//				+ (resolved ? "resolved" : ""));

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
				add_export(f = new FunID(exp), value);

	//			System.out.println("  export " + f);

				continue;
			}
		}

		process_native_annotations(this.getClass());

		String cname = module.getName();
		String nname = cname.substring(0, cname.lastIndexOf('.')) + ".Native";

		Class<? extends ENative> natives;

		try {
			natives = (Class<? extends ENative>) Class.forName(nname, true,
					module.getClassLoader());
		} catch (ClassNotFoundException e) {
//			System.out.println("no native: " + nname);
			return;
		}

		ENative en;
		try {
			en = natives.newInstance();
		} catch (Exception e) {
			throw new Error("cannot instantiate Natives for module "
					+ this.module_name(), e);
		}

		for (Class nat : en.getNativeClasses()) {
			process_native_annotations(nat);
		}

	}

	private void process_native_annotations(Class nat) throws Exception {
		for (Field field : nat.getDeclaredFields()) {
			if (!Modifier.isStatic(field.getModifiers()))
				continue;

			Import imp = field.getAnnotation(Import.class);
			if (imp != null) {
				field.setAccessible(true);
				FunID f;
				add_import(f = new FunID(imp), field);

	//			System.out.println("N import " + f);

				continue;
			}

		}

		// for native methods
		Method[] methods = nat.getDeclaredMethods();

		next_method: for (Method method : methods) {

			BIF efun = method.getAnnotation(BIF.class);
			if (efun != null && efun.type().export()) {
				String mod = module_name();

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

		//		System.out.println("N export " + f);

				add_export(f, EFun.make(method));
			}

		}
	}

	/**
	 * @return
	 */
	public abstract String module_name();


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
		FunID fun = new FunID(m,f,a);
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

}
