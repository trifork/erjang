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
import java.util.HashMap;
import java.util.Map;

import sun.tools.java.Imports;

import erjang.beam.ClassRepo;
import erjang.beam.Compiler;
import erjang.beam.DirClassRepo;

public abstract class EModule {

	
	static private Map<EAtom,EModule> modules = new HashMap<EAtom, EModule>();
	
	private static final EAtom AM_BADARG = EAtom.intern("badarg");

	public EModule() {
		// TODO: handle if there is a module of this name already!
		modules.put(EAtom.intern(this.module_name()), this);
	}

	private Map<FUN, EFun> exports = new HashMap<FUN, EFun>();
	private Map<FUN, Field> imports = new HashMap<FUN, Field>();
	
	public void resolve()
	{
		for (Map.Entry<FUN, Field> imp : imports.entrySet()) {
			FUN spec = imp.getKey();
			Field field = imp.getValue();
			
			EModule mod = modules.get(spec.module);
			if (mod == null) {
				throw new Error("cannot import "+spec+" from "+module_name()+" (no such module)");
			}
			
			EFun fun = mod.exports.get(spec);
			if (fun == null) {
				throw new Error("cannot import "+spec+" from "+module_name()+" (no such function)");
			}
			
			try {
				field.set(null, fun);
			} catch (Exception e) {
				System.out.println("type   "+fun.getClass());
				System.out.println(" super "+fun.getClass().getSuperclass());
				throw new Error("cannot assign to "+field, e);
			}
		}
	}

	@SuppressWarnings("unchecked")
	private void read_annotations() {
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
				FUN f;
				imports.put(f = new FUN(imp), field);

				System.out.println("  import " + f);

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

				FUN f;
				exports.put(f = new FUN(exp), value);

				System.out.println("  export " + f);

				continue;
			}
		}

		String cname = module.getName();
		String nname = cname.substring(0, cname.lastIndexOf('.')) + ".Native";

		Class<? extends ENative> natives;

		try {
			natives = (Class<? extends ENative>) Class.forName(nname, true,
					module.getClassLoader());
		} catch (ClassNotFoundException e) {
			System.out.println("no native: "+nname);
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

			for (Field field : nat.getDeclaredFields()) {
				if (!Modifier.isStatic(field.getModifiers()))
					continue;

				Import imp = field.getAnnotation(Import.class);
				if (imp != null) {
					field.setAccessible(true);
					FUN f;
					imports.put(f = new FUN(imp), field);

					System.out.println("N import " + f);

					continue;
				}

			}

			// for native methods
			Method[] methods = nat.getDeclaredMethods();
			
			next_method:
			for (Method method : methods) {

				BIF efun = method.getAnnotation(BIF.class);
				if (efun != null && efun.type().export()) {
					String mod = module_name();

					String name = efun.name();
					if (name.equals("__SELFNAME__"))
						name = method.getName();

					Class<?>[] parameterTypes = method.getParameterTypes();
					int arity = parameterTypes.length;
					if (arity > 0
							&& parameterTypes[0]
									.equals(EProc.class)) {
						arity -= 1;
					}
					
					for (int i = 0; i < parameterTypes.length; i++) {
						if (i == 0 && parameterTypes[i].equals(EProc.class)) continue;
						if (parameterTypes[i].equals(EObject.class)) continue;
						
						// we only allow EProc as zero'th and EObject as other args in exported functions
						continue next_method;
					}

					FUN f = new FUN(mod, name, arity);

					System.out.println("N export " + f);

					if (!exports.containsKey(f)) {
						exports.put(f, EFun.make(method));
					}
				}

			}

		}

	}

	/**
	 * @return
	 */
	public abstract String module_name();

	/**
	 * @param mod
	 * @param classData
	 */
	@SuppressWarnings("unchecked")
	public static void load_module(EAtom mod, EBinary bin) {

		File dir = new File("out");
		ClassRepo repo = new DirClassRepo(dir);

		URL url;
		try {
			Compiler.compile(bin, repo);
			repo.close();
			url = dir.toURI().toURL();
		} catch (IOException e) {
			throw new ErlangException(e);
		}

		String internalName = erjang.beam.Compiler.moduleClassName(mod
				.getName());
		String java_name = internalName.replace('/', '.');
		EModuleLoader loader = new EModuleLoader(url);
		Class<? extends EModule> clazz;
		try {
			clazz = (Class<? extends EModule>) loader.loadClass(java_name);
		} catch (ClassNotFoundException e1) {
			throw new Error(e1);
		}
		EModule mi;
		try {
			mi = clazz.newInstance();
		} catch (Exception e) {
			throw new ErlangException(AM_BADARG, "erlang", "load_module",
					new Object[] { mod, bin }, e);
		}

		mi.read_annotations();

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
			throw new Error(e1);
		}
		EModule mi;
		try {
			mi = clazz.newInstance();
		} catch (Exception e) {
			throw new ErlangException(AM_BADARG, "erlang", "load_module",
					new Object[] { mod }, e);
		}

		mi.read_annotations();

		return mi;
	}

}
