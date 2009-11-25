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

import erjang.beam.ClassRepo;
import erjang.beam.Compiler;
import erjang.beam.DirClassRepo;

public abstract class EModule {

	private static final EAtom AM_BADARG = EAtom.intern("badarg");

	Map<EAtom, EModule> modules = new HashMap<EAtom, EModule>();

	public EModule() {
		// we're ready!

	}

	Map<FUN, EFun> exports = new HashMap<FUN, EFun>();
	Map<FUN, Field> imports = new HashMap<FUN, Field>();

	private EModuleLoader loader;

	private void read_annotations(Class<? extends EModule> module) {
		Field[] fields = module.getFields();

		for (Field field : fields) {
			if (!Modifier.isStatic(field.getModifiers()))
				continue;

			Import imp = field.getAnnotation(Import.class);
			if (imp != null) {
				field.setAccessible(true);
				imports.put(new FUN(imp), field);
				continue;
			}

			Export exp = field.getAnnotation(Export.class);
			if (imp != null) {
				field.setAccessible(true);
				EFun value;
				try {
					value = (EFun) field.get(null);
				} catch (Exception e) {
					throw new Error(e);
				}

				if (value == null)
					throw new Error("field " + field + " not initialized");

				exports.put(new FUN(exp), value);
				continue;
			}
		}

		// for native methods
		Method[] methods = module.getDeclaredMethods();
		for (Method method : methods) {

			ErlFun efun = method.getAnnotation(ErlFun.class);
			if (efun.export()) {
				String mod = efun.module();
				if (mod.equals("__SELFNAME__"))
					mod = module_name();

				String name = efun.name();
				if (name.equals("__SELFNAME__"))
					name = method.getName();

				int arity = method.getParameterTypes().length;
				if (arity > 0
						&& method.getParameterTypes()[0].equals(EProc.class)) {
					arity -= 1;
				}

				FUN f = new FUN(mod, name, arity);

				if (!exports.containsKey(f)) {
					exports.put(f, EFun.make(method));
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
		
		String internalName = erjang.beam.Compiler.moduleClassName(mod.getName());
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
		
		mi.setLoader(loader);
		mi.read_annotations(clazz);
		
	}

	public static load_module(EAtom mod)
	{
		String internalName = erjang.beam.Compiler.moduleClassName(mod.getName());
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
		
		mi.setLoader(loader);
		mi.read_annotations(clazz);

	}
	
	/**
	 * @param loader
	 */
	private void setLoader(EModuleLoader loader) {
		this.loader = loader;
	}
}
