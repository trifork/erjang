/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
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

import erjang.EModule;
import erjang.EModuleManager;
import erjang.ENative;
import erjang.EFun;
import erjang.FunID;
import erjang.EObject;
import erjang.EProc;
import erjang.Export;
import erjang.Import;
import erjang.BIF;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

public abstract class ECompiledModule extends EModule {


        public ClassLoader getModuleClassLoader() {
	    return this.getClass().getClassLoader();
        }

	public void registerImportsAndExports() throws Exception {
		Class<? extends EModule> module_class = this.getClass();
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
				FunID f = new FunID(imp);

				EModuleManager.add_import(f, new FieldBinder(field, f, module_name()));

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
				EModuleManager.add_export(this, f = new FunID(exp), value);

				// System.out.println("  export " + f);

				continue;
			}
		}

		process_native_annotations(module_class);

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
					+ module_name(), e);
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
				FunID f = new FunID(imp);
				EModuleManager.add_import(f, new FieldBinder(field, f, module_name()));

				//System.out.println("N import " + f);

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

				//System.out.println("N export " + f);

				EModuleManager.add_export(this, f, EFun.make(method));
			}

		}
	}

	private static class FieldBinder extends EModuleManager.FunctionBinder {
		final Field field;
		final FunID funID;
		final String mod;

		FieldBinder(Field field, FunID funID, String mod) {
			this.field = field;
			field.setAccessible(true);
			this.funID = funID;
			this.mod = mod;
		}

		@Override
		public FunID getFunID(){
			return funID;
		}

		public void bind(EFun value) throws Exception {
			field.set(null, value);
		}

		public String toString() {
			return "<FieldBinder for "+funID+" in "+mod+": "+field+">";
		}
	}

}
