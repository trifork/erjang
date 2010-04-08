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

		load_native_bifs();
	}

}
