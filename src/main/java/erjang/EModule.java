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

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;


public abstract class EModule {


	/** Base constructor for modules.  Will register <code>this</code> in the system. */
	public EModule() {
		this(false);
	}

	public EModule(boolean delay_setup) {
		// TODO: handle if there is a module of this name already!
		if (!delay_setup) setup();
	}

	protected void setup() {EModuleManager.setup_module(this);}

	/**
	 * This method is code-generated in subclasses
	 * @return module name, as a Java string
	 */
	public abstract String module_name();

	/**
	 * This method is code-generated in subclasses
	 * 
	 * @return the attributes associated with this module
	 */
	public ESeq attributes() {
		return ERT.NIL;
	}

	/**
	 * This method is used by EModuleManager in function resolution.
	 */
	public abstract void registerImportsAndExports() throws Exception;

        public abstract ClassLoader getModuleClassLoader();

    protected void load_native_bifs() throws Exception {
		Class<? extends ENative> natives;

		String nname = "erjang.m."+module_name()+".Native";
		try {
		    natives = (Class<? extends ENative>) Class.forName(nname, true,
					getModuleClassLoader());
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

        protected void process_native_annotations(Class nat) throws Exception {
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

				EModuleManager.add_export(this, f, EFun.make(method, mod));
			}

		}
	}

	public static class FieldBinder extends EModuleManager.FunctionBinder {
		final Field field;
		final FunID funID;
		final String mod;

	        public FieldBinder(Field field, FunID funID, String mod) {
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
