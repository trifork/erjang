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
import java.io.InputStream;
import java.net.URL;
import java.net.URLClassLoader;

/**
 * Each module has it's own class loader.
 */
public class EModuleClassLoader extends URLClassLoader {

	/**
	 * @param urls
	 */
	public EModuleClassLoader(URL loadFrom) {
		super(loadFrom == null ? new URL[0] : new URL[] { loadFrom },
				EObject.class.getClassLoader());
	}

	/**
	 * @param javaName
	 * @param classData
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public <T> Class<? extends T> define(String javaName, byte[] data) {
		return (Class<? extends T>) super.defineClass(javaName, data, 0,
				data.length);
	}

	String ETUPLE_NAME = ETuple.class.getName();
	String EFUN_NAME = EFun.class.getName();

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.ClassLoader#findClass(java.lang.String)
	 */
	@Override
	protected Class<?> findClass(String name) throws ClassNotFoundException {

		if (name.startsWith(ETUPLE_NAME)) {
			int arity = Integer.parseInt(name.substring(ETUPLE_NAME.length()));
			return ETuple.get_tuple_class(arity);
		}

		if (name.startsWith(EFUN_NAME)) {
			int arity = Integer.parseInt(name.substring(EFUN_NAME.length()));
			return EFun.get_fun_class(arity);
		}
		
		if (name.startsWith("kilim.S_")) {
			String classFileName = name.replace('.', File.separatorChar) + ".class";
			InputStream resource = super.getResourceAsStream(classFileName);

			if (resource == null) {
				throw new ClassNotFoundException(name, new Error("while loading "+this.getURLs()[0]));
			}
			
			try {
				byte[] bb = new byte[resource.available()];
				resource.read(bb);
				return ERT.defineClass(EModuleClassLoader.class.getClassLoader(), name, bb, 0, bb.length);
			} catch (IOException ex) {
				throw new Error(ex);
			}
		}

		return super.findClass(name);
	}
}
