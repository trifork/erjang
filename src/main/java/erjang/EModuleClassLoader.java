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

import erjang.beam.RamClassRepo;

/**
 * Each module has its own class loader.
 */
public class EModuleClassLoader extends URLClassLoader {
    private final RamClassRepo ramClassRepo;

	/**
	 * @param urls
	 */
    public EModuleClassLoader(URL loadFrom, RamClassRepo repo) {
		super(loadFrom == null ? new URL[0] : new URL[] { loadFrom },
				EObject.class.getClassLoader());
		this.ramClassRepo = repo;
    }

    public EModuleClassLoader(URL loadFrom) {
	this(loadFrom, null);
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
			if (name.endsWith("Exported")) {
				int num_start  = EFUN_NAME.length();
				int num_end    = name.length() - "Exported".length();
				int arity = Integer.parseInt(name.substring(num_start, num_end));
				return EFun.get_exported_fun_class(arity);
			} else {
				int arity = Integer.parseInt(name.substring(EFUN_NAME.length()));
				return EFun.get_fun_class(arity);
			}
		}

		if (name.startsWith("kilim.S_")) {
			/* Resource names are '/'-separated, no matter the platform. */
			String classResourceName = name.replace('.', '/') + ".class";
			InputStream resource = super.getResourceAsStream(classResourceName);

			byte[] bb;
			if (resource != null) {
			    try {
				bb = new byte[resource.available()];
				resource.read(bb);
			    } catch (IOException ex) {
				throw new Error(ex);
			    }
			} else if (ramClassRepo != null) {
			    bb = ramClassRepo.get(name); // Might be null.
			} else bb = null;

			if (bb == null) {
			    throw new ClassNotFoundException(name, new Error("while loading "+this.getURLs()[0]));
			}

			return ERT.defineClass(EModuleClassLoader.class.getClassLoader(), name, bb);
		}

		if (ramClassRepo != null) {
		    byte[] data = ramClassRepo.get(name);
		    if (data != null) {
			return /*(Class<? extends T>)*/ super.defineClass(name, data, 0,
								      data.length);
		    }
		}

		return super.findClass(name);
	}
}
