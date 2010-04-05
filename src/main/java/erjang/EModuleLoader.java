/** -*- tab-width: 4 -*-
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

package erjang;

import erjang.beam.Compiler;
import erjang.beam.EUtil;

import java.io.IOException;
import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.net.URLClassLoader;

/** Handles the part of module loading that involves going from module name,
 *  or module name plus beam file name, to an EModule instance.
 *  The EModule instance may be compiled, interpreted, or whatever.
 *
 */
class EModuleLoader {

	public static EModule find_and_load_module(String moduleName) throws IOException {
		File jarFile = Compiler.find_and_compile(moduleName);
		return load_compiled_module(moduleName, jarFile.toURI().toURL());
	}

	public static EModule load_module(String moduleName, File beamFile) throws IOException {
		return load_module(moduleName, EUtil.readFile(beamFile));
	}

	public static EModule load_module(String moduleName, EBinary beamBin) throws IOException {
 		File jarFile = Compiler.compile(moduleName, beamBin);
 		return load_compiled_module(moduleName, jarFile.toURI().toURL());
	}

	@SuppressWarnings("unchecked")
	public static EModule load_compiled_module(String mod, URL jarUrl) {
		String internalName = erjang.beam.Compiler.moduleClassName(mod);
		String java_name = internalName.replace('/', '.');
		EModuleClassLoader loader = new EModuleClassLoader(jarUrl);
		Class<? extends EModule> clazz;
		try {
			clazz = (Class<? extends EModule>) loader.loadClass(java_name);
		} catch (ClassNotFoundException e1) {
			e1.printStackTrace();
			throw new ErlangError(e1);
		}
		EModule mi;
		try {
			mi = clazz.newInstance();
		} catch (Exception e) {
			e.printStackTrace();
			throw new ErlangError(e);
		}

		return mi;
	}
}
