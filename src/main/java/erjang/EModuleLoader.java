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

import erjang.beam.BeamFileData;
import erjang.beam.BeamLoader;
import erjang.beam.Compiler;
import erjang.beam.EUtil;

import erjang.beam.loader.ErjangBeamDisLoader;

import erjang.util.Progress;

import java.util.List;
import java.util.ArrayList;

import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.File;
import java.io.InputStream;

import java.net.URL;

/** Handles the part of module loading that involves going from module name,
 *  or module name plus beam file name, to a BeamFileData representation;
 *  and chooses how to convert that into an EModule instance.
 *  The EModule instance may be compiled, interpreted, or whatever.
 *
 *  The entire procedure looks roughly like this:
 *  - File resolution: from module name to beam file name.
 *  - File reading: from beam file name to raw beam data.
 *  - Beam parsing: From raw beam data to beam representation (BeamFileData).
 *  - Module creation: From beam representation to executable module (EModule).
 */
class EModuleLoader {
	public static final boolean DEBUG_MODULE_LOAD = ErjangConfig.getBoolean("erjang.debug.load");

	final static BeamLoader beamParser = new ErjangBeamDisLoader();

	/*==================== API ====================*/

	public static EModule find_and_load_module(String moduleName) throws IOException {
		File input = findBeamFile(moduleName);
		if (input == null)
			throw new FileNotFoundException(moduleName); // Is this the right error message?
		return load_module(moduleName, input);
	}

	public static EModule load_module(String moduleName, File beamFile) throws IOException {
		return load_module(moduleName, EUtil.readFile(beamFile));
	}

	static long acc_int_load = 0;
	static long acc_load = 0;
	public static EModule load_module(String moduleName, EBinary beamBin) throws IOException {
		// This is where the module creation mode is selected.
		boolean use_interpreter = ErjangConfig.getBoolean("erjang.beam.option.i");
		long before = System.currentTimeMillis();
		long after;

		EModule loaded_module;
		Progress.activity("loading "+moduleName+"...");
		if (use_interpreter) {
			BeamFileData bfd = beamParser.load(beamBin.toByteArray());
			loaded_module = erjang.beam.interpreter.Interpreter.beamFileToEModule(bfd);
			after = System.currentTimeMillis();
		} else { // Use compiler
			EModuleClassLoader moduleClassLoader = ErjangCodeCache.getModuleClassLoader(moduleName, beamBin, beamParser);
			after = System.currentTimeMillis();
			loaded_module = load_compiled_module(moduleName, moduleClassLoader);
		}
		if (DEBUG_MODULE_LOAD) {
			long after_load = System.currentTimeMillis();
			System.err.print("[");
			System.err.print(moduleName);
			System.err.print(":");
			System.err.print(""+(after-before)+"ms");
			System.err.print(";"+(after_load-after)+"ms]");
			if (use_interpreter)
				acc_int_load += (after_load-after);
			else
				acc_load += (after_load-after);
			System.err.println("("+acc_load+")");
		Progress.done();
 		}

		return loaded_module;
	}

	/*==================== BEAM FILE RESOLUTION STEP ====================*/

	private static File findBeamFile(String module) {
		String n = module;

		for (File e : loadPath) {
			File beam = new File(e, n + ".beam");
			if (beam.exists())
				return beam;
		}

		return null;
	}

	final static List<File> loadPath = new ArrayList<File>();

	static {
		String sys_path = System.getenv("ERJ_PATH");
		if (sys_path != null)
			addLoadPaths(loadPath, sys_path);

		String path = ErjangConfig.getString("erjang.path", ".");
		addLoadPaths(loadPath, path);
	}

	private static void addLoadPaths(List<File> out, String path) {
		for (String s : path.split(File.pathSeparator)) {
			File elem = new File(s);
			if (elem.exists() && elem.isDirectory()) {
				out.add(elem);
			}
		}
	}

	/*==================== MODULE CREATION STEP ====================*/

	@SuppressWarnings("unchecked")
	public static EModule load_compiled_module(String mod, EModuleClassLoader loader) {

		if (DEBUG_MODULE_LOAD) {
 			System.err.println("EML| load_compiled_module: "+mod+" @ "+loader);
		}
		
		String internalName = erjang.beam.Compiler.moduleClassName(mod);
		String java_name = internalName.replace('/', '.');
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
