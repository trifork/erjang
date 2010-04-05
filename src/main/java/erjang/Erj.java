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
import java.net.MalformedURLException;

import erjang.beam.Compiler;

/**
 * 
 */
public class Erj {

	@SuppressWarnings("unchecked")
	public static void main(String[] args) 
		throws Exception {

		String m = args[0];
		String f;
		int idx;
		if ((idx = m.indexOf(':')) == -1) {
			f = "main";
		} else {
			f = m.substring(idx+1);
			m = m.substring(0, idx);
		}
		
		EAtom module = EAtom.intern(m);
		EAtom fun = EAtom.intern(f);
		
		load("erlang");
		load(m);
		
		ESeq env = ERT.NIL;
		ESeq argv = ERT.NIL;
		
		for (int i = args.length-1; i > 0; i--) {
			argv = argv.cons(EBinary.fromString(args[i]));
		}
		
		//String s = argv.toString();
		
		EProc p;
		ERT.run(p=new EProc(null, module, fun, argv ));
		p.joinb();

		System.out.println("done.");
	}

	/**
	 * @param string
	 */
	private static void load(String module) throws Exception {
		EModuleLoader.find_and_load_module(module);
	}
}
