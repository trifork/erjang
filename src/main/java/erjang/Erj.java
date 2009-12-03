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
import java.net.URL;

/**
 * 
 */
public class Erj {

	public static String PRELOADED = "src/main/erl/preloaded/ebin";
	public static String[] MODULES = new String[] { "erl_prim_loader",
			"erlang", "init", "otp_ring0", "prim_file", "prim_inet",
			"prim_zip", "zlib" };

	@SuppressWarnings("unchecked")
	public static void main(String[] args) throws ClassNotFoundException, MalformedURLException, InstantiationException, IllegalAccessException {

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
		
		EModule[] modules = new EModule[MODULES.length];
		File preloaded_dir = new File(PRELOADED);
		
		for (int i = 0; i < modules.length; i++) {
			
			String mod = MODULES[i];
			File dir = preloaded_dir;
			
			File path = new File(dir, mod + ".classes");
			
			load(path, mod);
		}		
		
		EModule.load_module(EAtom.intern("io"), new File("target/classes").toURL());
		EModule.load_module(EAtom.intern("timer"), new File("target/classes").toURL());
		
		load(new File("src/main/erl/" + m + ".classes"), m);
		
		ESeq env = ERT.NIL;
		ESeq argv = ERT.NIL;
		
		for (int i = args.length-1; i > 0; i--) {
			argv = argv.cons(EBinary.fromString(args[i]));
		}
		
		//String s = argv.toString();
		
		EProc p;
		ERT.run(p=new EProc(null, module, fun, argv ));
		p.joinb();

		System.out.println("ready for second run...");
		try {
			Thread.sleep(10000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

		ERT.run(p=new EProc(null, module, fun, argv ));
		p.joinb();


		System.out.println("done.");
	}

	private static void load(File path, String mod) throws Error,
			MalformedURLException {
		if (!path.exists() || !path.isDirectory()) {
			throw new Error("no path to: "+path);
		}
					
		EModule.load_module(EAtom.intern(mod), 
					path.toURI().toURL());
	}
}
