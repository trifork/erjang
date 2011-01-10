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
import java.util.ArrayList;

public class Main {
	public static final String SYSTEM_ARCHITECTURE = "java";
	public static final String OTP_VERSION = ErjangConfig.getString("erjang.otp.version", "R14A");
	public static final String DRIVER_VERSION = "1.5";

	static String erts_version = "erts-"+ErjangConfig.getString("erjang.erts.version", "5.8");
	static String erl_rootdir;
	static String erl_bootstrap_ebindir;
	
	public static String erts_version() {
		return erts_version;
	}
	
	static String setup(String cmd_line_root) {
		
		erl_rootdir = cmd_line_root;
		if (cmd_line_root == null)			
			erl_rootdir = System.getenv("OTPROOT");
				
		if (erl_rootdir == null) {
			erl_rootdir = guess_erl_root();
		}
		
		erl_bootstrap_ebindir = System.getenv("ERL_BOOTSTRAP_EBIN");
		if (erl_bootstrap_ebindir == null) {
			erl_bootstrap_ebindir = erl_rootdir + File.separator + "lib" + File.separator
				+ erts_version + File.separator + "ebin";
		}
		
		return erl_rootdir;
	}
	
	private static String guess_erl_root() {

		// this logic works on Unixes ... what about windows?
		String path = System.getenv("PATH");
		for (String elem : path.split(File.pathSeparator)) {
			File dir = new File(elem);
			File erl = new File(dir, "erl");
			if (erl.exists()) {
				
				if (dir.getParent() == null)
					continue;
				
				File lib = new File (dir.getParent(), "lib");
				File root = new File(lib, "erlang");
				
				if (root.exists()) {
					return root.getAbsolutePath();
				}
				
			}
		}
		
		System.err.println("Cannot find OTPROOT directory\n"
				+ "Pass -root <dir>, or set environment variable.");
		System.exit(-1);
		
		return null;
	}

	/**
	 * @param args
	 * @throws IOException 
	 * @throws IllegalAccessException 
	 * @throws InstantiationException 
	 * @throws ClassNotFoundException 
	 */
	public static void main(String[] args) throws Exception {
		ErjangConfig.init();

		ArrayList<String> ra = new ArrayList<String>();
		
		String cmd_line_root = null;
		for (int i = 0; i < args.length; i++) {
			if ("-root".equals(args[i]) && i < args.length) {
				cmd_line_root = args[i+1];
				i +=1;
			} else if ("+e".equals(args[i]) && i < args.length) {
				erts_version = "erts-" + args[i+1];
				i += 1;
			}
		}
		
		String root = setup(cmd_line_root);

		if (cmd_line_root == null) {
			ra.add("-root");
			ra.add(root);
		}

		arg_loop: 
			for (int i = 0; i < args.length; i++) {
			String arg = args[i];
			
			if ("-extra".equals(arg)) {
				for (int ii = i; ii < args.length; ii++) {
					ra.add(args[ii]);
				}
				break;
			}
			
			if (arg.startsWith("+")) {
				switch (arg.charAt(1)) {
				case 'a':
				case 'e': // strip erts version too
				case 'A':
				case 'B':
				case 'h':
				case 'K':
				case 'M':
				case 'P':
				case 'R':
				case 'S':
				case 's':
				case 't':
				case 'T':
				case 'W':
					System.setProperty("erjang.beam.option."+arg.substring(1), args[i+1]);
					i += 1;
					continue arg_loop;
				default:
					System.setProperty("erjang.beam.option."+arg.substring(1), "true");
					continue arg_loop;
				}
			}
			
			ra.add(arg);
		}
		
		System.setProperty("erjang.path", erl_bootstrap_ebindir);
		
		if (!(new File(erl_bootstrap_ebindir)).exists()) {
			System.err.println("No bootstrap classes at: "+erl_bootstrap_ebindir);
			System.exit(1);
		}
		
		OTPMain.main(ra.toArray(new String[ra.size()]));
	}

}
