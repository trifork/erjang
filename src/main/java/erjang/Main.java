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

import erjang.driver.efile.EFile;

public class Main {
	public static final String SYSTEM_ARCHITECTURE = "java";
	public static final String DRIVER_VERSION = "1.5";
	

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
		String otp_version = (ErjangConfig.hasString("erjang.otp.version") ? ErjangConfig.getString("erjang.otp.version", null) : null);
		String erts_version = (ErjangConfig.hasString("erjang.erts.version") ? "erts-"+ErjangConfig.getString("erjang.erts.version", null) : null);
		for (int i = 0; i < args.length; i++) {
			if ("-root".equals(args[i]) && i < args.length) {
				cmd_line_root = args[i+1];
				i +=1;
			} else if ("+e".equals(args[i]) && i < args.length) {
				erts_version = "erts-" + args[i+1];
				i += 1;
			}
		}
		
		RuntimeInfo runtimeInfo = RuntimeInfo.setup(erts_version, otp_version, cmd_line_root);
		try {
			// verify we have a working configuration
			runtimeInfo.verify();
		}
		catch (RuntimeException e) {
			String reason = e.getMessage();
			ERT.log.severe(reason);
			System.err.println(reason);
			return;
		}

		String root = runtimeInfo.erl_rootdir;
		ra.add("-root");
		ra.add(root);

		arg_loop: 
			for (int i = 0; i < args.length; i++) {
			String arg = args[i];
			
			if ("-extra".equals(arg)) {
				for (int ii = i; ii < args.length; ii++) {
					ra.add(args[ii]);
				}
				break;
			}
			
			if ("-root".equals(args[i]) && i < args.length) {
				// skip "-root <dir>" arg, was set above
				i++;
				continue;
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
		
		if (!ra.contains("-home")) {
			ra.add("-home");
			ra.add(System.getProperty("user.home"));
		}
		
		ERT.setRuntimeInfo(runtimeInfo);
		
		if (!(new File(runtimeInfo.erl_bootstrap_ebindir)).exists() && !runtimeInfo.erl_bootstrap_ebindir.startsWith(EFile.RESOURCE_PREFIX)) {
			ERT.log.severe("No bootstrap classes at: "+runtimeInfo.erl_bootstrap_ebindir);
			throw new IllegalArgumentException("No bootstrap classes at: "+runtimeInfo.erl_bootstrap_ebindir);
		}
		
		OTPMain.main(ra.toArray(new String[ra.size()]));
	}
}
