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
import java.util.Arrays;

import erjang.boot.CommandLineParser;
import sun.misc.Signal;
import sun.misc.SignalHandler;
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
		
		// enforce Kilim to optimize for code size before anything else
		System.setProperty("kilim.optimize_codesize", "true");
		
		ErjangConfig.init();

        ArrayList<String> regular_args = new ArrayList<>();
		ArrayList<String> extra_args   = new ArrayList<>();
        CommandLineParser.parseArgumentList(args, regular_args, extra_args);

        RuntimeInfo runtimeInfo = determineRuntimeInfo(args, regular_args);

		if (runtimeInfo == null) {
			System.err.println("Erjang cannot find its BEAM files.");
			System.err.println("- You can set OTPROOT, pass -root /path/to/erlang, or put 'erl' in your PATH.");
			System.exit(1);
		}
		
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

        if (! regular_args.contains("-root")) {
            String root = runtimeInfo.erl_rootdir;
            regular_args.add(0,"-root");
            regular_args.add(1, root);
        }

		if (! regular_args.contains("-home")) {
            regular_args.add(0, "-home");
            regular_args.add(1, System.getProperty("user.home"));
		}

		ERT.setRuntimeInfo(runtimeInfo);
		
		if (!(new File(runtimeInfo.erl_bootstrap_ebindir)).exists() && !runtimeInfo.erl_bootstrap_ebindir.startsWith(EFile.RESOURCE_PREFIX)) {
			ERT.log.severe("No bootstrap classes at: "+runtimeInfo.erl_bootstrap_ebindir);
			throw new IllegalArgumentException("No bootstrap classes at: "+runtimeInfo.erl_bootstrap_ebindir);
		}
		
		@SuppressWarnings({})
		SignalHandler handler = new SignalHandler() {
			
			@Override
			public void handle(Signal arg0) {				
				ERT.print_all_stack_traces();
			}
		};
		
		sun.misc.Signal.handle(new Signal("HUP"), handler);

        ArrayList<String> actual_args = new ArrayList<>(regular_args);
        if (! extra_args.isEmpty()) {
            actual_args.add("-extra");
            actual_args.addAll(extra_args);
        }
		OTPMain.main(regular_args.toArray(new String[regular_args.size()]));
	}

    private static RuntimeInfo determineRuntimeInfo(String[] args, ArrayList<String> regular_args) {
        String cmd_line_root = null;
        String otp_version = (ErjangConfig.hasString("erjang.otp.version") ? ErjangConfig.getString("erjang.otp.version", null) : null);
        String erts_version = (ErjangConfig.hasString("erjang.erts.version") ? "erts-"+ErjangConfig.getString("erjang.erts.version", null) : null);
        String ertsVersionFromCommandLine = System.getProperty("erjang.beam.option.e");
        if (ertsVersionFromCommandLine != null) {
            erts_version = ertsVersionFromCommandLine;
        }

        for (int i = 0; i < regular_args.size(); i++) {
            String arg = regular_args.get(i);
            if ("-root".equals(arg) && i < args.length) {
                cmd_line_root = regular_args.get(i+1);
                i +=1;
            } else if ("-env".equals(arg) && (i+1) < args.length) {
                ErjangConfig.setenv(regular_args.get(i+1), regular_args.get(i+2));
                i += 2;
            }/* else if ("+e".equals(arg) && i < args.length) {
                // TODO: Handle both "+eVVV" and "+e VVV". Alternatively, getProperty("erjang.beam.option.e") to avoid duplication.
                erts_version = regular_args.get(i+1);
                i += 1;
            }*/
        }

        return RuntimeInfo.setup(erts_version, otp_version, cmd_line_root);
    }
}
