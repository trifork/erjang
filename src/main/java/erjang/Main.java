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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import erjang.driver.efile.EFile;

public class Main {
	public static final String SYSTEM_ARCHITECTURE = "java";
	public static final String DRIVER_VERSION = "1.5";

	// determine dynamically from $OTPROOT (and make therefore non-final), if unspecified
	public static String otp_version = (ErjangConfig.hasString("erjang.otp.version") ? ErjangConfig.getString("erjang.otp.version", null) : null);
	static String erts_version = (ErjangConfig.hasString("erjang.erts.version") ? "erts-"+ErjangConfig.getString("erjang.erts.version", null) : null);
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
		
		File erlangRoot = new File(erl_rootdir);
		if (!erlangRoot.isDirectory()) {
			return null;
		}
		
		if (erts_version == null) {
			// guess erts version from directory $OTPROOT/lib/erts-<version>
			File libDir = new File(erlangRoot, "lib");
			String[] ertsDirs = libDir.list(new FilenameFilter() {
				@Override
				public boolean accept(File dir, String name) {
					return name.startsWith("erts-");
				}
			});
			if (ertsDirs != null) {
				for (int d = 0; d < ertsDirs.length; d++) {
					String dir = ertsDirs[d];
					erts_version = dir;
				}
			}
		}
		
		if (otp_version == null) {
			// guess OTP version from directory $OTPROOT/bin/start.script
			File startScript = new File(erlangRoot, "bin/start.script");
			otp_version = guess_otp_version(startScript);
		}
		
		if (otp_version == null) {
			// guess OTP version from directory $OTPROOT/bin/start.script
			File startScript = new File(erlangRoot, "releases/RELEASES");
			otp_version = guess_otp_version(startScript);
		}
		
		if (otp_version == null) {
			ERT.log.severe("Cannot determine OTP version! Please specify system property 'erjang.otp.version'");
			return null;
		}
		
		erl_bootstrap_ebindir = System.getenv("ERL_BOOTSTRAP_EBIN");
		if (erl_bootstrap_ebindir == null) {
			erl_bootstrap_ebindir = erl_rootdir + File.separator + "lib" + File.separator
				+ erts_version + File.separator + "ebin";
		}
		
		return erl_rootdir;
	}
	
	private static String guess_otp_version(File file) {
		if ((file == null) || !file.exists() || !file.canRead()) {
			return null;
		}
		
		BufferedReader reader = null;
		try {
			Pattern pattern = Pattern.compile(".*\"(.*OTP[\\s]+APN.*)\",\"(R\\w+)\".*");
			reader = new BufferedReader(new FileReader(file));
			String line;
			while ((line = reader.readLine()) != null) {
				Matcher match = pattern.matcher(line);
				if (!match.matches()) {
					continue;
				}
				String otpVersion = match.group(2);
				if ((otpVersion != null)
					&& (otpVersion.length() > 0)) {
					return otpVersion;
				}
			}
			
		}
		catch (Throwable t) {
			// ignore
		}
		finally {
			// close reader
			if (reader != null) {
				try {
					reader.close();
				}
				catch (Throwable t) {
					// ignore
				}
			}
		}
		
		return null;
	}
	
	private static String guess_erl_root() {

		if (Main.class.getClassLoader().getResource("bin/start.boot") != null) {
			return EFile.RESOURCE_PREFIX.substring(0, EFile.RESOURCE_PREFIX.length()-1);
		}
		
		// this logic works on Unixes ... what about windows?
		String path = System.getenv("PATH");
		List<String> paths = new ArrayList<String>();
		paths.addAll(Arrays.asList(path.split(File.pathSeparator)));
		// also check canonical locations /usr/local/lib/erlang and
		// /opt/local/lib/erlang
		paths.add("/usr/local/lib/erlang/bin");
		paths.add("/opt/local/lib/erlang/bin");
		for (String elem : paths) {
			File dir = new File(elem);
			// TODO check for some other file, which might not be s 
			// symbolic link in a generic bin dir such as /usr/local/bin
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
		
		ERT.log.severe("Cannot find OTPROOT directory\n"
				+ "Pass -root <dir>, or set environment variable.");
		
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
		
		if (!(new File(erl_bootstrap_ebindir)).exists() && !erl_bootstrap_ebindir.startsWith(EFile.RESOURCE_PREFIX)) {
			ERT.log.severe("No bootstrap classes at: "+erl_bootstrap_ebindir);
			throw new IllegalArgumentException("No bootstrap classes at: "+erl_bootstrap_ebindir);
		}
		
		OTPMain.main(ra.toArray(new String[ra.size()]));
	}

}
