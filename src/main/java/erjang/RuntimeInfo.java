/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Wolfgang Schell
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
import java.io.Closeable;
import java.io.File;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import erjang.driver.efile.ClassPathResource;
import erjang.driver.efile.EFile;

/**
 * Holder of runtime information such as Erlang and OTP version, 
 * root path, etc.
 * 
 * @author jetztgradnet
 */
public class RuntimeInfo {
	public final String erts_version;
	public final String otp_version;
	public final String erl_rootdir;
	public final String erl_bootstrap_ebindir;
	
	/**
	 * the new unicode driver interface is used since OTP version R14B01.
	 * 
	 * @see http://www.erlang.org/doc/apps/stdlib/unicode_usage.html#id60205
	 */
	public final boolean unicodeDriverInterface;

	public RuntimeInfo(String erts_version, String otp_version, String erl_rootdir, String erl_bootstrap_ebindir) {
		this.erts_version = erts_version;
		this.otp_version = otp_version;
		this.erl_rootdir = erl_rootdir;
		this.erl_bootstrap_ebindir = erl_bootstrap_ebindir;
		
		if (otp_version != null) {
			unicodeDriverInterface = ("R14B".compareTo(otp_version) < 0);
		}
		else {
			unicodeDriverInterface = false;
		}
	}
	
	public void verify() throws RuntimeException {
		if (isNullOrEmpty(erl_rootdir)) {
			throw new RuntimeException("Erlang runtime not found, please specify root directory using parameter '-root <OTP_ROOT>'");
		}
		
		if (isNullOrEmpty(erts_version)) {
			throw new RuntimeException("Cannot determine ERTS version! Please specify system property 'erjang.erts.version'");
		}
		
		if (isNullOrEmpty(otp_version)) {
			throw new RuntimeException("Cannot determine OTP version! Please specify system property 'erjang.otp.version'");
		}
	}
	
	public boolean isClassPathInstallation() {
		return isClassPathInstallation(erl_rootdir);
	}

	public static boolean isClassPathInstallation(String erl_rootdir) {
		return ClassPathResource.isResource(erl_rootdir);
	}
	
	protected static final String ERLANG_RELEASESTARTFILE = "releases/start_erl.data";
	protected static final String ERLANG_RELEASEFILE = "releases/RELEASES";
	protected static final String ERLANG_STARTSCRIPT = "bin/start.script";
	protected static final String ERLANG_BOOTFILE = "bin/start.boot";
	protected static final String ERLANG_STARTSCRIPT_REGEX = ".*\".*OTP[\\s]+APN.*\",\"(R\\w+)\".*";
	protected static final String ERLANG_RELEASEFILE_REGEX = ".*\".*OTP[\\s]+APN.*\",\"(R\\w+)\",\"([0-9]\\.[0-9]\\.[0-9])\".*";
	protected static final String ERLANG_RELEASESTARTFILE_REGEX = "(\\S+)\\s+(\\S+)";

	// determine dynamically from $OTPROOT (and make therefore non-final), if unspecified
	
	public static RuntimeInfo setup(String erts_version, String otp_version, String erl_rootdir) {
		if (isNullOrEmpty(erl_rootdir)) {
			erl_rootdir = guess_erl_root();
		}
		if (!isNullOrEmpty(erl_rootdir)) {
			if (erl_rootdir.endsWith("/") || erl_rootdir.endsWith("\\")) {
				erl_rootdir = erl_rootdir.substring(0, erl_rootdir.length() - 1);
			}
		}
		
		if (erl_rootdir == null) {
			return null;
		}
		
		if (isNullOrEmpty(erts_version)) {
			erts_version = guess_erts_version(erl_rootdir);
		}
		
		if (isNullOrEmpty(otp_version)) {
			otp_version = guess_otp_version(erl_rootdir);
		}
		
		String erl_bootstrap_ebindir = System.getenv("ERL_BOOTSTRAP_EBIN");
		if (isNullOrEmpty(erl_bootstrap_ebindir)) {
			erl_bootstrap_ebindir = erl_rootdir;
			if (!erl_bootstrap_ebindir.endsWith("/") && !erl_bootstrap_ebindir.endsWith("\\")) {
				erl_bootstrap_ebindir += File.separator;
			}
			erl_bootstrap_ebindir += "lib" + File.separator + "erts-" + erts_version + File.separator + "ebin";
		}
		
		return new RuntimeInfo(erts_version, otp_version, erl_rootdir, erl_bootstrap_ebindir);
	}
	
	public static String guess_otp_version(String erl_rootdir) {
		// guess OTP version from directory $OTPROOT/releases/RELEASES
		String otp_version = guess_version(ERLANG_RELEASESTARTFILE_REGEX, 2, erl_rootdir, ERLANG_RELEASESTARTFILE);
		if (isNullOrEmpty(otp_version)) {
			otp_version = guess_version(ERLANG_RELEASEFILE_REGEX, 1, erl_rootdir, ERLANG_RELEASEFILE);
		}
		if (isNullOrEmpty(otp_version)) {
			// fallback: guess OTP version from directory $OTPROOT/bin/start.script
			otp_version = guess_version(ERLANG_STARTSCRIPT_REGEX, 1, erl_rootdir, ERLANG_STARTSCRIPT);
		}
		
		return otp_version;
	}
	
	public static String guess_erts_version(String erl_rootdir) {
		// guess ERTS version from directory $OTPROOT/releases/RELEASES
		String erts_version = guess_version(ERLANG_RELEASESTARTFILE_REGEX, 1, erl_rootdir, ERLANG_RELEASESTARTFILE);
		if (isNullOrEmpty(erts_version)) {
			erts_version = guess_version(ERLANG_RELEASEFILE_REGEX, 2, erl_rootdir, ERLANG_RELEASEFILE);
		}
		if (isNullOrEmpty(erts_version)) {
			// fallback: guess ERTS version from erts directory
			File erlangRoot = new File(erl_rootdir);
			if (erlangRoot.isDirectory()) {
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
		}
		return erts_version;
	}
	
	protected static String guess_version(String regex, int group, String erl_rootdir, String fileName) {
		if (isNullOrEmpty(erl_rootdir) || RuntimeInfo.isClassPathInstallation(erl_rootdir)) {
			return guess_version_from_resource(regex, group, fileName);
		}
		File erlangRoot = new File(erl_rootdir);
		if (erlangRoot.isDirectory()) {
			return guess_version_from_file(regex, group, new File(erlangRoot, fileName));
		}
		else {
			return guess_version_from_resource(regex, group, fileName);
		}
	}
	
	protected static String guess_version_from_resource(String regex, int group, String fileName) {
		InputStream input = null;
		
		try {
			input = Thread.currentThread().getContextClassLoader().getResourceAsStream(fileName);
			return guess_version_from_reader(regex, group, new InputStreamReader(input));
		}
		catch (Throwable t) {
			// ignore
		}
		finally {
			// close reader
			closeQuietly(input);
		}
		
		return null;
	}
	
	protected static String guess_version_from_file(String regex, int group, File file) {
		if ((file == null) || !file.exists() || !file.canRead()) {
			return null;
		}

		FileReader reader = null;
		try {
			reader = new FileReader(file);
			return guess_version_from_reader(regex, group, reader);
		}
		catch (Throwable t) {
			// ignore
		}
		finally {
			// close reader
			closeQuietly(reader);
		}
		
		return null;
	}
	
	protected static String guess_version_from_reader(String regex, int group, Reader input) {
		BufferedReader reader = null;
		try {
			Pattern pattern = Pattern.compile(regex);
			reader = new BufferedReader(input);
			String line;
			while ((line = reader.readLine()) != null) {
				Matcher match = pattern.matcher(line);
				if (!match.matches()) {
					continue;
				}
				String versionString = match.group(group);
				if ((versionString != null)
					&& (versionString.length() > 0)) {
					return versionString.trim();
				}
			}
		}
		catch (Throwable t) {
			// ignore
		}
		finally {
			// close reader
			closeQuietly(reader);
		}
		
		return null;
	}
	
	protected static boolean isNullOrEmpty(String text) {
		if (text == null || text.trim().length() == 0) {
			return true;
		}
		return false;
	}
	
	public static String guess_erl_root() {
		
		String erl_rootdir = System.getenv("OTPROOT");
		
		if (isNullOrEmpty(erl_rootdir)) {
			// check whether we have Erlang/OTP on the classpath 
			if (Thread.currentThread().getContextClassLoader().getResource(ERLANG_BOOTFILE) != null) {
				erl_rootdir = EFile.RESOURCE_PREFIX;
			}
		}
		
		if (isNullOrEmpty(erl_rootdir)) {
			// discover Erlang from $PATH
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
				// TODO check for some other file, which might not be a 
				// symbolic link in a generic bin dir such as /usr/local/bin
				File erl = new File(dir, "erl");
				if (erl.exists()) {
					File baseDir = dir.getParentFile();
					if (baseDir == null)
						continue;
					
					// check for <basedir>/releases
					File releases = new File(baseDir, "releases");
					if (releases.isDirectory()) {
						erl_rootdir = baseDir.getAbsolutePath();
						break;
					}

					
					// check for <basedir>/lib/erlang
					// (Unix-style installation with Erlang bin living next to lib/erlang)
					File lib = new File (baseDir, "lib");
					File root = new File(lib, "erlang");
					
					if (root.exists()) {
						erl_rootdir = root.getAbsolutePath();
						break;
					}
				}
			}
		}
		
		return erl_rootdir;
	}

	protected static void closeQuietly(Closeable stream) {
		if (stream != null) {
			try {
				stream.close();
			}
			catch (Throwable t) {
				// ignore
			}
		}
	}

}
