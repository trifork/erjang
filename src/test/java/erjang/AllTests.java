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

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 *
 */
public class AllTests {

	static final String OTP_HOME = ErjangConfig.getString("erjang.otp.root");

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for erjang");

		TestSuite otpCompileSuite = new TestSuite("Compiling OTP");
		//$JUnit-BEGIN$
		find_beam_files(otpCompileSuite, new File(OTP_HOME));
		//$JUnit-END$
 		suite.addTest(otpCompileSuite);
		
		TestSuite coverageRunSuite = new TestSuite("Coverage run tests");
		//$JUnit-BEGIN$
		find_erl_files(coverageRunSuite, new File("src/test/erl"));
		//$JUnit-END$

		suite.addTest(coverageRunSuite);

		return suite;
	}


	static void find_beam_files(TestSuite suite, File dir) {
		if (! dir.isDirectory()) throw new IllegalArgumentException("not a directory: "+dir);
		TestSuite ts = null;

		for (File file : dir.listFiles()) {
			if (file.isDirectory()) {
				find_beam_files(suite, file);
			} else if (file.getName().endsWith(".beam")) {
				if (ts == null) {
					System.err.println("added.. " + dir);

					ts = new TestSuite(dir.getPath());
					suite.addTest(ts);
				}

				ts.addTest(new TestCompileFile(file));

				// System.out.println("added: "+file);
			}
		}
	}

	static void find_erl_files(TestSuite suite, File dir) {
		if (! dir.isDirectory()) throw new IllegalArgumentException("not a directory: "+dir);
		TestSuite ts = null;

		for (File file : dir.listFiles()) {
			if (file.isDirectory()) {
				find_erl_files(suite, file);
			} else if (file.getName().endsWith(".erl")) {
				if (ts == null) {
					System.err.println("added.. " + dir);
					ts = new TestSuite(dir.getPath());
					suite.addTest(ts);
				}

				ts.addTest(new TestRunFile(file));
			}
		}
	}

}
