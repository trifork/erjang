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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
		Map<File, List<File>> testsOTP = new HashMap<File, List<File>>();
		find_beam_files(testsOTP, new File(OTP_HOME));
		buildTestHiearchie(testsOTP, otpCompileSuite, TestCompileFile.class);
		//$JUnit-END$
 		suite.addTest(otpCompileSuite);
		
		TestSuite coverageRunSuite = new TestSuite("Coverage run tests");
		//$JUnit-BEGIN$
		Map<File, List<File>> testsErl = new HashMap<File, List<File>>();
		find_erl_files(testsErl, new File("src/test/erl"));
		buildTestHiearchie(testsErl, coverageRunSuite, TestRunFile.class);
		//$JUnit-END$

		suite.addTest(coverageRunSuite);

		return suite;
	}

	protected static void buildTestHiearchie(Map<File, List<File>> tests, TestSuite suite, Class<? extends AbstractErjangTestCase> clazz) {
		TestSuite ts = null;
		for (File key : tests.keySet()) {
			ts = new TestSuite(key.getPath());
			for (File sub : tests.get(key)) {
				try {
					AbstractErjangTestCase tc = clazz.newInstance();
					tc.setFile(sub);
					ts.addTest(tc);
				}
				catch(Exception e) {
					throw new IllegalArgumentException("cannot create object of: " + clazz); 
				}
			}
		}
	}

	static void find_beam_files(Map<File, List<File>> tests, File dir) {
		List<File> ts = null;
		if (! dir.isDirectory()) throw new IllegalArgumentException("not a directory: "+dir);
		for (File file : dir.listFiles()) {
			if (file.isDirectory()) {
				find_beam_files(tests, file);
			} else if (file.getName().endsWith(".beam")) {
				if (ts == null) {
					System.err.println("added.. " + dir);
					
					ts = new ArrayList<File>();
					tests.put(dir, ts);
				}

				ts.add(file);
			}
		}
	}

	static void find_erl_files(Map<File, List<File>> tests, File dir) {
		if (! dir.isDirectory()) throw new IllegalArgumentException("not a directory: "+dir);
		List<File> ts = null;

		for (File file : dir.listFiles()) {
			if (file.isDirectory()) {
				find_erl_files(tests, file);
			} else if (file.getName().endsWith(".erl")) {
				if (ts == null) {
					System.err.println("added.. " + dir);
					ts = new ArrayList<File>();
					tests.put(dir, ts);
				}

				ts.add(file);
			}
		}
	}
}
