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
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * when called using "main", this class generates test class files. Otherwise, it is a test suite
 *
 * @author <unknown> (who wrote the original version?)
 * @author Pavlo Baron <pb@pbit.org>
 */
public class AllTests {

	static final String OTP_HOME = ErjangConfig.getString("erjang.otp.root");

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for erjang");

		TestSuite otpCompileSuite = new TestSuite("Compiling OTP");
		//$JUnit-BEGIN$
		//find_beam_files(otpCompileSuite, new File(OTP_HOME));
		//$JUnit-END$
 		suite.addTest(otpCompileSuite);
		
		TestSuite coverageRunSuite = new TestSuite("Coverage run tests");
		//$JUnit-BEGIN$
		Map<File, List<File>> testsErl = new HashMap<File, List<File>>();
		find_files(testsErl, new File("src/test/erl/deterministic"), ".erl");
		buildTestHierarchy(testsErl, coverageRunSuite, OfflineComparisonTestCase.class);
		//$JUnit-END$
		suite.addTest(coverageRunSuite);

		return suite;
	}

	protected static void buildTestHierarchy(Map<File, List<File>> tests, TestSuite suite,
                                             Class<? extends AbstractErjangTestCase> clazz) {
		TestSuite ts = null;
		for (File key : tests.keySet()) {
			ts = new TestSuite(key.getPath());
            suite.addTest(ts);
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

	static void find_files(Map<File, List<File>> tests, File dir, String ext) {
		List<File> ts = null;
		if (! dir.isDirectory()) throw new IllegalArgumentException("not a directory: "+dir);
		for (File file : dir.listFiles()) {
			if (file.isDirectory()) {
				find_files(tests, file, ext);
			} else if (file.getName().endsWith(ext)) {
				if (ts == null) {
					System.err.println("added.. " + dir);
					
					ts = new ArrayList<File>();
					tests.put(dir, ts);
				}

				ts.add(file);
			}
		}
	}

    public static void main(String[] args) {
        if (args.length == 0) {
            throw new IllegalArgumentException("need at least one command line argument (path)");
        }

        Map<File, List<File>> testsOTP = new HashMap<File, List<File>>();
		find_files(testsOTP, new File(OTP_HOME), ".beam");
        generateTestClasses(args[0], testsOTP, TestCompileFile.class);

        Map<File, List<File>> testsDet = new HashMap<File, List<File>>();
		find_files(testsDet, new File("src/test/erl/deterministic"), ".erl");
        generateTestClasses(args[0], testsDet, OfflineComparisonTestCase.class);

        Map<File, List<File>> testsProp = new HashMap<File, List<File>>();
		find_files(testsProp, new File("src/test/erl/properties/simple"), ".erl");
        generateTestClasses(args[0], testsProp, PropertyTestCase.class);

        Map<File, List<File>> testsPropNode = new HashMap<File, List<File>>();
		find_files(testsPropNode, new File("src/test/erl/properties/erlang"), ".erl");
        generateTestClasses(args[0], testsPropNode, TestCaseWithErlangNodeAccess.class);
    }

    protected static void generateTestClasses(String path, Map<File, List<File>> tests,
                                              Class<? extends AbstractErjangTestCase> clazz) {
        for (File key : tests.keySet()) {
			for (File sub : tests.get(key)) {
                String name = TestClassGenerator.classNameFor(sub);
                String content = TestClassGenerator.generateClassSource(clazz, sub);
                try {
                    File file = new File(path + File.separator + name + ".java");

                    System.out.println("generating test class: " + file.getName());

                    file.createNewFile();
                    FileOutputStream fop = new FileOutputStream(file);
                    fop.write(content.getBytes());
                    fop.flush();
                    fop.close();
                }
                catch (Exception e) {
                    throw new IllegalStateException(e);
                }
			}
		}
    }
}
