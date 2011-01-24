/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2011 by Trifork
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
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.*;

import erjang.m.erlang.ErlConvert;

import junit.framework.TestResult;
import junit.framework.Assert;
import junit.framework.AssertionFailedError;

/**
 * @author Erik Søe Sørensen <eriksoe@gmail.com>
 */
public class TestCaseWithErlangNodeAccess extends AbstractErjangTestCase {
	static final String RUN_WRAPPER_HOME = "src/test/erl";
	static final String ERL_NODE_NAME = "erl_test_counterpart";
	static final String THIS_NODE_NAME = "erjang_property_test";

	static final EObject POSITIVE_RESULT = ETuple.make(EAtom.intern("run_result"),
													   EAtom.intern("true"));

    public TestCaseWithErlangNodeAccess(String name) {
        super(name);
    }

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "Property test " + file.getName() + " (with node access)";
	}

	/* (non-Javadoc)
	 * @see junit.framework.Test#countTestCases()
	 */
	@Override
	public int countTestCases() {
		return 1;
	}

	/* (non-Javadoc)
	 * @see junit.framework.Test#run(junit.framework.TestResult)
	 */
	@Override
	public void run(TestResult result) {
		result.startTest(this);
		String rawOutput = null;
		try {
			String hostName = java.net.InetAddress.getLocalHost().getCanonicalHostName();
			TestUtil.erl_compile(RUN_WRAPPER_HOME + File.separator +"run_wrapper.erl");
			TestUtil.erl_compile(file.getAbsolutePath());
			Process erl_process = TestUtil.startErlProcess(ERL_NODE_NAME, hostName);
			String[] rawOutputArray = new String[1];
            EObject output = do_run(file, TestUtil.get_ej(), ERL_NODE_NAME, hostName, rawOutputArray);
			rawOutput = rawOutputArray[0];
			TestUtil.stopProcess(erl_process);

			Assert.assertEquals(POSITIVE_RESULT, output);
		} catch (AssertionFailedError e) {
			if (rawOutput != null) System.err.println(rawOutput);
		    result.addFailure(this, e);
		} catch (Throwable e) {
			result.addError(this, e);
		}
		result.endTest(this);
	}

    private EObject do_run(File file, String prog, String erlNodeName, String hostName, String[] outputDest) throws Exception {
		String moduleName = TestUtil.trimExtension(file.getName());
		String[] cmd = new String[] {prog, "-noinput",
									 TestUtil.nodeNameSwitch(hostName), THIS_NODE_NAME + "@" + hostName,
									 "-other", erlNodeName,
									 "-pa", TestUtil.TEST_BEAM_DIR,
									 "-pa", TestUtil.TRIQ_HOME + File.separator + "ebin",
									 "-sasl", "sasl_error_logger", "false", // Prevent SASL from polluting stdout
									 "-s", "run_wrapper", "run", "erlang", moduleName, "300",
									 "-s", "erlang", "halt"};

		byte[] bin = TestUtil.execGetBinaryOutput(cmd);
		outputDest[0] = EString.make(bin, 0, bin.length).stringValue();

        return processOutput(bin);
	}
}
