/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Erik Søe Sørensen
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
 * @author Pavlo Baron <pb@pbit.org>
 */
public class OfflineComparisonTestCase extends AbstractErjangTestCase {

	static final String RUN_WRAPPER_HOME = "src/test/erl";

    public OfflineComparisonTestCase(String name) {
        super(name);
    }

	/**
	 * @param file
	 */
	public OfflineComparisonTestCase(File file) {
		super(file);
	}


	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "compiling and running " + file.getName();
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
		try {
			TestUtil.erl_compile(RUN_WRAPPER_HOME + File.separator +"run_wrapper.erl");
			TestUtil.erl_compile(file.getAbsolutePath());
			EObject expected = do_run(file, TestUtil.ERL_PRG);
            EObject actual = do_run(file, TestUtil.get_ej());

			Assert.assertEquals(expected, actual);
		} catch (AssertionFailedError e) {
		    result.addFailure(this, e);
		} catch (Throwable e) {
			result.addError(this, e);
		}
		result.endTest(this);
	}

    private EObject do_run(File file, String prog) throws Exception {
		String moduleName = TestUtil.trimExtension(file.getName());
		String[] cmd = new String[] {prog, "-noinput",
					     "-pa", TestUtil.TEST_BEAM_DIR,
						 "-sasl", "sasl_error_logger", "false", // Prevent SASL from polluting stdout
					     "-s", "run_wrapper", "run", "erlang", moduleName,
					     "-s", "erlang", "halt"};

		byte[] bin = TestUtil.execGetBinaryOutput(cmd);

        /* Parse out the data part - drop the stdout noise.
		 * Search for the substring "DATA::"
		 * This would be easier if we could could use some
		 * indexOf()-like function; unfortunately, converting to String
		 * and back again via getBytes() is lossy in some encodings.
		 */
		int offset = 0;
		int len=bin.length;
		for (int i=0; i < len-6; i++) {
			if (bin[i+0]=='D' &&
				bin[i+1]=='A' &&
				bin[i+2]=='T' &&
				bin[i+3]=='A' &&
				bin[i+4]==':' &&
				bin[i+5]==':')
			{
				offset = i+6;
				break;
			}
		}

		EBinary binOutput = new EBinary(bin, offset, len-offset);
		try {
			return ErlConvert.binary_to_term(binOutput);
		} catch (Throwable e) {
			System.err.println("Undecodable output: "+e);
			System.err.println("Output is: "+binOutput);
			System.err.println("Full output is: "+new EBinary(bin)+"; data offset="+offset);
			return null;
		}
	}
}
