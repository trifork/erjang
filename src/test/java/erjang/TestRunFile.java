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
 * @author <? who wrote the original version>
 * @author Pavlo Baron <pb@pbit.org>
 */
public class TestRunFile extends AbstractErjangTestCase {

	static final String OTP_HOME = ErjangConfig.getString("erjang.otp.root");
	static final String ERTS_VSN = ErjangConfig.getString("erjang.erts.version");
	static final String ERLC_PRG = OTP_HOME + File.separator + "bin" + File.separator + "erlc";
	static final String ERL_PRG  = OTP_HOME + File.separator + "bin" + File.separator + "erl";

    static final String EJ_PRG  = "./ej";

	static final String RUN_WRAPPER_HOME = "src/test/erl";
	static final String BEAM_DIR = "target/test-beam";

    public TestRunFile() {
        super();
    }

    public TestRunFile(String name) {
        super(name);
    }

	/**
	 * @param file
	 */
	public TestRunFile(File file) {
		super(file);
		
		String path1 = OTP_HOME + File.separator + "erts" + File.separator
				+ "preloaded" + File.separator + "ebin";
		String path2 = OTP_HOME + File.separator + "lib" + File.separator
				+ "erts-" + ERTS_VSN + File.separator + "ebin";
		String path3 = OTP_HOME + File.separator + "lib" + File.separator
				+ "stdlib" + File.separator + "ebin";
		System.setProperty("erjang.path",
						   path1 + File.pathSeparator +
						   path2 + File.pathSeparator +
						   path3);
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

    protected String get_ej() {
        String ej = EJ_PRG;

        //on Windows, we need to wrap ej into a .bat wrapper
        if (System.getProperty("os.name").startsWith("Windows")) {
            ej = "ej.bat";
        }

        return ej;
    }

	/* (non-Javadoc)
	 * @see junit.framework.Test#run(junit.framework.TestResult)
	 */
	@Override
	public void run(TestResult result) {
		result.startTest(this);
		try {
			erl_compile(RUN_WRAPPER_HOME+"/run_wrapper.erl");
			erl_compile(file.getAbsolutePath());
			EObject expected = do_run(file, ERL_PRG);
            EObject actual = do_run(file, get_ej());

			Assert.assertEquals(expected, actual);
            
		} catch (AssertionFailedError e) {
		    result.addFailure(this, e);
		} catch (Throwable e) {
			result.addError(this, e);
		}
		result.endTest(this);
	}

    private EObject do_run(File file, String prog) throws Exception {
		String moduleName = trimExtension(file.getName());
		String[] cmd = new String[] {prog, "-noinput",
					     "-pa", BEAM_DIR,
						 "-sasl", "sasl_error_logger", "false", // Prevent SASL from polluting stdout
					     "-s", "run_wrapper", "run_wrapper", "erlang", moduleName,
					     "-s", "erlang", "halt"};

		byte[] bin = execGetBinaryOutput(cmd);

		return ErlConvert.binary_to_term(new EBinary(bin));
	}

	private void erl_compile(String fileName) throws Exception {
		execGetOutput(new String[] {ERLC_PRG,
					    "-o", BEAM_DIR,
					    fileName});
	}

	private String execGetOutput(String[] cmd) throws Exception {
	    byte[] output = execGetBinaryOutput(cmd);
	    return output.toString();
	}
	private byte[] execGetBinaryOutput(String[] cmd) throws Exception {
		Runtime rt = Runtime.getRuntime();
		Process p = rt.exec(cmd);

		OutputCollectorThread outThread = new OutputCollectorThread(p.getInputStream());
		OutputCollectorThread errThread = new OutputCollectorThread(p.getErrorStream());
		outThread.start();
		errThread.start();
		outThread.join();
		errThread.join();
		int exitCode = p.waitFor(); //TODO: implement a timeout
		if (exitCode != 0) {
			System.err.println("Exitcode="+exitCode+" for "+cmd[0]);
			System.err.println("Err//output="+new String(errThread.getResult())+"//"+new String(outThread.getResult()));
		}

		assert(exitCode == 0);

        return Arrays.copyOf(outThread.getResult(), outThread.getResult().length);
	}

	static class OutputCollectorThread extends Thread {
		InputStream in;
		ByteArrayOutputStream acc = new ByteArrayOutputStream();
		byte[] buf = new byte[1024];
		public OutputCollectorThread(InputStream in) {
			this.in = in;
		}

		@Override public void run() {
			try {
				int len;
				while ((len = in.read(buf)) > 0) {
					acc.write(buf, 0, len);
				}
			} catch (IOException ioe) {
				System.err.println("I/O error: "+ioe);
				try { in.close(); } catch (IOException ioe2) {}
			}
		}

		public byte[] getResult() {
			return acc.toByteArray();
		}
	}


	private static String trimExtension(String fileName) {
	    int i = fileName.lastIndexOf('.');
	    if (i>=0) fileName = fileName.substring(0,i);
	    return fileName;
	}
}
