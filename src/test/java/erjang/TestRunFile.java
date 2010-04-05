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

import erjang.beam.Compiler;
import erjang.EObject;
import erjang.EAtom;
import erjang.EList;
import erjang.ETuple;
import erjang.EBinary;
import erjang.beam.DirClassRepo;

import erjang.m.erlang.ErlConvert;

import junit.framework.Test;
import junit.framework.TestResult;
import junit.framework.Assert;
import junit.framework.AssertionFailedError;
import kilim.ExitMsg;
import kilim.Mailbox;

/**
 * 
 */
public class TestRunFile implements Test {

	static final String OTP_HOME = "/Users/krab/Projects/otp";
	static final String ERLC_PRG = "/sw/bin/erlc";
	static final String ERL_PRG  = "/sw/bin/erl";

	static final EAtom ERJANG_ATOM = EAtom.intern("erjang");
	static final EAtom ERLANG_ATOM = EAtom.intern("erlang");
	static final EAtom RUN_WRAPPER_ATOM = EAtom.intern("run_wrapper");
	static final String RUN_WRAPPER_HOME = "src/test/erl";
	static final String BEAM_DIR = "target/test-beam";

	private final File file;

	final static File repoDir = new File("target/compiled");
	final static DirClassRepo repo = new DirClassRepo(repoDir);

	/**
	 * @param file
	 */
	public TestRunFile(File file) {
		this.file = file;
		
		System.setProperty("erjpath", OTP_HOME + "/erts/preloaded/ebin"
							+ ":" + OTP_HOME + "/lib/stdlib/ebin");
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
			erl_compile(RUN_WRAPPER_HOME+"/run_wrapper.erl");
			erl_compile(file.getAbsolutePath());
			EObject expected = erl_run(file);

			File wrapperBeamFile = new File(BEAM_DIR, "run_wrapper.beam");
			File beamFile = new File(BEAM_DIR,
						 trimExtension(file.getName())+".beam");

			if (! EModuleManager.module_loaded(ERLANG_ATOM)) load("erlang");
			if (! EModuleManager.module_loaded(RUN_WRAPPER_ATOM)) load(wrapperBeamFile);
			load(beamFile);

			String moduleName = trimExtension(file.getName());
			EAtom module = EAtom.intern(moduleName);

			EProc p = new EProc(null, RUN_WRAPPER_ATOM, RUN_WRAPPER_ATOM, EList.make(EList.make(ERJANG_ATOM, module)));
			ERT.run(p);
			
	        Mailbox<ExitMsg> mb = new Mailbox<ExitMsg>();
	        p.informOnExit(mb);
	        ExitMsg exit = mb.getb(20*1000); // 20sec
	        Assert.assertNotNull("process timed out?", exit);
	        
			EObject actual = (EObject) p.exit_reason;
			Assert.assertEquals(expected, actual);
		} catch (AssertionFailedError e) {
		    result.addFailure(this, e);
		} catch (Throwable e) {
			result.addError(this, e);
		}
		result.endTest(this);
	}

	private static void load(String module) throws Exception {
		EModuleLoader.find_and_load_module(module);
	}

	private static void load(File file) throws Exception {
		Compiler compiler = new Compiler(repo);
		compiler.compile(file);
		String moduleName = trimExtension(file.getName());
		EModuleLoader.load_compiled_module(moduleName, repoDir.toURL());
	}

	private EObject erl_run(File file) throws Exception {
		String moduleName = trimExtension(file.getName());
		String[] cmd = new String[] {ERL_PRG, "-noinput",
					     "-pa", BEAM_DIR,
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
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		ByteArrayOutputStream err    = new ByteArrayOutputStream();
		byte[] buf = new byte[1024];
		InputStream stdout = p.getInputStream();
		InputStream stderr = p.getInputStream();
		while (true) {
			int len;
			try {
				boolean change;
				do {
					change = false;
					while ((len = stdout.read(buf)) > 0) {
						output.write(buf, 0, len);
						change = true;
					}
					while ((len = stderr.read(buf)) > 0) {
						err.write(buf, 0, len);
						change = true;
					}
				} while (change);
				int exitCode = p.waitFor();
				if (exitCode != 0) {
					System.err.println("Exitcode="+exitCode+" for "+cmd[0]);
					System.err.println("Err//output="+err+"//"+output);
				}
				assert(exitCode == 0);
				break;
			} catch (InterruptedException ie) {
				System.err.println("IE="+ie);
			}
		}
		return output.toByteArray();
	}


	private static String trimExtension(String fileName) {
	    int i = fileName.lastIndexOf('.');
	    if (i>=0) fileName = fileName.substring(0,i);
	    return fileName;
	}
}
