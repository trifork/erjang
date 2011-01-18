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
import java.io.IOException;
import java.util.Arrays;

/** Common definitions and function.
 */
public abstract class TestUtil {

	static final String OTP_HOME = ErjangConfig.getString("erjang.otp.root");
	static final String ERTS_VSN = ErjangConfig.getString("erjang.erts.version");
	static final String ERLC_PRG = OTP_HOME + File.separator + "bin" + File.separator + "erlc";
	static final String ERL_PRG  = OTP_HOME + File.separator + "bin" + File.separator + "erl";

    static final String EJ_PRG  = "./ej";
	static final String TEST_BEAM_DIR = "target/test-beam";

	static final String TRIQ_HOME = ErjangConfig.getString("erjang.triq.root");


    public static String get_ej() {
        String ej = EJ_PRG;

        // on Windows, we need to wrap ej into a .bat wrapper
        if (System.getProperty("os.name").startsWith("Windows")) {
            ej = "ej.bat";
        }

        return ej;
    }


	public static void erl_compile(String fileName) throws Exception {
		execGetOutput(new String[] {ERLC_PRG,
					    "-o", TEST_BEAM_DIR,
					    "-pa", TRIQ_HOME + File.separator + "ebin",
					    "-I", TRIQ_HOME + File.separator + "include",
					    fileName});
	}

	public static String execGetOutput(String[] cmd) throws Exception {
	    byte[] output = execGetBinaryOutput(cmd);
	    return output.toString();
	}
	public static byte[] execGetBinaryOutput(String[] cmd) throws Exception {
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

	public static String trimExtension(String fileName) {
	    int i = fileName.lastIndexOf('.');
	    if (i>=0) fileName = fileName.substring(0,i);
	    return fileName;
	}


    public static Process startErlProcess(String nodeName, String hostName) throws IOException {
	String[] cmd = new String[] {ERL_PRG, "-noinput",
				     nodeNameSwitch(hostName), nodeName + "@" + hostName,
				     "-pa", TestUtil.TEST_BEAM_DIR,
				     "-pa", TRIQ_HOME + File.separator + "ebin",
				     "-sasl", "sasl_error_logger", "false", // Prevent SASL from polluting stdout
				     "-noinput"
	};

	Runtime rt = Runtime.getRuntime();
	Process p = rt.exec(cmd);
	return p;
    }

    public static void stopProcess(Process p) {
	p.destroy();
    }

    public static String nodeNameSwitch(String hostName) {
	// Use -name if qualified, -sname if not:
	return (hostName.indexOf('.') >= 0) ? "-name" : "-sname";
    }

}
