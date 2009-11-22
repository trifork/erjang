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

	static final String OTP_HOME = "/Users/krab/Systems/otp_src_R13B02-1";

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for erjang");
		//$JUnit-BEGIN$
		find_beam_files(suite, new File(OTP_HOME));
		//$JUnit-END$
		return suite;
	}

	
	static void find_beam_files(TestSuite suite, File dir) {
		
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
}
