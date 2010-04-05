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
import java.io.IOException;

import com.ericsson.otp.erlang.OtpAuthException;

import erjang.beam.Compiler;
import erjang.beam.JarClassRepo;
import erjang.beam.BeamLoader;
import erjang.beam.loader.ErjangBeamDisLoader;

/**
 * Beam -> Java compiler
 */
public class ErjC {

	public static void main(String[] args) throws OtpAuthException, IOException {
		BeamLoader beamParser = new ErjangBeamDisLoader();

		for (int i = 0; i < args.length; i++) {
			if (args[i].endsWith(".beam")) {
				File in = new File(args[i]);
				int idx = args[i].lastIndexOf('.');
				File out = new File(args[i].substring(0, idx) + ".jar");
				JarClassRepo jcp = new JarClassRepo(out);

				System.out.println("compiling "+in+" -> "+out+" ...");
				new Compiler(jcp).compile(in, beamParser);

				jcp.close();
			}
		}
	}
}
