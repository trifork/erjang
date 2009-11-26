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

package erjang.beam;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.util.CheckClassAdapter;

import com.ericsson.otp.erlang.OtpAuthException;

import erjang.EBinary;
import erjang.beam.analysis.BeamTypeAnalysis;
import erjang.m.erlang.ErlBif;

public class Compiler implements Opcodes {

	static ErlangBeamDisLoader loader;
	private ClassRepo classRepo;

	/**
	 * @param repo 
	 * @throws IOException
	 * @throws OtpAuthException
	 * 
	 */
	public Compiler(ClassRepo repo) throws OtpAuthException, IOException {
		if (loader == null)
			loader = new ErlangBeamDisLoader();
		this.classRepo = repo;
	}

	public static void compile(EBinary data, ClassRepo repo) throws IOException
	{
		// class writer, phase 4
		ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);

		// 
		CheckClassAdapter ca = new CheckClassAdapter(cw);
		
		// the java bytecode generator, phase 3
		CompilerVisitor cv = new CompilerVisitor(ca, repo);

		// the type analysis, phase 2
		BeamTypeAnalysis analysis = new BeamTypeAnalysis(cv);

		// the beam file reader, phase 1
		BeamFileData reader = loader.load(data.getByteArray());

		try {
			// go!
			reader.accept(analysis);
		} catch (Error e) {
			e.printStackTrace();
		}
		
		repo.store(cv.getInternalClassName(), cw.toByteArray());

	}
	
	public void compile(File file) throws IOException {
		
		// class writer, phase 4
		ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);

		// 
		CheckClassAdapter ca = new CheckClassAdapter(cw);
		
		// the java bytecode generator, phase 3
		CompilerVisitor cv = new CompilerVisitor(ca, classRepo);

		// the type analysis, phase 2
		BeamTypeAnalysis analysis = new BeamTypeAnalysis(cv);

		// the beam file reader, phase 1
		BeamFileData reader = loader.load(file);

		try {
			// go!
			reader.accept(analysis);
		} finally {
			classRepo.store(cv.getInternalClassName(), cw.toByteArray());
		}
		// get byte code data
		// classRepo.store(cv.getInternalClassName(), cw.toByteArray());
	}

	public static String moduleClassName(String moduleName) {
		String cn = EUtil.toJavaIdentifier(moduleName);
	
		String base = "erjang/m/" + cn + "/" + cn;
		
		return base;
		
	}

	public static void main(String[] args) throws Exception {

		File out_dir = new File("out");
		ClassRepo repo = new DirClassRepo(out_dir);
		Compiler cc = new Compiler(repo);

		for (int i = 0; i < args.length; i++) {
			File infile = new File(args[i]);
			cc.compile(infile);
		}

		repo.close();		
	}


}
