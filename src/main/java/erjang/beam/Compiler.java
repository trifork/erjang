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

import kilim.analysis.ClassInfo;
import kilim.analysis.ClassWeaver;
import kilim.analysis.Detector;
import kilim.mirrors.Mirrors;

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
		ClassWriter cw = new ClassWriter(true);

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
		
		byte[] byteArray = cw.toByteArray();
		
		ClassWeaver cwe = new ClassWeaver(byteArray, Detector.DEFAULT);
		for (ClassInfo ci : cwe.getClassInfos()) {
			String name = ci.className;
			byte[] bytes = ci.bytes;
			
			repo.store(name.replace('.', '/'), bytes);
		}
		
		// repo.store(cv.getInternalClassName(), byteArray);

	}
	
	public void compile(File file) throws IOException {
		
		// class writer, phase 4
		ClassWriter cw = new ClassWriter(true);

		// 
		CheckClassAdapter ca = new CheckClassAdapter(cw);
		
		// the java bytecode generator, phase 3
		CompilerVisitor cv = new CompilerVisitor(ca, classRepo);

		// the type analysis, phase 2
		BeamTypeAnalysis analysis = new BeamTypeAnalysis(cv);

		// the beam file reader, phase 1
		BeamFileData reader = loader.load(file);

		// go!
		reader.accept(analysis);
		
		classRepo.store(cv.getInternalClassName(), cw.toByteArray());
		
		
		ClassWeaver cwe = new ClassWeaver(cw.toByteArray(), new ErjangDetector(cv.getInternalClassName()));
		for (ClassInfo ci : cwe.getClassInfos()) {
			String name = ci.className;
			byte[] bytes = ci.bytes;
			
			classRepo.store(name.replace('.', '/'), bytes);
		}

	}
	
	static public class ErjangDetector extends Detector {

		private final String className;

		/**
		 * @param className 
		 * @param mirrors
		 */
		public ErjangDetector(String className) {
			super(Mirrors.getRuntimeMirrors());
			this.className = className;
		}

		@Override
		public int getPausableStatus(String className, String methodName,
				String desc) {
			
			if (className.startsWith(CompilerVisitor.EFUN_NAME)) {
				if (methodName.equals("invoke_tail")) return Detector.METHOD_NOT_PAUSABLE;
				if (methodName.equals("arity")) return Detector.METHOD_NOT_PAUSABLE;
				if (methodName.equals("cast")) return Detector.METHOD_NOT_PAUSABLE;
				
				if (methodName.equals("go")) return Detector.PAUSABLE_METHOD_FOUND;
				if (methodName.equals("invoke")) return Detector.PAUSABLE_METHOD_FOUND;
			}
			
			if (className.equals(this.className)) {
				if (methodName.endsWith("$tail")) return Detector.METHOD_NOT_PAUSABLE;
				if (methodName.endsWith("init>")) return Detector.METHOD_NOT_PAUSABLE;
				if (methodName.equals("module_name")) return Detector.METHOD_NOT_PAUSABLE;
				return Detector.PAUSABLE_METHOD_FOUND;
			}
			
			return super.getPausableStatus(className, methodName, desc);
		}
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
