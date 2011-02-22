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

import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.CRC32;
import java.util.zip.CheckedInputStream;

import kilim.analysis.ClassInfo;
import kilim.analysis.ClassWeaver;
import kilim.analysis.Detector;
import kilim.mirrors.ClassMirrorNotFoundException;
import kilim.mirrors.Mirrors;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.util.CheckClassAdapter;

import com.ericsson.otp.erlang.OtpAuthException;

import erjang.EBinary;
import erjang.EFun;
import erjang.EObject;
import erjang.ERT;
import erjang.ETuple;
import erjang.beam.analysis.BeamTypeAnalysis;
import erjang.ErjangCodeCache;

import erjang.beam.loader.ErjangBeamDisLoader;

public class Compiler implements Opcodes {
	private ClassRepo classRepo;

	/**
	 * @param repo
	 * @throws IOException
	 * @throws OtpAuthException
	 * 
	 */
	public Compiler(ClassRepo repo) throws OtpAuthException, IOException {
		this.classRepo = repo;
	}

	public static void compile(BeamFileData data, ClassRepo repo) throws IOException {
		// reset thread-local data 
		ClassWeaver.reset();
		
		// class writer, phase 4
		ClassWriter cw = new ClassWriter(true);

		// class checker, optional phase
		//CheckClassAdapter ca = new CheckClassAdapter(cw);

		// the java bytecode generator, phase 3
		CompilerVisitor cv = new CompilerVisitor(cw, repo);

		// the type analysis, phase 2
		BeamTypeAnalysis analysis = new BeamTypeAnalysis(cv);

		// the module analyzer, phase 1 (not chained to phase 2)
		ModuleAnalyzer ma = new ModuleAnalyzer();
		data.accept(ma);

		cv.setFunInfos(ma.getFunInfos());

		try {
			// go!
			data.accept(analysis);
		} catch (Error e) {
			e.printStackTrace();
		}

		byte[] byteArray = cw.toByteArray();

		 // to emit pre-kilim code [for debugging]	
		repo.store("raw/"+cv.getInternalClassName(), byteArray);

		boolean written = false;
		ClassWeaver cwe = new ClassWeaver(byteArray, new ErjangDetector(
				cv.getInternalClassName(), cv.non_pausable_methods));
		for (ClassInfo ci : cwe.getClassInfos()) {
			String name = ci.className;
			byte[] bytes = ci.bytes;

			String iname = name.replace('.', '/');
			if (iname.equals(cv.getInternalClassName())) {
				written = true;
			}

			repo.store(iname, bytes);
		}

		if (!written) {
			// no pausable functions in module!
			repo.store(cv.getInternalClassName(), byteArray);
		}
	}

	public void compile(File file, BeamLoader beam_parser) throws IOException {
		EBinary eb = EUtil.readFile(file);
		BeamFileData bfd = beam_parser.load(eb.getByteArray());
		compile(bfd, this.classRepo);
	}

	static public class ErjangDetector extends Detector {

		private final String className;
		private final Set<String> nonPausableMethods;

		/**
		 * @param className
		 * @param nonPausableMethods 
		 * @param mirrors
		 */
		public ErjangDetector(String className, Set<String> nonPausableMethods) {
			super(Mirrors.getRuntimeMirrors());
			this.className = className;
			this.nonPausableMethods = nonPausableMethods;
		}

		
		static Pattern FUN = Pattern.compile("^erjang\\.m\\..*\\$FN_.*__([0-9])+$");
		
		/* (non-Javadoc)
		 * @see kilim.analysis.Detector#getSuperClasses(java.lang.String)
		 */
		@Override
		public ArrayList<String> getSuperClasses(String cc)
				throws ClassMirrorNotFoundException {
			Matcher m = FUN.matcher(cc);
			if (m.matches()) {
				int arity = Integer.parseInt(m.group(1));
				
				ArrayList<String> result = new ArrayList<String>();
				result.add(EFun.class.getName()+arity);
				result.add(EFun.class.getName());
				result.add(EObject.class.getName());
				result.add(Object.class.getName());
				return result;
			}
			
			return super.getSuperClasses(cc);
		}
		
		@Override
		public int getPausableStatus(String className, String methodName,
				String desc) {

			// System.out.println("status? "+className+"#"+methodName+""+desc);

			if (className.startsWith(CompilerVisitor.ETUPLE_NAME)) {
				return Detector.METHOD_NOT_PAUSABLE;
			}

			if (className.startsWith(CompilerVisitor.EFUN_NAME)) {
				if (methodName.equals("go"))
					return Detector.PAUSABLE_METHOD_FOUND;
				if (methodName.equals("invoke"))
					return Detector.PAUSABLE_METHOD_FOUND;

				return Detector.METHOD_NOT_PAUSABLE;
			}

			if (className.equals(this.className)) {
				if (methodName.endsWith("$tail"))
					return Detector.METHOD_NOT_PAUSABLE;
				if (methodName.endsWith("init>"))
					return Detector.METHOD_NOT_PAUSABLE;
				if (methodName.equals("module_name"))
					return Detector.METHOD_NOT_PAUSABLE;
				if (nonPausableMethods.contains(methodName)) 
					return Detector.METHOD_NOT_PAUSABLE;
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

		File out_dir = new File("target/compiled");
		out_dir.mkdirs();
		BeamLoader beamParser = new ErjangBeamDisLoader();

		for (int i = 0; i < args.length; i++) {
			if (args[i].endsWith(".beam")) {
				File in = new File(args[i]);
				if (!in.exists() || !in.isFile() || !in.canRead())
					throw new IOException("bad permissions for " + in);

				int idx = args[i].lastIndexOf('.');
				int idx0 = args[i].lastIndexOf(File.separator);

				String shortName = args[i].substring(idx0 + 1, idx);
				File out = new File(out_dir, ErjangCodeCache.moduleJarFileName(shortName, crcFile(in))); // TODO: don't use the code cache location.
				JarClassRepo jcp = new JarClassRepo(out);

				System.out.println("compiling " + in + " -> " + out + " ...");
				new Compiler(jcp).compile(in, beamParser);

				jcp.close();
			}
		}
	}

	private static long crcFile(File file) throws IOException {

		CheckedInputStream cis = null;
		long fileSize = 0;
		cis = new CheckedInputStream(new FileInputStream(file), new CRC32());
		try {
			byte[] buf = new byte[4 * 1024];
			while (cis.read(buf) >= 0)
				;

			return cis.getChecksum().getValue();
		} finally {
			cis.close();
		}
	}
}
