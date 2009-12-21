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

import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
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
import erjang.beam.analysis.BeamTypeAnalysis;

public class Compiler implements Opcodes {

	static BeamLoader xloader;
	private ClassRepo classRepo;

	static BeamLoader getLoader() {
		if (xloader == null) {
			try {
				xloader = new ErlangBeamDisLoader();
			} catch (OtpAuthException e) {
				throw new Error(e);
			} catch (IOException e) {
				throw new Error(e);
			}
		}
		
		return xloader;
	}

	/**
	 * @param repo
	 * @throws IOException
	 * @throws OtpAuthException
	 * 
	 */
	public Compiler(ClassRepo repo) throws OtpAuthException, IOException {
		this.classRepo = repo;
	}

	public static void compile(EBinary data, ClassRepo repo) throws IOException {
		
		
		// class writer, phase 4
		ClassWriter cw = new ClassWriter(true);

		// 
		CheckClassAdapter ca = new CheckClassAdapter(cw);

		// the java bytecode generator, phase 3
		CompilerVisitor cv = new CompilerVisitor(ca, repo);

		// the type analysis, phase 2
		BeamTypeAnalysis analysis = new BeamTypeAnalysis(cv);

		// the beam file reader, phase 1
		BeamFileData reader = getLoader().load(data.getByteArray());

		try {
			// go!
			reader.accept(analysis);
		} catch (Error e) {
			e.printStackTrace();
		}

		byte[] byteArray = cw.toByteArray();

		/* 
		 // uncomment this block to emit pre-kilim code [for debugging]	
		if (cv.getInternalClassName().indexOf("code_server") != -1) {
			repo.store(cv.getInternalClassName(), byteArray);
			return;
		}
		*/
		
		ClassWeaver cwe = new ClassWeaver(byteArray, new ErjangDetector(
				cv.getInternalClassName()));
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
		BeamFileData reader = getLoader().load(file);

		// go!
		reader.accept(analysis);

		// classRepo.store(cv.getInternalClassName(), cw.toByteArray());

		ClassWeaver cwe = new ClassWeaver(cw.toByteArray(), new ErjangDetector(
				cv.getInternalClassName()));
		for (ClassInfo ci : cwe.getClassInfos()) {
			String name = ci.className;
			byte[] bytes = ci.bytes;

			// System.out.println("storing "+name+" in "+classRepo);

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
		for (int i = 0; i < args.length; i++) {

			if (args[i].endsWith(".beam")) {
				File in = new File(args[i]);
				if (!in.exists() || !in.isFile() || !in.canRead())
					throw new IOException("bad permissions for " + in);

				int idx = args[i].lastIndexOf('.');
				int idx0 = args[i].lastIndexOf(File.separator);

				String shortName = args[i].substring(idx0 + 1, idx);

				File out = new File(out_dir, shortName + "-"
						+ Long.toHexString(crcFile(in)) + ".jar");
				JarClassRepo jcp = new JarClassRepo(out);

				System.out.println("compining " + in + " -> " + out + " ...");
				new Compiler(jcp).compile(in);

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

	public static File find_and_compile(String module) throws IOException {
		
		File input = findBeamFile(module);
		if (input == null)
			throw new FileNotFoundException(module);
		byte[] data = new byte[(int) input.length()];
		FileInputStream fi = new FileInputStream(input);
		try {
			int pos = 0, left = data.length;
			do {
				int val = fi.read(data, pos, left);
				if (val == -1)
					throw new EOFException();
				pos += val;
				left -= val;
			} while (left > 0);

		} finally {
			fi.close();
		}

		EBinary eb = new EBinary(data);

		
		
		return compile(module, eb);

	}

	/**
	 * @param module
	 * @return
	 */
	private static File findBeamFile(String module) {
		String n = module;

		for (File e : loadPath) {
			File beam = new File(e, n + ".beam");
			if (beam.exists())
				return beam;
		}

		return null;
	}

	static File[] loadPath;

	static {
		ArrayList<File> lp = new ArrayList<File>();
		String sys_path = System.getenv("ERJ_PATH");
		if (sys_path != null)
			add(lp, sys_path);

		String path = System.getProperty("erjpath", ".");
		add(lp, path);

		loadPath = lp.toArray(new File[lp.size()]);
	}

	private static void add(ArrayList<File> out, String path) {
		for (String s : path.split(":")) {
			File elem = new File(s);
			if (elem.exists() && elem.isDirectory()) {
				out.add(elem);
			}
		}
	}

	public static File compile(String name, EBinary beam_data) throws IOException {

		long crc = beam_data.crc();

		File jarFile = new File(erjdir(), name + "-" + Long.toHexString(crc)
				+ ".jar");

		if (!jarFile.exists()) {
			JarClassRepo repo = new JarClassRepo(jarFile);

			System.out.print("[compiling "); 
			System.out.print(name);
			long before = System.currentTimeMillis();

			try {
				compile(beam_data, repo);

				repo.close();
				repo = null;
			} finally {
				if (repo != null) {
					try {repo.close();
					jarFile.delete();
					} catch (Exception e) {}
				}
			}
			
			System.out.print(":"+(System.currentTimeMillis()-before)+"ms]");
		}

		return jarFile;
	}

	static File erjdir() throws IOException {
		File dir = new File(".erj");
		if (!dir.exists()) {
			if (!dir.mkdirs())
				throw new IOException("cannot create " + dir);

		} else if (!dir.canWrite()) {
			throw new IOException("cannot write to " + dir);
		}

		return dir;
	}

}
