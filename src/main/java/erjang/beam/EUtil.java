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
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.FileInputStream;
import java.io.IOException;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import org.objectweb.asm.Type;

import erjang.EAtom;
import erjang.EBinary;
import erjang.beam.repr.ExtFun;

/**
 * 
 */
public class EUtil {

	static final Pattern SIMPLE_ID = Pattern
			.compile("^([a-z]|[A-Z])\\p{Alnum}*$");

	private static final String EOBJECT_DESC = CompilerVisitor.EOBJECT_TYPE
			.getDescriptor();
	private static final String EPROC_DESC = CompilerVisitor.EPROC_TYPE
			.getDescriptor();

	static Map<Integer, String> signatures = new HashMap<Integer, String>();
	static Map<Integer, String> noproc_signatures = new HashMap<Integer, String>();

	public static String getSignature(int arity, boolean withProc) {

		Map<Integer, String> signatures = withProc ? noproc_signatures
				: EUtil.signatures;

		String res = signatures.get(arity);
		if (res == null) {
			StringBuffer sb = new StringBuffer("(");

			if (withProc) {
				sb.append(EPROC_DESC);
			}

			for (int i = 0; i < arity; i++) {
				sb.append(EOBJECT_DESC);
			}

			sb.append(")");
			sb.append(EOBJECT_DESC);

			signatures.put(arity, res = sb.toString());
		}

		return res;
	}

	static String toJavaIdentifier(EAtom name) {
		return toJavaIdentifier(name.getName());
	}

	/** encode any char sequence into a valid java identifier */
	static String toJavaIdentifier(String name) {
		if (SIMPLE_ID.matcher(name).matches())
			return name;

		StringBuilder sb = new StringBuilder();
		for (char c : name.toCharArray()) {

			if (c == '$') {
				sb.append("$$");
				continue;
			} else if (sb.length() == 0) {
				if (Character.isJavaIdentifierStart(c)) {
					sb.append(c);
					continue;
				}
			} else if (Character.isJavaIdentifierPart(c)) {
				sb.append(c);
				continue;
			}

			try {
				sb.append('$');

				ByteArrayOutputStream baro = new ByteArrayOutputStream(5);
				DataOutputStream dao = new DataOutputStream(baro);
				dao.writeUTF(new String(new char[] { c }));
				dao.close();

				byte[] data = baro.toByteArray();

				writeHexByte(sb, 0xff & data[2]);

				if (data.length > 3)
					writeHexByte(sb, 0xff & data[3]);

				if (data.length > 4)
					writeHexByte(sb, 0xff & data[4]);

			} catch (IOException ex) {
				throw new Error();
			}

		}

		return sb.toString();
	}

	private static void writeHexByte(StringBuilder sb, int b) {
		if (b < 0x10) {
			sb.append('0');
		}

		sb.append(Integer.toHexString(b).toUpperCase());
	}

	public static String plen(Object o) {
		String s = String.valueOf(o);
		StringBuilder sb = new StringBuilder("_");
		writeHexByte(sb, s.length());
		sb.append(s);
		return s.toString();
	}

	public static String getJavaName(EAtom fun, int arity) {
		String fname = fun.getName();
		if (fname.indexOf("__") == -1) {
			return toJavaIdentifier(fun.getName() + "__" + arity);
		} else {
			return toJavaIdentifier(plen(fun.getName()) + "__" + arity);
		}
	}

	/**
	 * @param fun
	 * @return
	 */
	public static String getJavaName(ExtFun fun) {
		return toJavaIdentifier(fun.mod) + "__" + getJavaName(fun.fun, fun.arity);
	}

	/**
	 * @param selfType
	 * @param efun
	 * @return
	 */
	public static String getFunClassName(Type self_type, ExtFun efun) {
		return self_type.getInternalName() + "$FN_"
				+ getJavaName(efun.fun, efun.arity);
	}

	public static String getFunClassName(Type self_type, ExtFun efun, int freevars) {
		return self_type.getInternalName() + "$FN_"
				+ getJavaName(efun.fun, efun.arity-freevars);
	}

	/**
	 * @param arity
	 * @param proc
	 * @param returnType
	 * @return
	 */
	public static String getSignature(int arity, boolean withProc,
			Type returnType) {

		StringBuffer sb = new StringBuffer("(");

		if (withProc) {
			sb.append(EPROC_DESC);
		}

		for (int i = 0; i < arity; i++) {
			sb.append(EOBJECT_DESC);
		}

		sb.append(")");
		sb.append(returnType.getDescriptor());

		return sb.toString();
	}

	/**
	 * @param methodName
	 * @return
	 */
	public static String decodeJavaName(String methodName) {

		int idx;
		if ((idx = methodName.indexOf('$')) == -1)
			return methodName;

		StringBuilder sb = new StringBuilder();

		int start = 0;
		while (idx != -1) {
			sb.append(methodName.substring(start, idx));
			if (methodName.charAt(idx+1) == '$') {
				sb.append('$');
				start = idx + 2;
			} else {
				String hex = methodName.substring(idx + 1, idx + 3);
				char chval;
				try {
					chval = (char) Integer.parseInt(hex, 16);
				} catch (NumberFormatException e) {
					chval = '?';
				}
				sb.append(chval);
				start = idx + 3;
			}
			idx = methodName.indexOf('$', start);
		}

		sb.append(methodName.substring(start));

		return sb.toString();
	}

	public static EBinary readFile(File file) throws IOException {
		int length = (int) file.length();
		byte[] data = new byte[length];
		FileInputStream fi = new FileInputStream(file);
		try {
			ByteArrayOutputStream bo = new ByteArrayOutputStream();
			int read = 0;
			while (read < length) {
				read += fi.read(data, read, length-read);
			}
		} finally {
			fi.close();
		}

		EBinary bin = new EBinary(data);
		return bin;
	}
}
