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
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import erjang.EAtom;

/**
 * 
 */
public class EUtil {

	static final Pattern SIMPLE_ID = Pattern
			.compile("^([a-z]|[A-Z])\\p{Alnum}*$");

	private static final String EOBJECT_DESC = CompilerVisitor.EOBJECT_TYPE.getDescriptor();

	static Map<Integer, String> signatures = new HashMap<Integer, String>();

	static String getSignature(int arity) {
		String res = signatures.get(arity);
		if (res == null) {
			StringBuffer sb = new StringBuffer("(");

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



	static String getJavaName(EAtom fun, int arity) {
		return toJavaIdentifier(fun.getName() + "/" + arity);
	}

	/**
	 * @param fun
	 * @return
	 */
	public static String getJavaName(ExtFunc fun) {
		return toJavaIdentifier(fun.mod + ":" + fun.fun + "/" + fun.no);
	}

}
