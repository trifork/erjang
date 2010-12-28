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

import java.io.IOException;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import erjang.driver.IO;
import erjang.m.ets.EMatchContext;
import erjang.m.ets.EPattern;
import erjang.m.ets.ETermPattern;

public final class EAtom extends EObject implements CharSequence {

	public EAtom testAtom() {
		return this;
	}

	@Override
	public boolean match(ETermPattern matcher, EMatchContext r) {
		return matcher.match(this, r);
	}

	@Override
	public ETermPattern compileMatch(Set<Integer> out) {
		return EPattern.compilePattern(this, out);
	}



	public EAtom testBoolean() {
		if ( this==ERT.TRUE || this == ERT.FALSE) return this;
		return null;
	}

	private final String value;
	public final int hash;

	@Override
	int compare_same(EObject rhs) {
		return compareTo((EAtom) rhs);
	}
	
	private static ConcurrentHashMap<String, EAtom> interns = new ConcurrentHashMap<String, EAtom>();

	private EAtom(String name) {
		this.value = name;
		this.hash = name.hashCode();
	}

	@Override
	int cmp_order() {
		return CMP_ORDER_ATOM;
	}
	
	static Pattern ATOM = Pattern.compile("[a-z_]([a-z]|[A-Z]|@|_|[0-9])*");

	@Override
	public String toString() {
		if (ATOM.matcher(value).matches()) {
			return value;
		} else {
			return "'" + encode_escapes(value) + "'";
		}
	}

	/**
	 * @param value2
	 * @return
	 */
	private String encode_escapes(CharSequence seq) {
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < seq.length(); i++) {
			char ch = seq.charAt(i);
			if ((ch >= 0x20 && ch <= 0x7f) && ch != '\'' && ch != 0x7f) {
				sb.append(ch);
			} else {
				switch (ch) {
				case '\t':
					sb.append("\\t");
					break;
				case '\n':
					sb.append("\\n");
					break;
				case '\r':
					sb.append("\\r");
					break;
				case '\f':
					sb.append("\\f");
					break;
				case '\b':
					sb.append("\\b");
					break;
				case '\'':
					sb.append("\\'");
					break;
				default:
					sb.append("\\");
					sb.append('x');
					if (ch < 0x10) sb.append('0');
					sb.append(Integer.toHexString(ch).toUpperCase());
				}
			}
		}
		return sb.toString();
	}

	public boolean isAtom(String name) {
		return interns.contains(name);
	}

	@Override
	public boolean equals(Object obj) {
		return this == obj;
	}

	@Override
	public int hashCode() {
		return hash;
	}

	public static EAtom intern(String name) {
		
		
		
		EAtom res = interns.get(name);
		if (res == null) {

			boolean changed = false;
			char[] data = name.toCharArray();
			for (int i = 0; i < data.length; i++) 
			{
				if ((data[i] & 0xff00) != 0) changed = true;
				data[i] = (char) (data[i] & 0xff);
			}

			if (changed)
				name = new String(data);
			
			EAtom new_val = new EAtom(name);
			do {
				res = interns.putIfAbsent(name, new_val);
			} while (res == null);
		}

		return res;
	}

	public static EAtom existing_atom(String name) {
		EAtom res = interns.get(name);
		if (res == null) {
			throw ERT.badarg(EString.fromString(name));
		}

		return res;
	}


	@Override
	public char charAt(int index) {
		return value.charAt(index);
	}

	@Override
	public int length() {
		return value.length();
	}

	@Override
	public CharSequence subSequence(int start, int end) {
		return value.subSequence(start, end);
	}

	public String getName() {
		return value;
	}

	private static final Type EATOM_TYPE = Type.getType(EAtom.class);
	private static final Type STRING_TYPE = Type.getType(String.class);

	@Override
	public org.objectweb.asm.Type emit_const(MethodVisitor fa) {

		Type type = EATOM_TYPE;

		fa.visitLdcInsn(value);
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, type.getInternalName(),
				"intern", "(" + STRING_TYPE.getDescriptor() + ")"
						+ type.getDescriptor());

		return type;
	}

	/**
	 * @param other
	 * @return
	 */
	public int compareTo(EAtom other) {
		if (this == other) return 0;
		return value.compareTo(other.value);
	}
	
	/* (non-Javadoc)
	 * @see erjang.EObject#equalsExactly(erjang.EObject)
	 */
	@Override
	public boolean equalsExactly(EObject rhs) {
		return rhs == this;
	}

	/**
	 * @param buf
	 * @return
	 */
	public static EAtom intern(byte[] buf) {
		char[] data = new char[buf.length];
		for (int i = 0; i < data.length; i++) {
			data[i] = (char) (buf[i] & 0xff);
		}
		String str = new String(data);
		return intern(str);
	}

	public static EAtom read(EInputStream ei) throws IOException {
		return ei.read_atom();
	}

	@Override
	public void encode(EOutputStream eos) {
		eos.write_atom(this.value);
	}
}
