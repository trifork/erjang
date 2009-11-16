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

import java.nio.charset.Charset;
import java.util.Arrays;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import erjang.jbeam.ops.CodeAdapter;

public class EString extends ESeq implements CharSequence {

	private static final Charset ISO_LATIN_1 = Charset.forName("ISO-8859-1");
	
	private byte[] data;
	private int off;
	private int hash = -1;
	
	public EString(String value) {
		this.hash = value.hashCode();
		this.data = value.getBytes(ISO_LATIN_1);
	}

	private EString(byte[] data, int off)
	{
		this.data = data;
		this.off = off;
	}
	
	@Override
	public int hashCode() {
		if (hash == -1) { hash = stringValue().hashCode(); }
		return hash;
	}
	
	public String stringValue() {
		return new String(data, off, data.length-off, ISO_LATIN_1);
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof EString) {
			EString es = (EString) obj;
			
			int len1 = data.length-off;
			int len2 = es.data.length-es.off;
			
			if (len1 != len2) return false;
			
			for (int i = 0; i < len1; i++) {
				if (data[off+i] != es.data[es.off+i]) return false;
			}
			
			return true;

		} else if (obj instanceof String) {			
			return stringValue().equals(obj);
		}
		
		return false;
	}

	@Override
	public char charAt(int index) {
		return (char) (data[off+index] & 0xff);
	}

	@Override
	public int length() {
		return data.length-off;
	}

	@Override
	public CharSequence subSequence(final int start, final int end) {		
		if (end == length()) return new EString(data, off+start);
		return new SubSequence(start, end-start);
	}
	
	
	public class SubSequence implements CharSequence {

		private final int offset;
		private final int length;

		public SubSequence(int start, int length) {
			this.offset = start;
			this.length = length;
			EString.this.check_subseq(offset, length);
		}

		@Override
		public char charAt(int index) {
			return EString.this.charAt(offset+index);
		}

		@Override
		public int length() {
			return length;
		}

		@Override
		public CharSequence subSequence(int start, int end) {
			return new SubSequence(this.offset+start, end-start);
		}

	}

	void check_subseq(int offset, int length) {
		if (offset < 0 || length < 0 || (offset+length) > length())
			throw new IllegalArgumentException();
	}
	
	@Override
	public String toString() {
		return '"' + stringValue() + '"';
	}

	public static EString fromString(String s) {
		return new EString(s);
	}

	private static final Type ESTRING_TYPE = Type.getType(EString.class);
	private static final Type STRING_TYPE = Type.getType(String.class);

	@Override
	public Type emit_const(CodeAdapter fa) {

		Type type = ESTRING_TYPE;
		
		fa.visitLdcInsn(this.stringValue());
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, type.getInternalName(),
					"fromString", "(" + STRING_TYPE.getDescriptor() + ")" + type.getDescriptor());
		
		return type;
	}

	/* (non-Javadoc)
	 * @see erjang.ESeq#cons(erjang.EObject)
	 */
	@Override
	public EList cons(EObject h) {
		return new EList(h, this);
	}

	/* (non-Javadoc)
	 * @see erjang.ESeq#tail()
	 */
	@Override
	public ESeq tail() {
		if (off == data.length) return ENil.NIL;
		return new EString(data, off+1);
	}

	/* (non-Javadoc)
	 * @see erjang.ECons#head()
	 */
	@Override
	public EInteger head() {
		return new EInteger(data[off] & 0xff);
	}

	@Override
	public ENil testNil() {
		return length() == 0 ? ENil.NIL : null;
	}

	public ECons testNonEmptyList() {
		return length() == 0 ? null : this;
	}
	

}
