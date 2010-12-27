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

import java.io.ByteArrayOutputStream;
import java.io.CharArrayWriter;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.List;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class EBigString extends ESeq implements CharSequence {

	private static final Charset ISO_LATIN_1 = Charset.forName("ISO-8859-1");

	private static final EBigString EMPTY = new EBigString("");

	final char[] data;
	final int off;
	private int hash = -1;

	public EBigString(String value) {
		this.hash = value.hashCode();
		this.data = value.toCharArray();
		this.off = 0;
	}

	public EBigString testBigString() {
		return this;
	}

	EBigString(char[] data, int off) {
		this.data = data;
		this.off = off;
	}

	EBigString(char[] data, int off, int len) {
		if (data.length == off+len) {
			this.data = data;
			this.off = off;
		} else {
			this.data = new char[len];
			this.off = 0;
			System.arraycopy(data, off, this.data, 0, len);
		}
	}

	/**
	 * @param array
	 * @param arrayOffset
	 * @param len
	 */
	public static EBigString make(char[] array, int arrayOffset, int len) {
		if (len == array.length - arrayOffset) {
			return new EBigString(array, arrayOffset);
		} else {
			char[] copy = new char[len];
			System.arraycopy(array, arrayOffset, copy, 0, len);
			return new EBigString(copy, 0);
		}
	}

	/**
	 * @param list
	 */
	public static EBigString make(ECons list) {
		EBigString s;
		if ((s = list.testBigString()) != null) {
			return s;

		} else if (list.isNil()) {
			return EBigString.EMPTY;

		} else {
			CharArrayWriter barr = new CharArrayWriter();

			EObject tail = list;
			while ((list = tail.testNonEmptyList()) != null) {

				EObject head = list.head();

				ESmall intval;
				if ((intval = head.testSmall()) == null) {
					throw ERT.badarg();
				}

				int byteValue = intval.value & 0xffff;
				if (intval.value != byteValue) {
					throw ERT.badarg();
				}

				barr.write(byteValue);
				tail = list.tail();
			}

			return new EBigString(barr.toCharArray(), 0);
		}
	}

	@Override
	public int hashCode() {
		if (hash == -1) {
			hash = stringValue().hashCode();
		}
		return hash;
	}

	public String stringValue() {
		return new String(data, off, data.length - off);
	}

	public boolean equalsExactly(EObject rhs) {
		ENil nil;
		int length = length();

		if ((nil = rhs.testNil()) != null) {
			return length == 0;
		}

		EBigString str;
		if ((str = rhs.testBigString()) != null) {
			//TODO: We could treat all CharSequences here,
			// or else use java.util.Arrays.equals(). --ESS
			EBigString es = str;

			if (length != es.length())
				return false;

			for (int i = 0; i < length; i++) {
				if (charAt(i) != es.charAt(i))
					return false;
			}

			return true;

		}

		ESeq seq;
		if ((seq = rhs.testSeq()) != null) {
			for (int i = 0; i<length; i++) {
				if (seq.testNil() != null)
					return false;

				if (!seq.head().equalsExactly(new ESmall(charAt(i)))) {
					return false;
				}

				seq = seq.tail();
			}
			return seq.isNil();
		}

		return false;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof String) {
			return ((String) obj).equals(stringValue());
		}

		if (!(obj instanceof EObject)) {
			return false;
		}

		return equalsExactly((EObject) obj);
	}

	@Override
	public char charAt(int index) {
		return data[off + index];
	}

	@Override
	public int length() {
		return data.length - off;
	}

	@Override
	public CharSequence subSequence(final int start, final int end) {
		if (end == length())
			return new EBigString(data, off + start);
		return new SubSequence(start, end - start);
	}

	public class SubSequence implements CharSequence {

		private final int offset;
		private final int length;

		public SubSequence(int start, int length) {
			this.offset = start;
			this.length = length;
			EBigString.this.check_subseq(offset, length);
		}

		@Override
		public char charAt(int index) {
			return EBigString.this.charAt(offset + index);
		}

		@Override
		public int length() {
			return length;
		}

		@Override
		public CharSequence subSequence(int start, int end) {
			return new SubSequence(this.offset + start, end - start);
		}

	}

	void check_subseq(int offset, int length) {
		if (offset < 0 || length < 0 || (offset + length) > length())
			throw new IllegalArgumentException();
	}

	@Override
	public String toString() {
		return '"' + stringValue() + '"';
	}

	public static EBigString fromString(String s) {
		return new EBigString(s);
	}

	private static final Type ESTRING_TYPE = Type.getType(EBigString.class);
	private static final Type STRING_TYPE = Type.getType(String.class);

	@Override
	public Type emit_const(MethodVisitor fa) {

		Type type = ESTRING_TYPE;

		fa.visitLdcInsn(this.stringValue());
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, type.getInternalName(),
				"fromString", "(" + STRING_TYPE.getDescriptor() + ")"
						+ type.getDescriptor());

		return type;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.ESeq#cons(erjang.EObject)
	 */
	@Override
	public EList cons(EObject h) {
		return new EList(h, this);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.ESeq#tail()
	 */
	@Override
	public ESeq tail() {
		if (off == data.length)
			return ERT.NIL;
		return new EBigString(data, off + 1);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.ECons#head()
	 */
	@Override
	public ESmall head() {
		return new ESmall(data[off] & 0xff);
	}

	@Override
	public ENil testNil() {
		return length() == 0 ? ERT.NIL : null;
	}

	@Override
	public ESeq testSeq() {
		return this;
	}

	public ECons testNonEmptyList() {
		return length() == 0 ? null : this;
	}

	public ECons testCons() {
		return this;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.ECons#compare_same(erjang.EObject)
	 */
	@Override
	int compare_same(EObject rhs) {
		if (rhs.isNil())
			return 1;

		int length = length();

		EBigString str;
		if ((str = rhs.testBigString()) != null) {
			EBigString es = str;
			int length2 = str.length();
			int limit = Math.min(length, length2);
			for (int i = 0; i < limit; i++) {
				char ch1 = charAt(i);
				char ch2 = es.charAt(i);
				if (ch1 < ch2)
					return -1;
				if (ch1 > ch2)
					return 1;
			}

			if (length > length2)
				return 1;
			if (length < length2)
				return -1;
			return 0;

		}

		ECons seq;
		if ((seq = rhs.testCons()) != null) {

			int i = 0;

			while (i < length) {

				if ((seq.testNil()) != null) {
					return -1; // I AM SHORTER
				}

				int cmp = (new ESmall(charAt(i++))).erlangCompareTo(seq.head());
				if (cmp != 0)
					return cmp;

				EObject res = seq.tail();

				if ((seq = res.testCons()) != null) {
					continue;
				}

				return -res.erlangCompareTo(new EBigString(data, i));
			}

		}

		return -rhs.erlangCompareTo(this);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.ECons#prepend(erjang.ECons)
	 */
	@Override
	public ESeq prepend(ESeq list) {
		EBigString other = list.testBigString();
		if (other != null) {
			char[] out = new char[length() + other.length()];
			System.arraycopy(other.data, other.off, out, 0, other.length());
			System.arraycopy(this.data, this.off, out, other.length(), this
					.length());
			return EBigString.make(out, 0, out.length);
		} else {
			return super.prepend(list);
		}
	}

	/**
	 * @return
	 */
	public EBinary asBitString(Charset charset) {
		byte[] data = stringValue().getBytes(charset);
		return new EBinary(data, 0, data.length);
	}

	/**
	 * @param eObject
	 * @return
	 */
	public static EBigString make(EObject eObject) {
		ESeq str;
		if ((str = eObject.testSeq()) != null)
			return make(str);

		EAtom am = eObject.testAtom();
		if (am != null) {
			return fromString(am.toString());
		}

		throw ERT.badarg();
	}

	public void collectCharList(CharCollector out)
		throws CharCollector.CollectingException, IOException
	{
		try {
			out.addIntegers(data, off, data.length - off);
		} catch (CharCollector.PartialDecodingException e) {
			int n = e.inputPos;
			throw new CharCollector.CollectingException(new EBigString(data, off+n));
		}
	}

	/**
	 * @param i
	 * @return
	 */
	public static boolean isValidCodePoint(int cp) {
		return (cp >>> 16) <= 0x10 // in 0..10FFFF; Unicode range
				&& (cp & ~0x7FF) != 0xD800 // not in D800..DFFF; surrogate range
				&& (cp & ~1) != 0xFFFE; // not in FFFE..FFFF; non-characters
	}

	public static ESeq read(EInputStream ei) throws IOException {
		return ei.read_string();
	}

	@Override
	public void encode(EOutputStream eos) {
		eos.write_string(stringValue());
	}

}
