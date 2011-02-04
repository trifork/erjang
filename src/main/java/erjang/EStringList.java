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

import java.nio.ByteBuffer;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * Special cons cell optimizing for [byte, byte, byte | Tail].  
 * An <code>EBinList</code> has a tail, and an array of bytes for the
 * values it contains.  It is optimized for the case of cons'ing a
 * byte onto it; it works like a java StringBuilder, but its contents
 * array grows backwards; so the bytes contained in it are stored in
 * the last bytes of the <code>data</code> array variable.
 */
public class EStringList extends ESeq {

	static class PrependableBytes {
		AtomicInteger start_pos;
		byte[] data;

		PrependableBytes(int size) {
			this.data = new byte[size];
			this.start_pos = new AtomicInteger(data.length);
		}

		PrependableBytes(int size, byte v) {
			this.data = new byte[size];
			data[size-1] = v;
			this.start_pos = new AtomicInteger(data.length-1);
		}

		PrependableBytes(byte[] org, int extra) {
			this(org, 0, org.length, extra);
		}

		PrependableBytes(byte[] org, int off, int len, int extra) {
			this(org.length + extra);
			System.arraycopy(org,off, data,data.length-len, len);
		}

		PrependableBytes prepend(int old_start, byte b) {
			assert (old_start > 0);
			int new_start = old_start-1;
			if (start_pos.compareAndSet(old_start, new_start)) {
				data[new_start] = b;
				return this;
			} else return null;
		}

		boolean prepend2(int old_start, byte b) {
			assert (old_start > 0);
			int new_start = old_start-1;
			if (start_pos.compareAndSet(old_start, new_start)) {
				data[new_start] = b;
				return true;
			} else return false;
		}

		PrependableBytes prepend(int old_start, byte[] org, int off, int len) {
			assert (old_start > 0);
			int new_start = old_start-len;
			if (start_pos.compareAndSet(old_start, new_start)) {
				System.arraycopy(org,off, data,new_start, len);
				return this;
			} else return null;
		}
	}

	/**
	 * 
	 */
	private static final int INITIAL_BUFFER_SIZE = 10;
	final PrependableBytes bytes;
	final int off;
	final int len;
	final ESeq tail;

	private EStringList(PrependableBytes bytes, int off, int len, ESeq tail) {
		assert len>0;
		this.bytes = bytes;
		this.off = off;
		this.len = len;
		this.tail = tail;
	}


	/** create a list with [value|tail], where value is a smallint 0..255 */
	public EStringList(byte value, ESeq tail) {
		this(new PrependableBytes(INITIAL_BUFFER_SIZE, value),
			 INITIAL_BUFFER_SIZE-1, 1, tail);
	}

	public EStringList(byte[] header, ESeq tail) {
		this(new PrependableBytes(header, INITIAL_BUFFER_SIZE),
			 INITIAL_BUFFER_SIZE, header.length, tail);
	}

	public ECons testNonEmptyList() {
		if (len == 0)
			return tail.testNonEmptyList();
		return this;
	}

	public ESeq testSeq() {
		return this;
	}

	@Override
	public ESeq cons(EObject h) {
		ESmall sm = h.testSmall();
		if (sm != null) {
			int value = sm.value;
			if ((value & ~0xff) == 0) { // Fits in a byte
				if (off==0) return new EStringList((byte)value, this);

				if (bytes.prepend2(this.off, (byte)value)) {
					return new EStringList(bytes, off-1, len+1, tail);
				} else {
					return new EStringList((byte)value, this);
				}
			}
		}
		return new EList(h, this);
	}

	public EObject drop(int n) {
		if (n > len) throw new IllegalArgumentException();
		if (n == len) return tail;

		return new EStringList(bytes, off + n, len - n, tail);
	}

	@Override
	public ESmall head() {
		return ESmall.little[(bytes.data[off] & 0xff)];
	}

	@Override
	public ESeq tail() {
		if (len == 1)
			return tail;

		return new EStringList(bytes, off + 1, len - 1, tail);
	}

	// TODO: Remove this method altogether
	@Override
	public boolean isNil() {
		assert len != 0;
		return false;
	}

	@Override
	public ENil testNil() {
		if (isNil())
			return ERT.NIL;
		return null;
	}

	public EString testString() {
		EString st = tail.testString();
		if (st == null) {
			return null;
		}
		//TODO: handle stringlist chain more efficiently?
		byte[] out_bin = new byte[len + st.length()];
		System.arraycopy(this.bytes.data, this.off, out_bin, 0, this.len);
		System.arraycopy(st.data, st.off, out_bin, len, st.length());
		return new EString(out_bin, 0);
	}

	private ESeq seq() { return new Seq(); }

   /**
	 * Helper class that looks at this EBinList as a Seq.
	 */
	private class Seq extends ESeq {
		@Override
		public ECons testNonEmptyList() {
			return EStringList.this.testNonEmptyList();
		}

		@Override
		public ESeq cons(EObject h) {
			return EStringList.this.cons(h);
		}

		@Override
		public ESeq tail() {
			return EStringList.this.tail();
		}

		@Override
		public EObject head() {
			return EStringList.this.head();
		}

		@Override
		public void encode(EOutputStream eos) {
			EStringList.this.encode(eos);
		}
	}
	
	private boolean all_printable() {
		byte val;
		for (int i = 0; i < len; i++) {
			val = bytes.data[off+i];
			if (val < ' ' || val >= 127) {
				return false;
			}
		}
		return true;
	}

	@Override
	public String toString() {
		
		//
		// Can this be printed as "..."
		//
		if (tail.isNil() && all_printable()) {
			StringBuilder sb = new StringBuilder("\"");
			for (int i = 0; i < len; i++) {
				byte val = bytes.data[off+i];
				sb.append((char)val);
			}
			sb.append('"');
			return sb.toString();
		}
		
		//
		// Otherwise, print it as [.., .., |..]
		//
		StringBuilder sb = new StringBuilder("[");
		
		int max = Math.min(len, 40);
		for (int i = 0; i < max; i++) {
			if (i != 0) { sb.append(","); }
			byte val = bytes.data[off+i];
			if (val > ' ' && val < 127) {
				sb.append('$');
				sb.append((char)val);
			} else {
				sb.append(val & 0xFF);
			}
		}
		
		if (max!=len) {
			sb.append("...");
		}
		
		if (!tail.isNil()) {
			sb.append('|');
			sb.append(tail);
		}
		
		sb.append("]");
		return sb.toString();
	}
	
	@Override
	public boolean collectIOList(List<ByteBuffer> out) {
		out.add(ByteBuffer.wrap(bytes.data, off, len)); //?
		return tail.collectIOList(out);
	}

	@Override
	public void collectCharList(CharCollector out)
		throws CharCollector.CollectingException,
		CharCollector.InvalidElementException,
		IOException
	{
		try {
			out.addBinary(bytes.data, off, len);
		} catch (CharCollector.PartialDecodingException e) {
			throw new CharCollector.CollectingException(drop(e.inputPos - off));
		}

		if (tail.testNumber() != null) {
			// Only nil and binaries are allowed as tail
			// TODO: Fail sooner?
			throw new CharCollector.InvalidElementException();
		} else tail.collectCharList(out);
	}

	@Override
	public void encode(EOutputStream eos) {
		if (tail.isNil()) {
			eos.write_string(bytes.data, off, len);
		} else { //TODO: Check whether tail is EString or EStringList
			eos.write_list_head(len);
			for (int i = 0; i < len; i++) {
				eos.write_int(bytes.data[off+i]);
			}
			eos.write_any(tail);
		}
	}
	
	public static EStringList fromString(String c, ESeq tail) {
		byte[] data = new byte[c.length()];
		for (int i = 0; i < data.length; i++) {
			data[i] = (byte) c.charAt(i);
		}
		return new EStringList(data, tail);
	}
	
	private static final Type ESTRINGLIST_TYPE = Type.getType(EStringList.class);
	private static final Type STRING_TYPE = Type.getType(String.class);
	private static final String ESEQ_DESC = Type.getDescriptor(ESeq.class);

	@Override
	public Type emit_const(MethodVisitor fa) {

		Type type = ESTRINGLIST_TYPE;

		char[] ch = new char[len];
		for (int i = 0; i < len; i++) {
			ch[i] = (char)(0xff & (int)bytes.data[off+i]);
		}
		
		fa.visitLdcInsn(new String(ch));
		tail.emit_const(fa);
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, type.getInternalName(),
				"fromString", "(" + STRING_TYPE.getDescriptor() + ESEQ_DESC + ")"
						+ type.getDescriptor());

		return type;
	}

}
