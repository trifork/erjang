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
public class EBinList extends ECons {

	/**
	 * 
	 */
	private static final int INITIAL_BUFFER_SIZE = 10;
	final byte[] data;
	final int off;
	final int len;
	final EObject tail;

	// synchronized access
	/** True if <code>data</code> is shared by other instances of EBinList.
	 * In that case, we need to copy the data array if we need to grow. */
	private boolean shared;

	/** create list as sublist of data given */
	private EBinList(byte[] data, int off, int len, EObject tail, boolean shared) {
		
		assert len != 0;
		
		this.data = data;
		this.off = off;
		this.len = len;
		this.tail = tail;
		this.shared = shared;

		assert(tail!=null);
		if (len < 1 || off + len > data.length)
			throw ERT.badarg();
	}

	@Override
	public int hashCode() {
		ESeq seq;
		if ((seq=testSeq()) != null) return seq.hashCode();

		//TODO: use phash() rather than this brittle nonsense:
		int h = 0;
		for (int i=off; i<off+len; i++) {
			h = 31*h + data[i];
		}
		return h + tail.hashCode();
	}

	/** create a list with [value|tail], where value is a smallint 0..255 */
	public EBinList(byte value, EObject tail) {
		this.data = new byte[INITIAL_BUFFER_SIZE];
		this.off = INITIAL_BUFFER_SIZE-1;
		this.len = 1;
		this.tail = tail;
		this.shared = false;

		data[off] = value;
	}
	
	public ECons testNonEmptyList() {
		if (len == 0) 
			return tail.testNonEmptyList();
		return this;
	}
	
	public ESeq testSeq() {
		if (tail.testSeq() != null) {
			return this.seq();
		} else {
			return null;
		}
	}


	/**
	 * @param header
	 * @param tail
	 */
	public EBinList(byte[] header, EObject tail) {
		this(header, 0, header.length, tail, true);
	}

	/**
	 * @param header
	 * @param tail2
	 */
	public EBinList(ByteBuffer buf, EObject tail) {
		this(buf.array(), buf.arrayOffset()+buf.position(), buf.remaining(), tail, true);
	}

	@Override
	public ECons cons(EObject h) {
		ESmall sm;
		if ((sm = h.testSmall()) != null && sm.value >= 0 && sm.value < 256) {

			byte[] res_data = data;
			int res_off = off;
			int res_len = len + 1;

			synchronized (this) {
				if (shared || off == 0) {
					res_data = new byte[len * 2 + 1];
					System.arraycopy(data, off, res_data, len + 1, len);
					res_off = len + 1;
				} else {
					shared = true;
				}
			}

			res_data[--res_off] = (byte) sm.value;
			return new EBinList(res_data, res_off, res_len, tail, res_data==data);

		} else {
			return new EPair(h, this);
		}
	}

	public EObject drop(int n) {
		if (n > len) throw new IllegalArgumentException();
		if (n == len) return tail;

		synchronized (this) {
			shared = true;
		}
		return new EBinList(data, off+n, len-n, tail, true);
	}

	@Override
	public ESmall head() {
		return ESmall.little[(data[off] & 0xff)];
	}

	@Override
	public EObject tail() {
		if (len == 1)
			return tail;

		return new EBinList(data, off + 1, len - 1, tail, true);
	}

	// TODO: Remote this method all together
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
		
		byte[] out_bin = new byte[len + st.length()];
		System.arraycopy(this.data, this.off, out_bin, 0, this.len);
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
			return EBinList.this.testNonEmptyList();
		}

		@Override
		public ESeq cons(EObject h) {
			return EBinList.this.cons(h).testSeq();
		}

		@Override
		public ESeq tail() {
			return EBinList.this.tail().testSeq();
		}

		@Override
		public EObject head() {
			return EBinList.this.head();
		}

		@Override
		public void encode(EOutputStream eos) {
			EBinList.this.encode(eos);
		}
	}
	
	private boolean all_printable() {
		byte val;
		for (int i = 0; i < len; i++) {
			val = data[off+i];
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
				byte val = data[off+i];
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
			byte val = data[off+i];
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
		out.add(ByteBuffer.wrap(data, off, len));
		return tail.collectIOList(out);
	}

	@Override
	public void collectCharList(CharCollector out)
		throws CharCollector.CollectingException,
		CharCollector.InvalidElementException,
		IOException
	{
		try {
			out.addBinary(data, off, len);
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
		eos.write_list_head(len);
		for (int i = 0; i < len; i++) {
			eos.write_int(data[off+i]);
		}
		eos.write_any(tail);
	}
	
	public static EBinList fromString(String c, EObject tail) {
		byte[] data = new byte[c.length()];
		for (int i = 0; i < data.length; i++) {
			data[i] = (byte) c.charAt(i);
		}
		return new EBinList(data, tail);
	}
	
	private static final Type EBINLIST_TYPE = Type.getType(EBinList.class);
	private static final Type STRING_TYPE = Type.getType(String.class);
	private static final String EOBJECT_DESC = Type.getDescriptor(EObject.class);

	@Override
	public Type emit_const(MethodVisitor fa) {

		Type type = EBINLIST_TYPE;

		char[] ch = new char[len];
		for (int i = 0; i < len; i++) {
			ch[i] = (char)(0xff & (int)data[off+i]);
		}
		
		fa.visitLdcInsn(new String(ch));
		tail.emit_const(fa);
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, type.getInternalName(),
				"fromString", "(" + STRING_TYPE.getDescriptor() + EOBJECT_DESC + ")"
						+ type.getDescriptor());

		return type;
	}

}
