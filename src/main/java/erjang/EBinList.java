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
import java.util.List;

/**
 * Special cons cell optimizing for [byte, byte, byte | Tail]
 */
public class EBinList extends ECons {

	final byte[] data;
	final int off;
	final int len;
	final EObject tail;

	// synchronized access
	private boolean shared;

	private EBinList(byte[] data, int off, int len, EObject tail, boolean shared) {
		this.data = data;
		this.off = off;
		this.len = len;
		this.tail = tail;
		this.shared = shared;

		if (len < 1 || off + len > data.length)
			throw ERT.badarg();
		
	}

	public EBinList(byte value, EObject tail) {
		this.data = new byte[10];
		this.off = 9;
		this.len = 1;
		this.tail = tail;
		this.shared = false;

		data[off] = value;
	}
	
	public ECons testNonEmptyList() {
		return this;
	}
	
	public ESeq testWellformedList() {
		if (tail.testWellformedList() != null) {
			return this.seq();
		} else {
			return null;
		}
	}


	/**
	 * @param header
	 * @param out
	 */
	public EBinList(byte[] header, EObject out) {
		this(header, 0, header.length, out, true);
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
		if ((sm = h.testSmall()) != null && sm.value < 256) {

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

	@Override
	public EObject head() {
		return new ESmall(data[off]);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.ECons#tail()
	 */
	@Override
	public EObject tail() {
		if (len == 1)
			return tail;

		return new EBinList(data, off + 1, len - 1, tail, true);
	}

	@Override
	public boolean isNil() {
		return len == 0 && tail.isNil();
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

	/** An EBinList is an ESeq iff the tail is an ESeq */
	@Override
	public ESeq testSeq() {
		if (tail.testSeq() != null) {
			return new Seq();
		}
		
		return null;
	}
	
	private ESeq seq() { return new Seq(); }
	
	/**
	 * Helper class that looks at this EBinList as a Seq.
	 */
	private class Seq extends ESeq {

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

	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("[");
		
		int max = Math.min(len, 40);
		for (int i = 0; i < max; i++) {
			if (i != 0) { sb.append(","); }
			byte val = data[off+i];
			if (val > ' ' && val < 127) {
				sb.append('$');
				sb.append((char)val);
			} else {
				sb.append((int)val);
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

}
