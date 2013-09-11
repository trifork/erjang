/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
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
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;
import java.nio.CharBuffer;
import java.nio.ByteBuffer;

/** Used by the unicode BIFs characters_to_binary/2 and characters_to_list/2.
 */
public class CharCollector {
	public static ByteBuffer EMPTY = ByteBuffer.wrap(new byte[0]);
	public static int BUF_SIZE = 20486;

	CharsetDecoder decoder;
	CharBuffer buffer;
	Appendable output;
	boolean dirtyDecoder = false;

	public CharCollector(Charset charset, Appendable output) {
		buffer = CharBuffer.allocate(BUF_SIZE);
		this.output = output;
		this.decoder = charset.newDecoder();
		this.decoder.reset();
	}

	public ESeq addInteger(int value, ESeq rest) throws IOException, DecodingException {
		
		if (rest != ERT.NIL) {
			return rest.cons(ERT.box(value));
		}

		if (dirtyDecoder)
			try { flushDecoder(); }
			catch (PartialDecodingException e) {
				// Incomplete binary was followed by an integer...
				// Convert exception.
				throw new DecodingException();
			}
		char c = (char)value;
		if (c != value || !Character.isDefined(value)) fail(new DecodingException());

		if (! buffer.hasRemaining())
			flushBuffer(); /* Or write to output directly?   Provided that we
							* flush the buffer when we do the decoder. */
		buffer.put(c);
		
		return rest;
	}

	/** Add a byte array, interpreted as a list of integers.
	 *  Equivalent to (but faster than) calling add(int) for each of
	 *  the elements.
	 * @return 
	 *  @throws PartialDecodingException if these integers follow an
	 *  incomplete binary.
	 */
	public ESeq addIntegers(byte[] data, int offset, int length, ESeq rest) throws IOException, PartialDecodingException {
		
		if (rest != ERT.NIL) {
			return rest.cons(EBinary.make(data, offset, length, 0));
		}
		
		if (length==0) return rest;
		if (dirtyDecoder) flushDecoder();
		while (length > 0) {
			int free = buffer.remaining();
			while (length > 0 && free > 0) {
				char c = (char)(data[offset] & 0xFF);
				buffer.put(c);
				offset++; length--; free--; // Could be smarter.
			}
			if (free==0) flushBuffer();
		}
		return rest;
	}

	/** Add a byte array, interpreted as a binary to be decoded. 
	 * @param rest TODO
	 * @return TODO*/
	public ESeq addBinary(byte[] data, int offset, int length, ESeq rest) throws IOException, PartialDecodingException {
		if (rest != ERT.NIL) {
			return rest.cons(EBinary.make(data, offset, length, 0));
		}
		return addBinary(ByteBuffer.wrap(data, offset, length), false, rest);
	}

	public ESeq addBinary(ByteBuffer data, boolean endOfInput, ESeq rest)
		throws IOException, PartialDecodingException
	{
		if (rest != ERT.NIL) {
			return rest.cons(EBinary.make(data));
		}
		
		CoderResult res;
		do {
			res = decoder.decode(data, buffer, endOfInput);
			if (!handle(res))
				fail(new PartialDecodingException(data.position()));
		} while (res == CoderResult.OVERFLOW);
		
		if (data.hasRemaining()) {
			if (!endOfInput) {
				decoder.decode(ByteBuffer.wrap(new byte[0]), buffer, true);
				// flush the decoder ...
				// decoder.flush(buffer);
			}
			return rest.cons(EBinary.make(data));
		}
		// The decoder may have left some data. Save that in some fashion...
		dirtyDecoder = true;
		return rest;
	}

	protected boolean handle(CoderResult res) throws IOException {
		if (res == CoderResult.UNDERFLOW) {
			return true;
		} else if (res == CoderResult.OVERFLOW) {
			flushBuffer();
			return true;
		} else return false;
	}

	public void end() throws IOException, PartialDecodingException {
		flushDecoder();
		flushBuffer();
	}

	protected void fail(DecodingException e) throws DecodingException, IOException {
		flushBuffer();
		throw e;
	}
	protected void fail(PartialDecodingException e) throws PartialDecodingException, IOException {
		flushBuffer();
		throw e;
	}

	protected void flushDecoder() throws IOException,PartialDecodingException {
		addBinary(EMPTY, true, ERT.NIL);
		decoder.flush(buffer);
		decoder.reset();
		dirtyDecoder = false;
	}

	protected void flushBuffer() throws IOException {
		buffer.flip();
		output.append(buffer);
	}

	@SuppressWarnings("serial")
	public static class DecodingException extends Exception { }

	@SuppressWarnings("serial")
	public static class PartialDecodingException extends Exception {
		public final int inputPos;
		public PartialDecodingException(int inputPos) {
			this.inputPos = inputPos;
		}
	}

 	@SuppressWarnings("serial")
	public static class InvalidElementException extends Exception { }

	@SuppressWarnings("serial")
	public static class CollectingException extends Exception {
		public final EObject restOfInput;
		public CollectingException(EObject restOfInput) {
			this.restOfInput = restOfInput;
		}
	}

	/**
	 * @param data
	 * @param off
	 * @param length
	 * @throws PartialDecodingException 
	 * @throws IOException 
	 */
	public ESeq addIntegers(char[] data, int offset, int length, ESeq rest) throws IOException, PartialDecodingException {
		if (length==0) return rest;
		if (dirtyDecoder) flushDecoder();
		while (length > 0) {
			int free = buffer.remaining();
			while (length > 0 && free > 0) {
				char c = (char)(data[offset] & 0xFFFF);
				buffer.put(c);
				offset++; length--; free--; // Could be smarter.
			}
			if (free==0) flushBuffer();
		}
		return rest;
	}

}
