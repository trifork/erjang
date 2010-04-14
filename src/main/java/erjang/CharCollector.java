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

	public void addInteger(int value) throws IOException, DecodingException {
		if (dirtyDecoder)
			try { flushDecoder(); }
			catch (PartialDecodingException e) {
				// Incomplete binary was followed by an integer...
				// Convert exception.
				throw new DecodingException();
			}
		char c = (char)value;
		if (c != value) fail(new DecodingException());

		if (! buffer.hasRemaining())
			flushBuffer(); /* Or write to output directly?   Provided that we
							* flush the buffer when we do the decoder. */
		buffer.put(c);
	}

	/** Add a byte array, interpreted as a list of integers.
	 *  Equivalent to (but faster than) calling add(int) for each of
	 *  the elements.
	 *  @throws PartialDecodingException if these integers follow an
	 *  incomplete binary.
	 */
	public void addIntegers(byte[] data, int offset, int length) throws IOException, PartialDecodingException {
		if (length==0) return;
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
	}

	/** Add a byte array, interpreted as a binary to be decoded. */
	public void addBinary(byte[] data, int offset, int length) throws IOException, PartialDecodingException {
		addBinary(ByteBuffer.wrap(data, offset, length), false);
	}

	public void addBinary(ByteBuffer data, boolean endOfInput)
		throws IOException, PartialDecodingException
	{
		CoderResult res;
		do {
			res = decoder.decode(data, buffer, endOfInput);
			if (!handle(res))
				fail(new PartialDecodingException(data.position()));
		} while (res == CoderResult.OVERFLOW);
		if (data.hasRemaining())
			throw new NotImplemented("Character possibly spanning binary boundary");
		// The decoder may have left some data. Save that in some fashion...
		dirtyDecoder = true;
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
		addBinary(EMPTY, true);
		decoder.flush(buffer);
		decoder.reset();
		dirtyDecoder = false;
	}

	protected void flushBuffer() throws IOException {
		buffer.flip();
		output.append(buffer);
	}

	public static class DecodingException extends Exception { }

	public static class PartialDecodingException extends Exception {
		public final int inputPos;
		public PartialDecodingException(int inputPos) {
			this.inputPos = inputPos;
		}
	}

 	public static class InvalidElementException extends Exception { }

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
	public void addIntegers(char[] data, int offset, int length) throws IOException, PartialDecodingException {
		if (length==0) return;
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
	}

}
