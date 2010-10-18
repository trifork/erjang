/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2000-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */

/**
 * This is derived from com.ericsson.otp.erlang.OtpInputStream
 */
package erjang;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.EOFException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.security.MessageDigest;

import erjang.driver.IO;

/**
 * Provides a stream for decoding Erlang terms from external format.
 * 
 * <p>
 * Note that this class is not synchronized, if you need synchronization you
 * must provide it yourself.
 */
public class EInputStream extends ByteArrayInputStream {

	public static int DECODE_INT_LISTS_AS_STRINGS = 1;

	private final int flags;

	private EAtom[] atom_cache_refs;

	/**
	 * @param buf
	 */
	public EInputStream(final byte[] buf) {
		this(buf, 0);
	}

	/**
	 * Create a stream from a buffer containing encoded Erlang terms.
	 * 
	 * @param flags
	 */
	public EInputStream(final byte[] buf, final int flags) {
		super(buf);
		this.flags = flags;
	}

	/**
	 * Create a stream from a buffer containing encoded Erlang terms at the
	 * given offset and length.
	 * 
	 * @param flags
	 */
	public EInputStream(final byte[] buf, final int offset, final int length,
			final int flags) {
		super(buf, offset, length);
		this.flags = flags;
	}
	
	public void updateMessageDigest(MessageDigest digest, int offset, int len) {
		digest.update(this.buf, offset, len);
	}

	/**
	 * Get the current position in the stream.
	 * 
	 * @return the current position in the stream.
	 */
	public int getPos() {
		return super.pos;
	}

	/**
	 * Set the current position in the stream.
	 * 
	 * @param pos
	 *            the position to move to in the stream. If pos indicates a
	 *            position beyond the end of the stream, the position is move to
	 *            the end of the stream instead. If pos is negative, the
	 *            position is moved to the beginning of the stream instead.
	 * 
	 * @return the previous position in the stream.
	 */
	public int setPos(int pos) {
		final int oldpos = super.pos;

		if (pos > super.count) {
			pos = super.count;
		} else if (pos < 0) {
			pos = 0;
		}

		super.pos = pos;

		return oldpos;
	}

	/**
	 * Read an array of bytes from the stream. The method reads at most
	 * buf.length bytes from the input stream.
	 * 
	 * @return the number of bytes read.
	 * 
	 * @exception IOException
	 *                if the next byte cannot be read.
	 */
	public int readN(final byte[] buf) throws IOException {
		return this.readN(buf, 0, buf.length);
	}

	/**
	 * Read an array of bytes from the stream. The method reads at most len
	 * bytes from the input stream into offset off of the buffer.
	 * 
	 * @return the number of bytes read.
	 * 
	 * @exception IOException
	 *                if the next byte cannot be read.
	 */
	public int readN(final byte[] buf, final int off, final int len)
			throws IOException {
		if (len == 0 && available() == 0) {
			return 0;
		}
		final int i = super.read(buf, off, len);
		if (i < 0) {
			throw new IOException("Cannot read from input stream");
		}
		return i;
	}

	/**
	 * Alias for peek1()
	 */
	public int peek() throws IOException {
		return peek1();
	}

	/**
	 * Look ahead one position in the stream without consuming the byte found
	 * there.
	 * 
	 * @return the next byte in the stream, as an integer.
	 * 
	 * @exception IOException
	 *                if the next byte cannot be read.
	 */
	public int peek1() throws IOException {
		int i;
		try {
			i = super.buf[super.pos];
			if (i < 0) {
				i += 256;
			}

			return i;
		} catch (final Exception e) {
			throw new IOException("Cannot read from input stream");
		}
	}

	public int peek1skip_version() throws IOException {
		int i = peek1();
		if (i == EExternal.versionTag) {
			read1();
			i = peek1();
		}
		return i;
	}

	/**
	 * Read a one byte integer from the stream.
	 * 
	 * @return the byte read, as an integer.
	 * 
	 * @exception IOException
	 *                if the next byte cannot be read.
	 */
	public int read1() throws IOException {
		int i;
		i = super.read();

		if (i < 0) {
			throw new IOException("Cannot read from input stream");
		}

		return i;
	}

	public int read1skip_version() throws IOException {
		int tag = read1();
		if (tag == EExternal.versionTag) {
			tag = read1();
		}
		return tag;
	}

	public void readFully(byte[] b) throws IOException {
		int pos = 0;
		while (pos < b.length) {
			int read;
			try {
				read = super.read(b, pos, b.length-pos);
			} catch (IndexOutOfBoundsException ioobe) {
				read = 0;
			}
			if (read==0) throw new EOFException("Can't read enough from input stream");
			pos += read;
		}
	}

	/**
	 * Read a two byte big endian integer from the stream.
	 * 
	 * @return the bytes read, converted from big endian to an integer.
	 * 
	 * @exception IOException
	 *                if the next byte cannot be read.
	 */
	public int read2BE() throws IOException {
		final byte[] b = new byte[2];
		readFully(b);
		return (b[0] << 8 & 0xff00) + (b[1] & 0xff);
	}

	/**
	 * Read a four byte big endian integer from the stream.
	 * 
	 * @return the bytes read, converted from big endian to an integer.
	 * 
	 * @exception IOException
	 *                if the next byte cannot be read.
	 */
	public int read4BE() throws IOException {
		final byte[] b = new byte[4];
		readFully(b);
		return (b[0] << 24 & 0xff000000) + (b[1] << 16 & 0xff0000)
				+ (b[2] << 8 & 0xff00) + (b[3] & 0xff);
	}

	/**
	 * Read a two byte little endian integer from the stream.
	 * 
	 * @return the bytes read, converted from little endian to an integer.
	 * 
	 * @exception IOException
	 *                if the next byte cannot be read.
	 */
	public int read2LE() throws IOException {
		final byte[] b = new byte[2];
		readFully(b);
		return (b[1] << 8 & 0xff00) + (b[0] & 0xff);
	}

	/**
	 * Read a four byte little endian integer from the stream.
	 * 
	 * @return the bytes read, converted from little endian to an integer.
	 * 
	 * @exception IOException
	 *                if the next byte cannot be read.
	 */
	public int read4LE() throws IOException {
		final byte[] b = new byte[4];
		readFully(b);
		return (b[3] << 24 & 0xff000000) + (b[2] << 16 & 0xff0000)
				+ (b[1] << 8 & 0xff00) + (b[0] & 0xff);
	}

	/**
	 * Read a little endian integer from the stream.
	 * 
	 * @param n
	 *            the number of bytes to read
	 * 
	 * @return the bytes read, converted from little endian to an integer.
	 * 
	 * @exception IOException
	 *                if the next byte cannot be read.
	 */
	public long readLE(int n) throws IOException {
		final byte[] b = new byte[n];
		try {
			super.read(b);
		} catch (final IOException e) {
			throw new IOException("Cannot read from input stream");
		}
		;
		long v = 0;
		while (n-- > 0) {
			v = v << 8 | (long) b[n] & 0xff;
		}
		return v;
	}

	/**
	 * Read a bigendian integer from the stream.
	 * 
	 * @param n
	 *            the number of bytes to read
	 * 
	 * @return the bytes read, converted from big endian to an integer.
	 * 
	 * @exception IOException
	 *                if the next byte cannot be read.
	 */
	public long readBE(final int n) throws IOException {
		final byte[] b = new byte[n];
		try {
			super.read(b);
		} catch (final IOException e) {
			throw new IOException("Cannot read from input stream");
		}
		;
		long v = 0;
		for (int i = 0; i < n; i++) {
			v = v << 8 | (long) b[i] & 0xff;
		}
		return v;
	}

	/**
	 * Read an Erlang atom from the stream and interpret the value as a boolean.
	 * 
	 * @return true if the atom at the current position in the stream contains
	 *         the value 'true' (ignoring case), false otherwise.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not an atom.
	 */
	public boolean read_boolean() throws IOException {
		return read_atom() == ERT.TRUE;
	}

	/**
	 * Read an Erlang atom from the stream.
	 * 
	 * @return a String containing the value of the atom.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not an atom.
	 */
	public EAtom read_atom() throws IOException {
		int tag;
		int len;
		byte[] strbuf;
		String atom;

		tag = read1skip_version();

		if (tag == EExternal.atomCacheRef) {
			int index = read1() & 0xff;
			EAtom res = atom_cache_refs[index];
			if (res == null) {
				throw new IOException("no cached atom at "+index);
			}
			return res;
		}
		
		if (tag == EExternal.smallAtomTag) {
			len = read1();
		} else {
			if (tag != EExternal.atomTag) {
				throw new IOException("wrong tag encountered, expected "
						+ EExternal.atomTag + ", got " + tag);
			}
	
			len = read2BE();
		}
		
		strbuf = new byte[len];
		this.readN(strbuf);
		
		char[] in = new char[len];
		for (int i = 0; i < len; i++) {
			in[i] = (char) (strbuf[i] & 0xff);
		}
		
		atom = new String(in);

		if (atom.length() > EExternal.maxAtomLength) {
			atom = atom.substring(0, EExternal.maxAtomLength);
		}

		return EAtom.intern(atom);
	}

	/**
	 * Read an Erlang binary from the stream.
	 * 
	 * @return a byte array containing the value of the binary.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not a binary.
	 */
	public byte[] read_binary() throws IOException {
		int tag;
		int len;
		byte[] bin;

		tag = read1skip_version();

		if (tag != EExternal.binTag) {
			throw new IOException("Wrong tag encountered, expected "
					+ EExternal.binTag + ", got " + tag);
		}

		len = read4BE();

		bin = new byte[len];
		this.readN(bin);

		return bin;
	}

	/**
	 * Read an Erlang bitstr from the stream.
	 * 
	 * @param pad_bits
	 *            an int array whose first element will be set to the number of
	 *            pad bits in the last byte.
	 * 
	 * @return a byte array containing the value of the bitstr.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not a bitstr.
	 */
	public byte[] read_bitstr(final int pad_bits[]) throws IOException {
		int tag;
		int len;
		byte[] bin;

		tag = read1skip_version();

		if (tag != EExternal.bitBinTag) {
			throw new IOException("Wrong tag encountered, expected "
					+ EExternal.bitBinTag + ", got " + tag);
		}

		len = read4BE();
		bin = new byte[len];
		final int tail_bits = read1();
		if (tail_bits < 0 || 7 < tail_bits) {
			throw new IOException("Wrong tail bit count in bitstr: "
					+ tail_bits);
		}
		if (len == 0 && tail_bits != 0) {
			throw new IOException("Length 0 on bitstr with tail bit count: "
					+ tail_bits);
		}
		this.readN(bin);

		pad_bits[0] = 8 - tail_bits;
		return bin;
	}

	/**
	 * Read an Erlang float from the stream.
	 * 
	 * @return the float value.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not a float.
	 */
	public float read_float() throws IOException {
		final double d = read_double();
		return (float) d;
	}

	/**
	 * Read an Erlang float from the stream.
	 * 
	 * @return the float value, as a double.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not a float.
	 */
	public double read_double() throws IOException {
		int tag;

		// parse the stream
		tag = read1skip_version();

		switch (tag) {
		case EExternal.newFloatTag: {
			return Double.longBitsToDouble(readBE(8));
		}
		case EExternal.floatTag: {
			BigDecimal val;
			int epos;
			int exp;
			final byte[] strbuf = new byte[31];
			String str;

			// get the string
			this.readN(strbuf);
			str = new String(strbuf, IO.ISO_LATIN_1);

			// find the exponent prefix 'e' in the string
			epos = str.indexOf('e', 0);

			if (epos < 0) {
				throw new IOException("Invalid float format: '" + str + "'");
			}

			// remove the sign from the exponent, if positive
			String estr = str.substring(epos + 1).trim();

			if (estr.substring(0, 1).equals("+")) {
				estr = estr.substring(1);
			}

			// now put the mantissa and exponent together
			exp = Integer.valueOf(estr).intValue();
			val = new BigDecimal(str.substring(0, epos)).movePointRight(exp);

			return val.doubleValue();
		}
		default:
			throw new IOException("Wrong tag encountered, expected "
					+ EExternal.newFloatTag + ", got " + tag);
		}
	}

	/**
	 * Read one byte from the stream.
	 * 
	 * @return the byte read.
	 * 
	 * @exception IOException
	 *                if the next byte cannot be read.
	 */
	public byte read_byte() throws IOException {
		final long l = this.read_long(false);
		final byte i = (byte) l;

		if (l != i) {
			throw new IOException("Value does not fit in byte: " + l);
		}

		return i;
	}

	/**
	 * Read a character from the stream.
	 * 
	 * @return the character value.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not an integer that can
	 *                be represented as a char.
	 */
	public char read_char() throws IOException {
		final long l = this.read_long(true);
		final char i = (char) l;

		if (l != (i & 0xffffL)) {
			throw new IOException("Value does not fit in char: " + l);
		}

		return i;
	}

	/**
	 * Read an unsigned integer from the stream.
	 * 
	 * @return the integer value.
	 * 
	 * @exception IOException
	 *                if the next term in the stream can not be represented as a
	 *                positive integer.
	 */
	public int read_uint() throws IOException {
		final long l = this.read_long(true);
		final int i = (int) l;

		if (l != (i & 0xFFFFffffL)) {
			throw new IOException("Value does not fit in uint: " + l);
		}

		return i;
	}

	/**
	 * Read an integer from the stream.
	 * 
	 * @return the integer value.
	 * 
	 * @exception IOException
	 *                if the next term in the stream can not be represented as
	 *                an integer.
	 */
	public int read_int() throws IOException {
		final long l = this.read_long(false);
		final int i = (int) l;

		if (l != i) {
			throw new IOException("Value does not fit in int: " + l);
		}

		return i;
	}

	/**
	 * Read an unsigned short from the stream.
	 * 
	 * @return the short value.
	 * 
	 * @exception IOException
	 *                if the next term in the stream can not be represented as a
	 *                positive short.
	 */
	public short read_ushort() throws IOException {
		final long l = this.read_long(true);
		final short i = (short) l;

		if (l != (i & 0xffffL)) {
			throw new IOException("Value does not fit in ushort: " + l);
		}

		return i;
	}

	/**
	 * Read a short from the stream.
	 * 
	 * @return the short value.
	 * 
	 * @exception IOException
	 *                if the next term in the stream can not be represented as a
	 *                short.
	 */
	public short read_short() throws IOException {
		final long l = this.read_long(false);
		final short i = (short) l;

		if (l != i) {
			throw new IOException("Value does not fit in short: " + l);
		}

		return i;
	}

	/**
	 * Read an unsigned long from the stream.
	 * 
	 * @return the long value.
	 * 
	 * @exception IOException
	 *                if the next term in the stream can not be represented as a
	 *                positive long.
	 */
	public long read_ulong() throws IOException {
		return this.read_long(true);
	}

	/**
	 * Read a long from the stream.
	 * 
	 * @return the long value.
	 * 
	 * @exception IOException
	 *                if the next term in the stream can not be represented as a
	 *                long.
	 */
	public long read_long() throws IOException {
		return this.read_long(false);
	}

	public long read_long(final boolean unsigned) throws IOException {
		final byte[] b = read_integer_byte_array();
		return EInputStream.byte_array_to_long(b, unsigned);
	}

	public EInteger read_tagged_integer() throws IOException {
		int tag = read1skip_version();

		switch (tag) {
		case EExternal.smallIntTag: return new ESmall(read1());
		case EExternal.intTag:      return new ESmall(read4BE());
		default: setPos(getPos()-1); return ERT.box(new BigInteger(read_integer_byte_array()));
		} // switch
	}

	/**
	 * Read an integer from the stream.
	 * 
	 * @return the value as a big endian 2's complement byte array.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not an integer.
	 */
	public byte[] read_integer_byte_array() throws IOException {
		int tag;
		byte[] nb;

		tag = read1skip_version();

		switch (tag) {
		case EExternal.smallIntTag:
			nb = new byte[2];
			nb[0] = 0;
			nb[1] = (byte) read1();
			break;

		case EExternal.intTag:
			nb = new byte[4];
			if (this.readN(nb) != 4) { // Big endian
				throw new IOException("Cannot read from intput stream");
			}
			break;

		case EExternal.smallBigTag:
		case EExternal.largeBigTag:
			int arity;
			int sign;
			if (tag == EExternal.smallBigTag) {
				arity = read1();
				sign = read1();
			} else {
				arity = read4BE();
				sign = read1();
				if (arity + 1 < 0) {
					throw new IOException(
							"Value of largeBig does not fit in BigInteger, arity "
									+ arity + " sign " + sign);
				}
			}
			nb = new byte[arity + 1];
			// Value is read as little endian. The big end is augumented
			// with one zero byte to make the value 2's complement positive.
			if (this.readN(nb, 0, arity) != arity) {
				throw new IOException("Cannot read from intput stream");
			}
			// Reverse the array to make it big endian.
			for (int i = 0, j = nb.length; i < j--; i++) {
				// Swap [i] with [j]
				final byte b = nb[i];
				nb[i] = nb[j];
				nb[j] = b;
			}
			if (sign != 0) {
				// 2's complement negate the big endian value in the array
				int c = 1; // Carry
				for (int j = nb.length; j-- > 0;) {
					c = (~nb[j] & 0xFF) + c;
					nb[j] = (byte) c;
					c >>= 8;
				}
			}
			break;

		default:
			throw new IOException("Not valid integer tag: " + tag);
		}

		return nb;
	}

	public static long byte_array_to_long(final byte[] b, final boolean unsigned)
			throws IOException {
		long v;
		switch (b.length) {
		case 0:
			v = 0;
			break;
		case 2:
			v = ((b[0] & 0xFF) << 8) + (b[1] & 0xFF);
			v = (short) v; // Sign extend
			if (v < 0 && unsigned) {
				throw new IOException("Value not unsigned: " + v);
			}
			break;
		case 4:
			v = ((b[0] & 0xFF) << 24) + ((b[1] & 0xFF) << 16)
					+ ((b[2] & 0xFF) << 8) + (b[3] & 0xFF);
			v = (int) v; // Sign extend
			if (v < 0 && unsigned) {
				throw new IOException("Value not unsigned: " + v);
			}
			break;
		default:
			int i = 0;
			final byte c = b[i];
			// Skip non-essential leading bytes
			if (unsigned) {
				if (c < 0) {
					throw new IOException("Value not unsigned: " + b);
				}
				while (b[i] == 0) {
					i++; // Skip leading zero sign bytes
				}
			} else {
				if (c == 0 || c == -1) { // Leading sign byte
					i = 1;
					// Skip all leading sign bytes
					while (i < b.length && b[i] == c) {
						i++;
					}
					if (i < b.length) {
						// Check first non-sign byte to see if its sign
						// matches the whole number's sign. If not one more
						// byte is needed to represent the value.
						if (((c ^ b[i]) & 0x80) != 0) {
							i--;
						}
					}
				}
			}
			if (b.length - i > 8) {
				// More than 64 bits of value
				throw new IOException("Value does not fit in long: " + b);
			}
			// Convert the necessary bytes
			for (v = c < 0 ? -1 : 0; i < b.length; i++) {
				v = v << 8 | b[i] & 0xFF;
			}
		}
		return v;
	}

	/**
	 * Read a list header from the stream.
	 * 
	 * @return the arity of the list.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not a list.
	 */
	public int read_list_head() throws IOException {
		int arity = 0;
		final int tag = read1skip_version();

		switch (tag) {
		case EExternal.nilTag:
			arity = 0;
			break;

		case EExternal.stringTag:
			arity = read2BE();
			break;

		case EExternal.listTag:
			arity = read4BE();
			break;

		default:
			throw new IOException("Not valid list tag: " + tag);
		}

		return arity;
	}

	/**
	 * Read a tuple header from the stream.
	 * 
	 * @return the arity of the tuple.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not a tuple.
	 */
	public int read_tuple_head() throws IOException {
		int arity = 0;
		final int tag = read1skip_version();

		// decode the tuple header and get arity
		switch (tag) {
		case EExternal.smallTupleTag:
			arity = read1();
			break;

		case EExternal.largeTupleTag:
			arity = read4BE();
			break;

		default:
			throw new IOException("Not valid tuple tag: " + tag);
		}

		return arity;
	}

	/**
	 * Read an empty list from the stream.
	 * 
	 * @return zero (the arity of the list).
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not an empty list.
	 */
	public int read_nil() throws IOException {
		int arity = 0;
		final int tag = read1skip_version();

		switch (tag) {
		case EExternal.nilTag:
			arity = 0;
			break;

		default:
			throw new IOException("Not valid nil tag: " + tag);
		}

		return arity;
	}

	/**
	 * Read an Erlang PID from the stream.
	 * 
	 * @return the value of the PID.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not an Erlang PID.
	 */
	public EPID read_pid() throws IOException {
		EAtom node;
		int id;
		int serial;
		int creation;
		int tag;

		tag = read1skip_version();

		if (tag != EExternal.pidTag) {
			throw new IOException("Wrong tag encountered, expected "
					+ EExternal.pidTag + ", got " + tag);
		}

		node = read_atom();
		id = read4BE() & 0x7fff; // 15 bits
		serial = read4BE() & 0x1fff; // 13 bits
		creation = read1() & 0x03; // 2 bits

		return EPID.make(node, id, serial, creation);
	}

	/**
	 * Read an Erlang port from the stream.
	 * 
	 * @return the value of the port.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not an Erlang port.
	 */
	public EPort read_port() throws IOException {
		EAtom node;
		int id;
		int creation;
		int tag;

		tag = read1skip_version();

		if (tag != EExternal.portTag) {
			throw new IOException("Wrong tag encountered, expected "
					+ EExternal.portTag + ", got " + tag);
		}

		node = read_atom();
		id = read4BE() & 0xfffffff; // 28 bits
		creation = read1() & 0x03; // 2 bits

		return EPort.make(node, id, creation);
	}

	/**
	 * Read an Erlang reference from the stream.
	 * 
	 * @return the value of the reference
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not an Erlang reference.
	 */
	public ERef read_ref() throws IOException {
		EAtom node;
		int id;
		int creation;
		int tag;

		tag = read1skip_version();

		switch (tag) {
		case EExternal.refTag:
			node = read_atom();
			id = read4BE() & 0x3ffff; // 18 bits
			creation = read1() & 0x03; // 2 bits
			return new ERef(node, id, creation);

		case EExternal.newRefTag:
			final int arity = read2BE();
			node = read_atom();
			creation = read1() & 0x03; // 2 bits

			final int[] ids = new int[arity];
			for (int i = 0; i < arity; i++) {
				ids[i] = read4BE();
			}
			ids[0] &= 0x3ffff; // first id gets truncated to 18 bits
			return new ERef(node, ids, creation);

		default:
			throw new IOException("Wrong tag encountered, expected ref, got "
					+ tag);
		}
	}

	public EFun read_fun() throws IOException {
		final int tag = read1skip_version();
		if (tag == EExternal.funTag) {
			final int nFreeVars = read4BE();
			final EPID pid = read_pid();
			final EAtom module = read_atom();
			final long index = read_long();
			final long uniq = read_long();
			final EObject[] freeVars = new EObject[nFreeVars];
			for (int i = 0; i < nFreeVars; ++i) {
				freeVars[i] = read_any();
			}
			return EModuleManager.resolve(pid, module, (int)uniq, (int)index, freeVars);

		} else if (tag == EExternal.newFunTag) {
			@SuppressWarnings("unused")
			final int n = read4BE(); // size
			final int arity = read1();
			final byte[] md5 = new byte[16];
			readN(md5);
			final int index = read4BE();
			final int nFreeVars = read4BE();
			final EAtom module = read_atom();
			final long oldIndex = read_long();
			final long uniq = read_long();
			final EPID pid = read_pid();
			final EObject[] freeVars = new EObject[nFreeVars];
			for (int i = 0; i < nFreeVars; ++i) {
				freeVars[i] = read_any();
			}
			return EModuleManager.resolve(pid, module, new EBinary(md5), index, (int)uniq, (int)oldIndex, arity, freeVars);
			
		} else if (tag == EExternal.externalFunTag) {
			final EAtom module = read_atom();
			final EAtom function = read_atom();
			final int arity = (int) read_long();
			
			return EModuleManager.resolve(new FunID(module,function,arity));
		} else {
			throw new IOException("Wrong tag encountered, expected fun, got "
					+ tag);
		}
	}

	public EFun read_external_fun() throws IOException {
		final int tag = read1skip_version();
		if (tag != EExternal.externalFunTag) {
			throw new IOException(
					"Wrong tag encountered, expected external fun, got " + tag);
		}
		final EAtom module = read_atom();
		final EAtom function = read_atom();
		final int arity = (int) read_long();
		
		return EModuleManager.resolve(new FunID(module,function,arity));
	}

	/**
	 * Read a string from the stream.
	 * 
	 * @return the value of the string.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not a string.
	 */
	public ESeq read_string() throws IOException {
		int tag;
		int len;
		byte[] strbuf;
		int[] intbuf;
		tag = read1skip_version();
		switch (tag) {
		case EExternal.stringTag:
			len = read2BE();
			strbuf = new byte[len];
			this.readN(strbuf);
			return EString.make(strbuf);
		case EExternal.nilTag:
			return ERT.NIL;
		case EExternal.listTag: // List when unicode +
			len = read4BE();
			intbuf = new int[len];
			boolean unicode = false;
			for (int i = 0; i < len; i++) {
				intbuf[i] = read_int();
				unicode |= intbuf[i] > 0xff;
				if (!EString.isValidCodePoint(intbuf[i])) {
					throw new IOException("Invalid CodePoint: " + intbuf[i]);
				}
			}
			read_nil();
			return EList.make(intbuf);
		default:
			throw new IOException("Wrong tag encountered, expected "
					+ EExternal.stringTag + " or " + EExternal.listTag
					+ ", got " + tag);
		}
	}

	/**
	 * Read a compressed term from the stream
	 * 
	 * @return the resulting uncompressed term.
	 * 
	 * @exception IOException
	 *                if the next term in the stream is not a compressed term.
	 */
	public EObject read_compressed() throws IOException {
		final int tag = read1skip_version();

		if (tag != EExternal.compressedTag) {
			throw new IOException("Wrong tag encountered, expected "
					+ EExternal.compressedTag + ", got " + tag);
		}
		final byte[] buf = read_size_and_inflate();

		final EInputStream ois = new EInputStream(buf, flags);
		return ois.read_any();
	}

	public byte[] read_size_and_inflate() throws IOException {
		final int size = read4BE();
		final byte[] buf = new byte[size];
		final java.util.zip.Inflater inflater = new java.util.zip.Inflater();
		final java.util.zip.InflaterInputStream is = new java.util.zip.InflaterInputStream(this, inflater);

		try {
		int pos = 0;
		while (pos<size) { // Inflate fully
			final int dsize = is.read(buf, pos, size-pos);
			if (dsize==0) break;
			pos += dsize;
		}
		if (pos < size) {
			throw new IOException("Decompression gave " + pos
					      + " bytes, not " + size);
		}

		// Back up if the inflater read ahead:
		int read_ahead = inflater.getRemaining();
		setPos(getPos() - read_ahead);
		} finally {
			is.close();
		}
		
		return buf;
	}

	/**
	 * Read an arbitrary Erlang term from the stream.
	 * 
	 * @return the Erlang term.
	 * 
	 * @exception IOException
	 *                if the stream does not contain a known Erlang type at the
	 *                next position.
	 */
	public EObject read_any() throws IOException {
		// calls one of the above functions, depending on o
		final int tag = peek1skip_version();

		switch (tag) {
		case EExternal.smallIntTag:
		case EExternal.intTag:
		case EExternal.smallBigTag:
		case EExternal.largeBigTag:
			return EInteger.read(this);

		case EExternal.atomCacheRef:
		case EExternal.atomTag:
		case EExternal.smallAtomTag:
			return EAtom.read(this);
			
		case EExternal.floatTag:
		case EExternal.newFloatTag:
			return EDouble.read(this);

		case EExternal.refTag:
		case EExternal.newRefTag:
			return ERef.read(this);

		case EExternal.portTag:
			return EPort.read(this);

		case EExternal.pidTag:
			return EPID.read(this);

		case EExternal.stringTag:
			return EString.read(this);

		case EExternal.listTag:
		case EExternal.nilTag:
			if ((flags & DECODE_INT_LISTS_AS_STRINGS) != 0) {
				final int savePos = getPos();
				try {
					return EString.read(this);
				} catch (final IOException e) {
				}
				setPos(savePos);
			}
			return EList.read(this);

		case EExternal.smallTupleTag:
		case EExternal.largeTupleTag:
			return ETuple.read(this);

		case EExternal.binTag:
			return EBinary.read(this);

		case EExternal.bitBinTag:
			return EBitString.read(this);

		case EExternal.compressedTag:
			return read_compressed();

		case EExternal.newFunTag:
		case EExternal.funTag:
			return EFun.read(this);
			
		case EExternal.externalFunTag:
			return read_external_fun();

		default:
			throw new IOException("Unknown data type: " + tag + " at position " + getPos());
		}
	}

	public void setAtomCacheRefs(EAtom[] atomCacheRefs) {
		this.atom_cache_refs = atomCacheRefs;		
	}
}
