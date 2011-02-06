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
package erjang;

// import java.io.OutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.text.DecimalFormat;
import java.util.zip.Deflater;

import erjang.driver.IO;

/**
 * Provides a stream for encoding Erlang terms to external format, for
 * transmission or storage.
 * 
 * <p>
 * Note that this class is not synchronized, if you need synchronization you
 * must provide it yourself.
 * 
 */
public class EOutputStream extends ByteArrayOutputStream {
    /** The default initial size of the stream. * */
    public static final int DEFAULT_INITIAL_BUFFER_SIZE = 512;

    /** The default increment used when growing the stream. * */
    public static final int DEFAULT_BUFFER_INCREMENT = 2048;

    // static formats, used to encode floats and doubles
    private static final DecimalFormat eform = new DecimalFormat("e+00;e-00");
    private static final BigDecimal ten = new BigDecimal(10.0);
    private static final BigDecimal one = new BigDecimal(1.0);

	private int flags;

    /**
     * Create a stream with the default initial size (2048 bytes).
     */
    public EOutputStream() {
	this(DEFAULT_INITIAL_BUFFER_SIZE);
    }

    /**
     * Create a stream with the specified initial size.
     */
    public EOutputStream(final int size) {
    	super(size);
        }

    public EOutputStream(final int size, int flags) {
    	super(size);
    	this.flags = flags;
        }

    /**
     * Create a stream containing the encoded version of the given Erlang term.
     */
    public EOutputStream(final EObject o) {
	this();
	write_any(o);
    }

    // package scope
    /*
     * Get the contents of the output stream as an input stream instead. This is
     * used internally in {@link ECconnection} for tracing outgoing packages.
     * 
     * @param offset where in the output stream to read data from when creating
     * the input stream. The offset is necessary because header contents start 5
     * bytes into the header buffer, whereas payload contents start at the
     * beginning
     * 
     * @return an input stream containing the same raw data.
     */
    EInputStream getEInputStream(final int offset) {
	return new EInputStream(super.buf, offset, super.count - offset, 0);
    }
    
    public EBinary getBinaryContent() {
    	return new EBinary(super.buf, 0, super.count);
    }

    /**
     * Get the current position in the stream.
     * 
     * @return the current position in the stream.
     */
    public int getPos() {
	return super.count;
    }

    /**
     * Write one byte to the stream.
     * 
     * @param b
     *            the byte to write.
     * 
     */
    public void write(final byte b) {
	if (super.count >= super.buf.length) {
	    // System.err.println("Expanding buffer from " + this.buf.length
	    // + " to " + (this.buf.length+DEFAULT_BUFFER_INCREMENT));
	    final byte[] tmp = new byte[super.buf.length + DEFAULT_BUFFER_INCREMENT];
	    System.arraycopy(super.buf, 0, tmp, 0, super.count);
	    super.buf = tmp;
	}
	super.buf[super.count++] = b;
    }

    /**
     * Write an array of bytes to the stream.
     * 
     * @param buf
     *            the array of bytes to write.
     * 
     */

    @Override
    public void write(final byte[] buf) {
	if (super.count + buf.length > super.buf.length) {
	    // System.err.println("Expanding buffer from " + super.buf.length
	    // + " to " + (buf.length + super.buf.lengt + DEFAULT_BUFFER_INCREMENT));
	    final byte[] tmp = new byte[super.buf.length + buf.length
		    + DEFAULT_BUFFER_INCREMENT];
	    System.arraycopy(super.buf, 0, tmp, 0, super.count);
	    super.buf = tmp;
	}
	System.arraycopy(buf, 0, super.buf, super.count, buf.length);
	super.count += buf.length;
    }


    public void ensureSpace(int space) {
	if (super.count + space > super.buf.length) {
	    // System.err.println("Expanding buffer from " + super.buf.length
	    // + " to " + (buf.length + super.buf.lengt + DEFAULT_BUFFER_INCREMENT));
	    final byte[] tmp = new byte[super.buf.length + space
		    + DEFAULT_BUFFER_INCREMENT];
	    System.arraycopy(super.buf, 0, tmp, 0, super.count);
	    super.buf = tmp;
	}
    }

    /** 'Unsafe' write - doesn't ensure that there's space in the array. */
    public void raw_write(final byte b) {
	super.buf[super.count++] = b;
    }
    public void raw_write(int b) {
		super.buf[super.count++] = (byte)b;
    }



    /**
     * Write the low byte of a value to the stream.
     * 
     * @param n
     *            the value to use.
     * 
     */
    public void write1(final long n) {
	write((byte) (n & 0xff));
    }

    /**
     * Write an array of bytes to the stream.
     * 
     * @param buf
     *            the array of bytes to write.
     * 
     */
    public void writeN(final byte[] bytes) {
	write(bytes);
    }

    public void writeN(final byte[] bytes, int off, int len) {
		write(bytes, off, len);
    }

    /**
     * Get the current capacity of the stream. As bytes are added the capacity
     * of the stream is increased automatically, however this method returns the
     * current size.
     * 
     * @return the size of the internal buffer used by the stream.
     */
    public int length() {
	return super.buf.length;
    }

    /**
     * Get the number of bytes in the stream.
     * 
     * @return the number of bytes in the stream.
     * 
     * @deprecated As of Jinterface 1.4, replaced by super.size().
     * @see #size()
     */

    @Deprecated
    public int count() {
	return count;
    }

    /**
     * Write the low two bytes of a value to the stream in big endian order.
     * 
     * @param n
     *            the value to use.
     */
    public void write2BE(final long n) {
	write((byte) ((n) >> 8));
	write((byte) (n));
    }

    public void raw_write2BE(final long n) {
	raw_write((byte) ((n) >> 8));
	raw_write((byte) (n));
    }

    /**
     * Write the low four bytes of a value to the stream in big endian order.
     * 
     * @param n
     *            the value to use.
     */
    public void write4BE(final long n) {
	write((byte) ((n) >> 24));
	write((byte) ((n) >> 16));
	write((byte) ((n) >> 8));
	write((byte) (n));
    }

    public void raw_write4BE(int n) {
	raw_write((byte) ((n) >> 24));
	raw_write((byte) ((n) >> 16));
	raw_write((byte) ((n) >> 8));
	raw_write((byte) (n));
    }

    /**
     * Write the low eight (all) bytes of a value to the stream in big endian
     * order.
     * 
     * @param n
     *            the value to use.
     */
    public void write8BE(final long n) {
	write((byte) (n >> 56));
	write((byte) (n >> 48));
	write((byte) (n >> 40));
	write((byte) (n >> 32));
	write((byte) (n >> 24));
	write((byte) (n >> 16));
	write((byte) (n >> 8));
	write((byte) (n));
    }

    /**
     * Write any number of bytes in little endian format.
     * 
     * @param n
     *            the value to use.
     * @param b
     *            the number of bytes to write from the little end.
     */
    public void writeLE(long n, final int b) {
	for (int i = 0; i < b; i++) {
	    write((byte) (n & 0xff));
	    n >>= 8;
	}
    }

    /**
     * Write the low two bytes of a value to the stream in little endian order.
     * 
     * @param n
     *            the value to use.
     */
    public void write2LE(final long n) {
	write((byte) (n));
	write((byte) ((n) >> 8));
    }

    /**
     * Write the low four bytes of a value to the stream in little endian order.
     * 
     * @param n
     *            the value to use.
     */
    public void write4LE(final long n) {
	write((byte) (n));
	write((byte) ((n) >> 8));
	write((byte) ((n) >> 16));
	write((byte) ((n) >> 24));
    }

    /**
     * Write the low eight bytes of a value to the stream in little endian
     * order.
     * 
     * @param n
     *            the value to use.
     */
    public void write8LE(final long n) {
	write((byte) (n));
	write((byte) (n >> 8));
	write((byte) (n >> 16));
	write((byte) (n >> 24));
	write((byte) (n >> 32));
	write((byte) (n >> 40));
	write((byte) (n >> 48));
	write((byte) (n >> 56));
    }

    /**
     * Write the low four bytes of a value to the stream in bif endian order, at
     * the specified position. If the position specified is beyond the end of
     * the stream, this method will have no effect.
     * 
     * Normally this method should be used in conjunction with {@link #size()
     * size()}, when is is necessary to insert data into the stream before it is
     * known what the actual value should be. For example:
     * 
     * <pre>
     * int pos = s.size();
     *    s.write4BE(0); // make space for length data,
     *                   // but final value is not yet known
     *     [ ...more write statements...]
     *    // later... when we know the length value
     *    s.poke4BE(pos, length);
     * </pre>
     * 
     * 
     * @param offset
     *            the position in the stream.
     * @param n
     *            the value to use.
     */
    public void poke4BE(final int offset, final long n) {
	if (offset < super.count) {
	    buf[offset + 0] = (byte) ((n & 0xff000000) >> 24);
	    buf[offset + 1] = (byte) ((n & 0xff0000) >> 16);
	    buf[offset + 2] = (byte) ((n & 0xff00) >> 8);
	    buf[offset + 3] = (byte) (n & 0xff);
	}
    }

    /**
     * Write a string to the stream as an Erlang atom.
     * 
     * @param atom
     *            the string to write.
     */
    public void write_atom(final String atom) {
		int len = Math.min(EExternal.maxAtomLength, atom.length());
		if (len < 256 && (flags & EAbstractNode.dFlagSmallAtoms) != 0)
		{
			ensureSpace(1+1+len);
			raw_write(EExternal.smallAtomTag);
			raw_write(len);
		} else {
		    ensureSpace(1+2+len);
			raw_write(EExternal.atomTag);
			raw_write2BE(atom.length());
		}

		for (int i = 0; i < len; i++) {
			raw_write((byte)atom.charAt(i));
		}
    }

    /**
     * Write an array of bytes to the stream as an Erlang binary.
     * 
     * @param bin
     *            the array of bytes to write.
     */
    public void write_binary(final byte[] bin) {
	write1(EExternal.binTag);
	write4BE(bin.length);
	writeN(bin);
    }

    /**
     * Write an array of bytes to the stream as an Erlang bitstr.
     * 
     * @param bin
     *            the array of bytes to write.
     * @param pad_bits
     *            the number of zero pad bits at the low end of the last byte
     */
    public void write_bitstr(final byte[] bin, final int pad_bits) {
	if (pad_bits == 0) {
	    write_binary(bin);
	    return;
	}
	write1(EExternal.bitBinTag);
	write4BE(bin.length);
	write1(8 - pad_bits);
	writeN(bin);
    }

    /**
     * Write a boolean value to the stream as the Erlang atom 'true' or 'false'.
     * 
     * @param b
     *            the boolean value to write.
     */
    public void write_boolean(final boolean b) {
	write_atom(String.valueOf(b));
    }

    /**
     * Write a single byte to the stream as an Erlang integer. The byte is
     * really an IDL 'octet', that is, unsigned.
     * 
     * @param b
     *            the byte to use.
     */
    public void write_byte(final byte b) {
	this.write_long(b & 0xffL, true);
    }

    /**
     * Write a character to the stream as an Erlang integer. The character may
     * be a 16 bit character, kind of IDL 'wchar'. It is up to the Erlang side
     * to take care of souch, if they should be used.
     * 
     * @param c
     *            the character to use.
     */
    public void write_char(final char c) {
	this.write_long(c & 0xffffL, true);
    }

    /**
     * Write a double value to the stream.
     * 
     * @param d
     *            the double to use.
     */
    public void write_double(final double d) {
	write1(EExternal.newFloatTag);
	write8BE(Double.doubleToLongBits(d));
    }

    /**
     * Write a float value to the stream.
     * 
     * @param f
     *            the float to use.
     */
    public void write_float(final float f) {
	write_double(f);
    }

    public void write_big_integer(BigInteger v) {
	if (v.bitLength() < 64) {
	    this.write_long(v.longValue(), false);
	    return;
	}
	final int signum = v.signum();
	if (signum < 0) {
	    v = v.negate();
	}
	final byte[] magnitude = v.toByteArray();
	final int n = magnitude.length;
	// Reverse the array to make it little endian.
	for (int i = 0, j = n; i < j--; i++) {
	    // Swap [i] with [j]
	    final byte b = magnitude[i];
	    magnitude[i] = magnitude[j];
	    magnitude[j] = b;
	}
	if ((n & 0xFF) == n) {
	    write1(EExternal.smallBigTag);
	    write1(n); // length
	} else {
	    write1(EExternal.largeBigTag);
	    write4BE(n); // length
	}
	write1(signum < 0 ? 1 : 0); // sign
	// Write the array
	writeN(magnitude);
    }

    void write_long(final long v, final boolean unsigned) {
	/*
	 * If v<0 and unsigned==true the value
	 * java.lang.Long.MAX_VALUE-java.lang.Long.MIN_VALUE+1+v is written, i.e
	 * v is regarded as unsigned two's complement.
	 */
	if ((v & 0xffL) == v) {
	    // will fit in one byte
		ensureSpace(2);
	    raw_write(EExternal.smallIntTag);
	    raw_write((int)v);
	} else {
	    // note that v != 0L
	    if (v < 0 && unsigned || v < EExternal.erlMin
		    || v > EExternal.erlMax) {
		// some kind of bignum
		final long abs = unsigned ? v : v < 0 ? -v : v;
		final int sign = unsigned ? 0 : v < 0 ? 1 : 0;
		int n;
		long mask;
		for (mask = 0xFFFFffffL, n = 4; (abs & mask) != abs; n++, mask = mask << 8 | 0xffL) {
		    ; // count nonzero bytes
		}
		write1(EExternal.smallBigTag);
		write1(n); // length
		write1(sign); // sign
		writeLE(abs, n); // value. obs! little endian
	    } else {
			ensureSpace(5);
			raw_write(EExternal.intTag);
			raw_write4BE((int)v);
	    }
	}
    }

    /**
     * Write a long to the stream.
     * 
     * @param l
     *            the long to use.
     */
    public void write_long(final long l) {
	this.write_long(l, false);
    }

    /**
     * Write a positive long to the stream. The long is interpreted as a two's
     * complement unsigned long even if it is negative.
     * 
     * @param ul
     *            the long to use.
     */
    public void write_ulong(final long ul) {
	this.write_long(ul, true);
    }

    /**
     * Write an integer to the stream.
     * 
     * @param i
     *            the integer to use.
     */
    public void write_int(final int i) {
	this.write_long(i, false);
    }

    /**
     * Write a positive integer to the stream. The integer is interpreted as a
     * two's complement unsigned integer even if it is negative.
     * 
     * @param ui
     *            the integer to use.
     */
    public void write_uint(final int ui) {
	this.write_long(ui & 0xFFFFffffL, true);
    }

    /**
     * Write a short to the stream.
     * 
     * @param s
     *            the short to use.
     */
    public void write_short(final short s) {
	this.write_long(s, false);
    }

    /**
     * Write a positive short to the stream. The short is interpreted as a two's
     * complement unsigned short even if it is negative.
     * 
     * @param s
     *            the short to use.
     */
    public void write_ushort(final short us) {
	this.write_long(us & 0xffffL, true);
    }

    /**
     * Write an Erlang list header to the stream. After calling this method, you
     * must write 'arity' elements to the stream followed by nil, or it will not
     * be possible to decode it later.
     * 
     * @param arity
     *            the number of elements in the list.
     */
    public void write_list_head(final int arity) {
	if (arity == 0) {
	    write_nil(); // Is this correct?
	} else {
	    write1(EExternal.listTag);
	    write4BE(arity);
	}
    }

    /**
     * Write an empty Erlang list to the stream.
     */
    public void write_nil() {
	write1(EExternal.nilTag);
    }

    /**
     * Write an Erlang tuple header to the stream. After calling this method,
     * you must write 'arity' elements to the stream or it will not be possible
     * to decode it later.
     * 
     * @param arity
     *            the number of elements in the tuple.
     */
    public void write_tuple_head(final int arity) {
	if (arity < 0xff) {
	    write1(EExternal.smallTupleTag);
	    write1(arity);
	} else {
	    write1(EExternal.largeTupleTag);
	    write4BE(arity);
	}
    }

    /**
     * Write an Erlang PID to the stream.
     * 
     * @param node
     *            the nodename.
     * 
     * @param id
     *            an arbitrary number. Only the low order 15 bits will be used.
     * 
     * @param serial
     *            another arbitrary number. Only the low order 13 bits will be
     *            used.
     * 
     * @param creation
     *            yet another arbitrary number. Only the low order 2 bits will
     *            be used.
     * 
     */
    public void write_pid(final EAtom node, final int id, final int serial,
	    final int creation) {
	write1(EExternal.pidTag);
	write_atom(node.getName());
	write4BE(id & 0x7fff); // 15 bits
	write4BE(serial & 0x1fff); // 13 bits
	write1(creation & 0x3); // 2 bits
    }

    /**
     * Write an Erlang port to the stream.
     * 
     * @param node
     *            the nodename.
     * 
     * @param id
     *            an arbitrary number. Only the low order 28 bits will be used.
     * 
     * @param creation
     *            another arbitrary number. Only the low order 2 bits will be
     *            used.
     * 
     */
    public void write_port(final String node, final int id, final int creation) {
	write1(EExternal.portTag);
	write_atom(node);
	write4BE(id & 0xfffffff); // 28 bits
	write1(creation & 0x3); // 2 bits
    }

    /**
     * Write an old style Erlang ref to the stream.
     * 
     * @param node
     *            the nodename.
     * 
     * @param id
     *            an arbitrary number. Only the low order 18 bits will be used.
     * 
     * @param creation
     *            another arbitrary number. Only the low order 2 bits will be
     *            used.
     * 
     */
    public void write_ref(final String node, final int id, final int creation) {
	write1(EExternal.refTag);
	write_atom(node);
	write4BE(id & 0x3ffff); // 18 bits
	write1(creation & 0x3); // 2 bits
    }

    /**
     * Write a new style (R6 and later) Erlang ref to the stream.
     * 
     * @param node
     *            the nodename.
     * 
     * @param ids
     *            an array of arbitrary numbers. Only the low order 18 bits of
     *            the first number will be used. If the array contains only one
     *            number, an old style ref will be written instead. At most
     *            three numbers will be read from the array.
     * 
     * @param creation
     *            another arbitrary number. Only the low order 2 bits will be
     *            used.
     * 
     */
    public void write_ref(final String node, final int[] ids, final int creation) {
	int arity = ids.length;
	if (arity > 3) {
	    arity = 3; // max 3 words in ref
	}

	if (arity == 1) {
	    // use old method
	    this.write_ref(node, ids[0], creation);
	} else {
	    // r6 ref
	    write1(EExternal.newRefTag);

	    // how many id values
	    write2BE(arity);

	    write_atom(node);

	    // note: creation BEFORE id in r6 ref
	    write1(creation & 0x3); // 2 bits

	    // first int gets truncated to 18 bits
	    write4BE(ids[0] & 0x3ffff);

	    // remaining ones are left as is
	    for (int i = 1; i < arity; i++) {
		write4BE(ids[i]);
	    }
	}
    }

    /**
     * Write a string to the stream.
     * 
     * @param s
     *            the string to write.
     */
    public void write_string(final String s) {
	final int len = s.length();

	switch (len) {
	case 0:
	    write_nil();
	    break;
	default:
	    if (len <= 65535 && is8bitString(s)) { // 8-bit string
		try {
		    final byte[] bytebuf = s.getBytes("ISO-8859-1");
		    write1(EExternal.stringTag);
		    write2BE(len);
		    writeN(bytebuf);
		} catch (final UnsupportedEncodingException e) {
		    write_nil(); // it should never ever get here...
		}
	    } else { // unicode or longer, must code as list
		final char[] charbuf = s.toCharArray();
		final int[] codePoints = EString.stringToCodePoints(s);
		write_list_head(codePoints.length);
		for (final int codePoint : codePoints) {
		    write_int(codePoint);
		}
		write_nil();
	    }
	}
    }

	public void write_string(byte[] data, int off, int len) {
	    if (len <= 65535) { // 8-bit string
			ensureSpace(1+2+len);
			raw_write(EExternal.stringTag);
			raw_write2BE(len);
			writeN(data, off, len);
	    } else { // Code as list
			write_list_head(len);
			for (int i=off; i<off+len; i++) {
				write_int(data[i]); // Possible performance improvement?
			}
			write_nil();
	    }
	}

    private boolean is8bitString(final String s) {
	for (int i = 0; i < s.length(); ++i) {
	    final char c = s.charAt(i);
	    if (c < 0 || c > 255) {
		return false;
	    }
	}
	return true;
    }

    /**
     * Write an arbitrary Erlang term to the stream in compressed format.
     * 
     * @param o
     *            the Erlang tem to write.
     */
    public void write_compressed(final EObject o, int level) {
	final EOutputStream oos = new EOutputStream(o);
	write1(EExternal.compressedTag);
	write4BE(oos.size());
	final java.io.FilterOutputStream fos = new java.io.FilterOutputStream(
		this);
	final java.util.zip.DeflaterOutputStream dos = new java.util.zip.DeflaterOutputStream(
		fos, new Deflater(level));
	try {
	    oos.writeTo(dos);
	    dos.close();
	} catch (final IOException e) {
	    throw new java.lang.IllegalArgumentException(
		    "Intremediate stream failed for Erlang object " + o);
	}
    }

    /**
     * Write an arbitrary Erlang term to the stream.
     * 
     * @param o
     *            the Erlang term to write.
     */
    public void write_any(final EObject o) {
	// calls one of the above functions, depending on o
	o.encode(this);
    }

    public void write_fun(final EPID pid, final String module,
	    final long old_index, final int arity, final EBinary md5,
	    final long index, final long uniq, final EObject[] freeVars) {

// DEBUG...    	
//		ESeq fvx = ERT.NIL;
//		for (int i = freeVars.length-1; i >= 0; i--) { fvx = fvx.cons(freeVars[i]); }
//		System.err.println("EOut.write<pid="+pid+",module="+module+",arity="+arity+",md5="+md5
//				+",index="+index+",old_index="+old_index+",uniq="+uniq+",freevars="+fvx+"> old="+((flags & EAbstractNode.dflagNewFunTags) == 0));
    	
    if ((flags & EAbstractNode.dflagNewFunTags) == 0) {
	    write1(EExternal.funTag);
	    write4BE(freeVars.length);
	    pid.encode(this);
	    write_atom(module);
	    write_long(index & 0xffffffffL);
	    write_long(uniq  & 0xffffffffL);
	    for (final EObject fv : freeVars) {
		fv.encode(this);
	    }
	} else {
	    write1(EExternal.newFunTag);
	    final int saveSizePos = getPos();
	    write4BE(0); // this is where we patch in the size
	    write1(arity);
	    writeN(md5.getByteArray());
	    write4BE(index);
	    write4BE(freeVars.length);
	    write_atom(module);
	    write_long(old_index & 0xffffffffL);
	    write_long(uniq      & 0xffffffffL);
	    pid.encode(this);
	    for (final EObject fv : freeVars) {
		fv.encode(this);
	    }
	    poke4BE(saveSizePos, getPos() - saveSizePos);
	}
    }

    public void write_external_fun(final EAtom module, final EAtom function, final int arity) {
		write_external_fun(module.getName(), function.getName(), arity);
	}

    public void write_external_fun(final String module, final String function,
	    final int arity) {
	write1(EExternal.externalFunTag);
	write_atom(module);
	write_atom(function);
	write_long(arity);
    }

	public ByteBuffer toByteBuffer() {
    	return ByteBuffer.wrap(super.buf, 0, super.count);
	}
}
