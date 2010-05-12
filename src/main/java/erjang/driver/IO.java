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

package erjang.driver;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.BindException;
import java.net.ConnectException;
import java.nio.ByteBuffer;
import java.nio.channels.GatheringByteChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.charset.Charset;
import java.util.zip.GZIPOutputStream;

import erjang.ERT;
import erjang.driver.efile.Posix;

/**
 * 
 */
public class IO {

	/**
	 * 
	 */
	public static final Charset ISO_LATIN_1 = Charset.forName("ISO-8859-1");
	public static final Charset UTF8 = Charset.forName("UTF8");

	static private class BARR extends ByteArrayOutputStream {
		ByteBuffer wrap() {
			return ByteBuffer.wrap(super.buf, 0, super.count);
		}
	}

	/**
	 * Write src[position .. limit]. Set position to limit.
	 * 
	 * @param out
	 * @param src
	 * @throws IOException
	 */
	public static void write(OutputStream out, ByteBuffer src)
			throws IOException {
		out.write(src.array(), src.arrayOffset() + src.position(), src
				.remaining());
		src.position(src.limit());
	}

	/**
	 * @param fd
	 * @param byteBuffer
	 * @return
	 * @throws InterruptedException
	 */
	public static long gzwrite(WritableByteChannel fd, ByteBuffer src)
			throws IOException {

		long result = src.remaining();
		BARR barr = new BARR();
		GZIPOutputStream go = new GZIPOutputStream(barr);
		IO.write(go, src);
		go.close();
		src = barr.wrap();

		writeFully(fd, src);

		if (src.hasRemaining()) {
			throw new Error("should not happen");
		}

		return result;
	}

	public static long writeFully(WritableByteChannel fd, ByteBuffer src)
			throws IOException {

		long written = 0;

		written += fd.write(src);
		if (src.hasRemaining()) {
			while (src.hasRemaining()) {
				// FIXME: use Thread.sleep() in stead?
				Thread.yield();
				written += fd.write(src);
			}
		}

		return written;
	}

	/**
	 * @param e
	 * @return
	 */
	public static int exception_to_posix_code(IOException e) {
		
		if (e instanceof java.nio.channels.ClosedChannelException) {
			return Posix.ENOTCONN;
		}
		
		if ("Broken pipe".equals(e.getMessage())
			|| "Connection reset by peer".equals(e.getMessage())) {
			return Posix.ENOTCONN;
		}
		
		if (e instanceof ConnectException) {
			if ("Connection refused".equals(e.getMessage())) {
				return Posix.ECONNREFUSED;
			}
		}
		
		if (e instanceof BindException) {
			if ("Can't assign requested address".equals(e.getMessage())) {
				return Posix.EADDRNOTAVAIL;
			}
		}
		
		if (e instanceof java.net.SocketException) {
			if ("Network is unreachable".equals (e.getMessage())) {
				return Posix.ENETUNREACH;
			}
		}
		
		System.err.println("unknown exception: "+e);
		e.printStackTrace(System.err);
		
		// TODO: implement some more error codes here
		return Posix.EUNKNOWN;
	}

	public static long writev(GatheringByteChannel fd, ByteBuffer[] iov)
			throws IOException {
		int cnt = 0;
		long written = 0;
		while (cnt < iov.length) {
			int b = iov.length - cnt;
			if (iov[cnt].hasRemaining()) {
				if (b == 1) {
					written += fd.write(iov[cnt]);
				} else {
					written += fd.write(iov, cnt, b);
				}
			} else {
				cnt++;
			}
		}
		return written;
	}

	/**
	 * Do a strcpy on ByteBuffer. Assuming
	 * 
	 * @param data
	 * @return
	 */
	public static String strcpy(ByteBuffer data) {

		int end_pos;
		for (end_pos = data.position(); end_pos < data.limit(); end_pos++) {
			if (data.get(end_pos) == 0) {
				byte[] bb = data.array();
				int arr_off = data.arrayOffset() + data.position();
				int len = end_pos - data.position();

				String result = new String(bb, arr_off, len, ISO_LATIN_1);
				data.position(end_pos + 1);
				return result;
			}
		}

		throw ERT.badarg();
	}

	/**
	 * @param buf
	 * @param err
	 */
	public static void putstr(ByteBuffer buf, String err, boolean term) {
		for (int i = 0; i < err.length(); i++) {
			buf.put((byte) err.charAt(i));
		}
		if (term) {
			buf.put((byte) 0);
		}
	}

	public static String getstr(ByteBuffer buf, boolean term) {
		if (term) {
			StringBuilder sb = new StringBuilder();
			byte b;
			while ((b=buf.get()) != 0) {
				sb.append((char)(0xff & b));
			}
			return sb.toString();
			
		} else {
			String str = new String(buf.array(), buf.arrayOffset()+buf.position(), buf
					.remaining(), ISO_LATIN_1);
			buf.position(buf.limit());
			return str;
		}
	}

}
