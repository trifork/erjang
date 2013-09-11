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

package erjang.driver.zlib;

import java.io.IOException;
import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

import com.jcraft.jzlib.CRC32;
import com.jcraft.jzlib.Deflater;
import com.jcraft.jzlib.Inflater;
import com.jcraft.jzlib.JZlib;
import com.jcraft.jzlib.ZStream;
import com.jcraft.jzlib.ZStreamException;

import kilim.Pausable;
import kilim.Task;
import erjang.EBinary;
import erjang.EHandle;
import erjang.EPID;
import erjang.EString;
import erjang.NotImplemented;
import erjang.driver.EAsync;
import erjang.driver.EDriver;
import erjang.driver.EDriverInstance;
import erjang.driver.IO;

public class ZLibDriver extends EDriverInstance {

	
	
	// flush argument encoding
	public static final int Z_NO_FLUSH = 0;
	public static final int Z_SYNC_FLUSH = 2;
	public static final int Z_FULL_FLUSH = 3;
	public static final int Z_FINISH = 4;

	// compression level
	public static final int Z_NO_COMPRESSION = 0;
	public static final int Z_BEST_SPEED = 1;
	public static final int Z_BEST_COMPRESSION = 9;
	public static final int Z_DEFAULT_COMPRESSION = (-1);

	// compresssion strategy
	public static final int Z_FILTERED = 1;
	public static final int Z_HUFFMAN_ONLY = 2;
	public static final int Z_DEFAULT_STRATEGY = 0;

	// deflate compression method
	public static final int Z_DEFLATED = 8;

	public static final int Z_NULL = 0;

	public static final int MAX_WBITS = 15;

	// gzip defs (rfc 1952)

	public static final int ID1 = 0x1f;
	public static final int ID2 = 0x8b;

	public static final int FTEXT = 0x01;
	public static final int FHCRC = 0x02;
	public static final int FEXTRA = 0x04;
	public static final int FNAME = 0x08;
	public static final int FCOMMENT = 0x10;
	public static final int RESERVED = 0xE0;

	public static final int OS_MDDOS = 0;
	public static final int OS_AMIGA = 1;
	public static final int OS_OPENVMS = 2;
	public static final int OS_UNIX = 3;
	public static final int OS_VMCMS = 4;
	public static final int OS_ATARI = 5;
	public static final int OS_OS2 = 6;
	public static final int OS_MAC = 7;
	public static final int OS_ZSYS = 8;
	public static final int OS_CPM = 9;
	public static final int OS_TOP20 = 10;
	public static final int OS_NTFS = 11;
	public static final int OS_QDOS = 12;
	public static final int OS_ACORN = 13;
	public static final int OS_UNKNOWN = 255;

	public static final int DEFLATE_INIT = 1;
	public static final int DEFLATE_INIT2 = 2;
	public static final int DEFLATE_SETDICT = 3;
	public static final int DEFLATE_RESET = 4;
	public static final int DEFLATE_END = 5;
	public static final int DEFLATE_PARAMS = 6;
	public static final int DEFLATE = 7;

	public static final int INFLATE_INIT = 8;
	public static final int INFLATE_INIT2 = 9;
	public static final int INFLATE_SETDICT = 10;
	public static final int INFLATE_SYNC = 11;
	public static final int INFLATE_RESET = 12;
	public static final int INFLATE_END = 13;
	public static final int INFLATE = 14;

	public static final int CRC32_0 = 15;
	public static final int CRC32_1 = 16;
	public static final int CRC32_2 = 17;

	public static final int SET_BUFSZ = 18;
	public static final int GET_BUFSZ = 19;
	public static final int GET_QSIZE = 20;

	public static final int ADLER32_1 = 21;
	public static final int ADLER32_2 = 22;

	public static final int CRC32_COMBINE = 23;
	public static final int ADLER32_COMBINE = 24;

	private Inflater inflater;
	private Deflater deflater;

	public ZLibDriver(EDriver driver, EString command) {
		super(driver);
	}
	
	ArrayList<ByteBuffer> inbuf = new ArrayList<ByteBuffer>();

	@Override
	protected void output(EHandle caller, ByteBuffer buf) throws IOException,
			Pausable {
		inbuf.add(buf);
	}
	
	@Override
	protected void outputv(EHandle caller, ByteBuffer[] buf) throws IOException,
			Pausable {
		for (int i = 0; i < buf.length; i++) {
			inbuf.add(buf[i]);
		}
	}
	
	int do_inflate(int flush_mode) throws Pausable {
		int err = JZlib.Z_OK;
		
		read_loop:
		for (int i = 0; err==JZlib.Z_OK && i < inbuf.size(); i++) {
			
			ByteBuffer bb = inbuf.get(i);
			
			inflater.setInput(bb.array(), bb.arrayOffset() + bb.position(), bb.remaining(), true);

			int in_size = bb.remaining();
			ByteBuffer out = ByteBuffer.allocate(1024);				
			do {
				long read_before = inflater.total_in;
				long wrote_before = inflater.total_out;

				inflater.next_out = out.array();
				inflater.next_out_index = out.arrayOffset() + out.position();
				inflater.avail_out = out.remaining();
					
				err = inflater.inflate(flush_mode);

				if (err == JZlib.Z_OK || err == JZlib.Z_STREAM_END) {
				
					long read_after = inflater.total_in;
					long wrote_after = inflater.total_out;
					
					in_size -= (read_after-read_before);
					
					int written = (int) (wrote_after - wrote_before);
					out.position(written);
					
					driver_output(out);
					out = ByteBuffer.allocate(1024);				
				}

			} while(err==JZlib.Z_OK && in_size > 0);
		}
		
		inbuf.clear();
		
		if (flush_mode == Z_FINISH) {
			inflater.free();
		}
		
		return err;
	}
	
	void do_deflate(int flush_mode) throws Pausable {
		int err = JZlib.Z_OK;

		read_loop:
		for (int i = 0; err==JZlib.Z_OK && i < inbuf.size(); i++) {

			ByteBuffer bb = inbuf.get(i);

			deflater.next_in = bb.array();
			deflater.next_in_index = bb.arrayOffset() + bb.position();
			deflater.avail_in = bb.remaining();

	//		inf.setInput(bb.array(), bb.arrayOffset()+bb.position(), bb.limit());

			int in_size = bb.remaining();
			do {
				ByteBuffer out = ByteBuffer.allocate(1024);				

				long read_before = deflater.total_in;
				long wrote_before = deflater.total_out;

				deflater.next_out = out.array();
				deflater.next_out_index = out.arrayOffset() + out.position();
				deflater.avail_out = out.remaining();

				err = deflater.deflate(flush_mode);

				if (err == JZlib.Z_OK || err == JZlib.Z_STREAM_END) {

					long read_after = deflater.total_in;
					long wrote_after = deflater.total_out;

					in_size -= (read_after-read_before);

					int written = (int) (wrote_after - wrote_before);
					out.position(written);

					driver_output(out);

				}

			} while(err==JZlib.Z_OK && in_size > 0);
		}

		inbuf.clear();

		if (flush_mode == Z_FINISH) {
			deflater.free();
		}
	}
	
	@Override
	protected ByteBuffer control(EPID caller, int command, ByteBuffer cmd)
			throws Pausable {

		switch (command) {
		case INFLATE_INIT:
		{
			this.inflater = new Inflater();
			this.inflater.inflateInit(true);
			return reply_ok();
		}
		case INFLATE_INIT2:
		{
			int ws = cmd.getInt();
			this.inflater = new Inflater();
			this.inflater.inflateInit(ws);
			return reply_ok();
		}
		case DEFLATE_INIT2:
		{
			int level = cmd.getInt();
			int method = cmd.getInt();
			int bits = cmd.getInt();
			int memlevel = cmd.getInt();
			int strategy = cmd.getInt();
			this.deflater = new Deflater();
			this.deflater.deflateInit(level, bits, memlevel);
			this.deflater.deflateParams(level, strategy);
			return reply_ok();
		}
		case INFLATE: {
			int flush_mode = cmd.getInt();
			int err = do_inflate(flush_mode);
			if (err == JZlib.Z_NEED_DICT) {
				return reply_need_dict(inflater.getAdler());
			}
			return reply_ok();
		}
		case INFLATE_END: {
			do_inflate(Z_FINISH);
			return reply_ok();
		}
		case DEFLATE: {
			int flush_mode = cmd.getInt();
			do_deflate(flush_mode);
			return reply_ok();
		}
		case DEFLATE_END: {
			do_deflate(Z_FINISH);
			return reply_ok();
		}
		case CRC32_0: {
			if (deflater != null) {
				return reply_int( deflater.getAdler() );
			}
			if (inflater != null) {
				return reply_int( inflater.getAdler() );
			}
			return reply_int(0);
		}
		case CRC32_1: {
			CRC32 crc = new CRC32();
			crc.update(cmd.array(), cmd.arrayOffset()+cmd.position(), cmd.remaining());
			return reply_int((int) crc.getValue());
		}
		case CRC32_2: {
			CRC32 crc = new CRC32();
			long init = cmd.getInt() & 0xffffffffL;
			crc.reset(init);
			crc.update(cmd.array(), cmd.arrayOffset()+cmd.position(), cmd.remaining());
			return reply_int((int) crc.getValue());
		}
		}

		throw new erjang.NotImplemented("command="+command+"; data="+EBinary.make(cmd));
	}

	private ByteBuffer reply_ok() {
		ByteBuffer ok = ByteBuffer.allocate(3);
		ok.put((byte) 0);
		ok.put((byte) 'o');
		ok.put((byte) 'k');
		return ok;
	}

	private ByteBuffer reply_error(String atom) {
		ByteBuffer err = ByteBuffer.allocate(1 + atom.length());
		err.put((byte) 1);
		err.put( atom.getBytes(StandardCharsets.UTF_8) );
		return err;
	}

	private ByteBuffer reply_int(long l) {
		ByteBuffer ok = ByteBuffer.allocate(5);
		ok.put((byte) 2);
		ok.putInt((int)l);
		return ok;
	}

	private ByteBuffer reply_need_dict(long l) {
		ByteBuffer ok = ByteBuffer.allocate(5);
		ok.put((byte) 3);
		ok.putInt((int) l);
		return ok;
	}

	@Override
	protected void readyAsync(EAsync data) throws Pausable {
		data.ready();
	}

}
