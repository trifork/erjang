/** -*- tab-width: 4 -*-
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


package erjang.driver.ram_file;

import erjang.EHandle;
import erjang.EObject;
import erjang.EPID;
import erjang.EString;
import erjang.EBinary;
import erjang.ERef;
import erjang.NotImplemented;

import erjang.driver.EDriverInstance;
import erjang.driver.EAsync;
import erjang.driver.IO;
import erjang.driver.efile.Posix;

import java.io.IOException;
import java.io.ByteArrayOutputStream;

import java.util.zip.Inflater;

import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;

import kilim.Pausable;

import static java.util.zip.GZIPInputStream.GZIP_MAGIC;

import static erjang.driver.efile.EFile.FILE_OPEN;
import static erjang.driver.efile.EFile.FILE_READ;
import static erjang.driver.efile.EFile.FILE_LSEEK;
import static erjang.driver.efile.EFile.FILE_WRITE;
import static erjang.driver.efile.EFile.FILE_FSYNC;
import static erjang.driver.efile.EFile.FILE_TRUNCATE;

import static erjang.driver.efile.EFile.FILE_RESP_OK;
import static erjang.driver.efile.EFile.FILE_RESP_ERROR;
import static erjang.driver.efile.EFile.FILE_RESP_DATA;
import static erjang.driver.efile.EFile.FILE_RESP_NUMBER;
import static erjang.driver.efile.EFile.FILE_RESP_INFO;

import static erjang.driver.efile.EFile.EFILE_MODE_READ;
import static erjang.driver.efile.EFile.EFILE_MODE_WRITE;
import static erjang.driver.efile.EFile.EFILE_MODE_READ_WRITE;

public class RamFile extends EDriverInstance {
	/* Commands like file interface's */
	public static final int FILE_PREAD		= 17;
	public static final int FILE_PWRITE		= 18;

	/* Special ram_file commands */
	public static final int RAM_FILE_GET		= 30;
	public static final int RAM_FILE_SET		= 31;
	public static final int RAM_FILE_GET_CLOSE	= 32;
	public static final int RAM_FILE_COMPRESS	= 33;
	public static final int RAM_FILE_UNCOMPRESS	= 34;
	public static final int RAM_FILE_UUENCODE	= 35;
	public static final int RAM_FILE_UUDECODE	= 36;
	public static final int RAM_FILE_SIZE		= 37;

	private static final byte[] FILE_RESP_NUMBER_HEADER = new byte[]{ FILE_RESP_NUMBER };

	private ByteBuffer contents;
	private int flags;

	RamFile(EString command, Driver driver) {
		super(driver);
	}

	@Override
	protected void outputv(EHandle caller, ByteBuffer[] ev) throws IOException, Pausable {
		if (ev.length == 0 || ev[0].remaining() == 0) {
			reply_posix_error(Posix.EINVAL);
			return;
		}

		byte command = ev[0].get();
		//TODO: Handle more commands here, without flattening.
		switch (command) {
// 		case FILE_CLOSE: {
// 			if (ev.length > 1 && ev[0].hasRemaining()) {
// 				reply_posix_error(Posix.EINVAL);
// 				return;
// 			}
// 			ByteBuffer last = ev[ev.length - 1];
// 			throw new NotImplemented();
// 		}

		default:
			// undo the get() we did to find command
			ev[0].position(ev[0].position() - 1);
			output(caller, flatten(ev));
		} // switch
	}
	protected void output(EHandle caller, ByteBuffer data) throws IOException, Pausable {
		byte cmd = data.get();
		switch (cmd) {
 		case FILE_OPEN: {
			if (data.remaining() < 4) {
				reply_posix_error(Posix.EINVAL);
				return;
			}
			flags = data.getInt();
			try {
				contents = ByteBuffer.allocate(data.remaining()).put(data);
			} catch (OutOfMemoryError e) {
				reply_posix_error(Posix.ENOMEM);
				return;
			}
			reply_number(0);
		} break;
		case RAM_FILE_UNCOMPRESS: {
			if (data.hasRemaining()) {
				reply_posix_error(Posix.EINVAL);
				return;
			}

			// Uncompress only when a known header is there:
			data.mark();
			boolean is_gzip = (data.remaining() >= 2 && data.getShort() == GZIP_MAGIC);
			data.reset();

			if (is_gzip) {
				final Inflater inflater = new Inflater(true);
				inflater.setInput(contents.array(), 0, contents.limit());
				byte[] buf = new byte[4096];
				ByteArrayOutputStream baos = new ByteArrayOutputStream(contents.limit());
				try {
					while (! inflater.finished()) {
						int inflated = inflater.inflate(buf, 0, buf.length);
						baos.write(buf, 0, inflated);
					}
				} catch (Exception e) {
					System.err.println("DB| inflation error: "+e);
					reply_posix_error(Posix.EINVAL);
					return;
				}
				contents = ByteBuffer.wrap(baos.toByteArray());
			}
			reply_number(contents.limit());
		} break;

		case RAM_FILE_GET: {
			if (data.hasRemaining()) {
				reply_posix_error(Posix.EINVAL);
				return;
			}
			reply_buf(contents);
		} break;

		default:
			throw new NotImplemented("ram_file output command:" + ((int) cmd) + " "
									 + EBinary.make(data));
		} // switch
	}


	public void reply_ok() throws Pausable {
		ByteBuffer header = ByteBuffer.allocate(1);
		header.put(FILE_RESP_OK);
		driver_output2(header, null);
	}

	public void reply_number(int val) throws Pausable {
		ByteBuffer reply = ByteBuffer.allocate(1 + 4);
		reply.put(FILE_RESP_NUMBER);
		reply.putInt(val);
		driver_output2(reply, null);
	}

	void reply_buf(ByteBuffer buf) throws Pausable {
		ByteBuffer header = ByteBuffer.allocate(1 + 4);
		header.put(FILE_RESP_DATA);
		header.putInt(buf.position());
		driver_output2(header, buf);
	}

	/**
	 * @param reply_Uint
	 * @throws Pausable 
	 */
	public void reply_Uint(int value) throws Pausable {
		ByteBuffer response = ByteBuffer.allocate(4);
		response.putInt(value);
		driver_output2(response, null);
	}

	/**
	 * @param error
	 * @throws Pausable 
	 */
	public void reply_posix_error(int posix_errno) throws Pausable {
		ByteBuffer response = ByteBuffer.allocate(256);
		response.put(FILE_RESP_ERROR);
		String err = Posix.errno_id(posix_errno);
		IO.putstr(response, err, false);

		driver_output2(response, null);
	}

	@Override
	protected EObject call(EPID caller, int command, EObject data) throws Pausable {
		// TODO
		return null;
	}

	@Override
	protected void flush() throws Pausable {
		// TODO
	}

	@Override
	public void processExit(ERef monitor) throws Pausable {
		// TODO

	}

	@Override
	protected void readyInput(SelectableChannel ch) throws Pausable {
		// TODO Auto-generated method stub

	}

	@Override
	protected void readyOutput(SelectableChannel evt) throws Pausable {
		// TODO Auto-generated method stub

	}

	@Override
	protected void timeout() throws Pausable {
		// TODO
	}

	@Override
	protected void readyAsync(EAsync data) throws Pausable {
		// TODO
	}
}
