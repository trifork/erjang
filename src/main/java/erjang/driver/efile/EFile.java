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

package erjang.driver.efile;

import java.io.File;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.channels.SelectableChannel;
import java.nio.charset.Charset;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.LinkedList;
import java.util.Queue;
import java.util.concurrent.locks.Lock;

import kilim.Pausable;
import erjang.EBinary;
import erjang.EHandle;
import erjang.EPort;
import erjang.ERT;
import erjang.ERef;
import erjang.EString;
import erjang.NotImplemented;
import erjang.driver.EAsync;
import erjang.driver.EDriverInstance;
import erjang.driver.IO;

/**
 * Java does nor support proper non-blocking IO for files (i.e., you cannot
 * select on a file). Select is only supported for sockets (and server sockets).
 */
public class EFile extends EDriverInstance {

	private static Field FileDescriptor_FD;
	
	static {
		try {
			FileDescriptor_FD = FileDescriptor.class.getDeclaredField("fd");
			FileDescriptor_FD.setAccessible(true);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	private int getFDnumber(FileDescriptor fd) throws IllegalAccessException {
		int res = FileDescriptor_FD.getInt(fd);
		// Windows doesn't provide FD numbers - fallback to a usable dummy:
		if (res == -1) res = 255;
		return res;
	}

	/**
	 * 
	 */
	private static final Charset ISO_8859_1 = Charset.forName("ISO_8859_1");
	private final EString command;
	private FileChannel fd;
	private EPort port;
	private EPort key;
	private int flags;
	private TimerState timer_state;
	private FileAsync invoke;
	private Queue<FileAsync> cq;
	private int level;
	private Lock q_mtx;
	private int write_buffered;
	private int write_bufsize;

	public int posix_errno;
	public boolean write_error;
	private long write_delay;
	private ByteBuffer read_binp;
	protected File name;

	/**
	 * 
	 */
	private final class WriteAsync extends FileAsync {
		Lock q_mtx;
		int size, free_size, reply_size;

		/**
		 * @param xreplySize
		 * @param efile
		 */
		private WriteAsync(boolean reply, int reply_size) {

			EFile efile = EFile.this;

			super.command = FILE_WRITE;
			super.fd = efile.fd;
			super.flags = efile.flags;
			super.level = 1;

			this.q_mtx = efile.q_mtx;
			this.size = efile.write_buffered;

			super.reply = reply;

			this.free_size = 0;
			this.reply_size = reply_size;
		}

		/** invoke_writev */
		@Override
		public void async() {
			int size;

			boolean segment = again && this.size >= 2 * FILE_SEGMENT_WRITE;
			if (segment) {
				size = FILE_SEGMENT_WRITE;
			} else {
				size = this.size;
			}

			q_mtx.lock();
			ByteBuffer[] iov0 = driver_peekq();
			if (iov0 != null) {

				// copy the buffers
				ByteBuffer[] iov = iov0.clone();
				q_mtx.unlock();

				// figure out how much data we have available for writing
				long p = 0;
				int iovcnt = 0;
				while (p < size && iovcnt < iov.length) {
					p += iov[iovcnt++].remaining();
				}

				if (iov.length > 0) {
					// What is this good for?
					assert iov[iovcnt - 1].limit() > p - size;

					if ((flags & EFILE_COMPRESSED) == EFILE_COMPRESSED) {
						for (int i = 0; i < iovcnt; i++) {
							try {
								free_size += IO.gzwrite(fd, iov[i]);
								super.result_ok = true;
							} catch (IOException e) {
								posix_errno = IO.exception_to_posix_code(e);
								super.result_ok = false;
							}
						}
					} else {
						try {

							long written_bytes = IO.writev(fd, iov);
							
							if (ERT.DEBUG_EFILE) {
								System.err.println(""+EFile.this+" :: wrote "+written_bytes);
							}
							
							free_size += written_bytes;
							result_ok = true;
						} catch (IOException e) {
							posix_errno = IO.exception_to_posix_code(e);
							super.result_ok = false;

						}
					}
				} else if (iov.length == 0) {
					result_ok = true;
				}
			} else {
				q_mtx.unlock();
				posix_errno = Posix.EINVAL;
				result_ok = false;
			}

			if (!result_ok) {
				again = false;
			} else if (!segment) {
				again = false;
			}
		}

		// called from the DriverTask
		@Override
		public void ready() throws Pausable {
			if (reply) {
				if (!result_ok) {
					reply_posix_error(posix_errno);
				} else {
					reply_Uint(reply_size);
				}

			} else {
				if (!result_ok) {
					EFile.this.write_error = true;
					EFile.this.posix_errno = posix_errno;
				}

			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see erjang.driver.efile.FileAsync#deq_free_size()
		 */
		@Override
		public void deq_free_size() {
			driver_deq(free_size);
		}
	}

	/**
	 * 
	 */
	public enum TimerState {
		IDLE, AGAIN, WRITE
	}

	/*
	 * This structure contains date and time.
	 */
	public static class Time {
		short year; /* (4 digits). */
		short month; /* (1..12). */
		short day; /* (1..31). */
		short hour; /* (0..23). */
		short minute; /* (0..59). */
		short second; /* (0..59). */
	}

	/** stat info about file */
	public static class Info {
		int size_low; /* Size of file, lower 32 bits.. */
		int size_high; /* Size of file, higher 32 bits. */
		int type; /* Type of file -- one of FT_*. */
		int access; /* Access to file -- one of FA_*. */
		int mode; /* Access permissions -- bit field. */
		int links; /* Number of links to file. */
		int major_device; /* Major device or file system. */
		int minor_device; /* Minor device (for devices). */
		int inode; /* Inode number. */
		int uid; /* User id of owner. */
		int gid; /* Group id of owner. */
		Time accessTime; /* Last time the file was accessed. */
		Time modifyTime; /* Last time the file was modified. */
		Time cTime; /* Creation time (Windows) or last */
	}

	/*
	 * Open modes for efile_openfile().
	 */
	public static final int EFILE_MODE_READ = 1;
	public static final int EFILE_MODE_WRITE = 2; /*
												 * Implies truncating file when
												 * used alone.
												 */
	public static final int EFILE_MODE_READ_WRITE = 3;
	public static final int EFILE_MODE_APPEND = 4;
	public static final int EFILE_COMPRESSED = 8;
	public static final int EFILE_NO_TRUNCATE = 16; /*
													 * Special for reopening on
													 * VxWorks
													 */

	/*
	 * Seek modes for efile_seek().
	 */
	public static final int EFILE_SEEK_SET = 0;
	public static final int EFILE_SEEK_CUR = 1;
	public static final int EFILE_SEEK_END = 2;

	/*
	 * File types returned by efile_fileinfo().
	 */
	public static final int FT_DEVICE = 1;
	public static final int FT_DIRECTORY = 2;
	public static final int FT_REGULAR = 3;
	public static final int FT_SYMLINK = 4;
	public static final int FT_OTHER = 5;

	/*
	 * Access attributes returned by efile_fileinfo() (the bits can be ORed
	 * together).
	 */
	public static final int FA_NONE = 0;
	public static final int FA_WRITE = 1;
	public static final int FA_READ = 2;
	public static final int FA_EXECUTE = 4;

	/* commands sent via efile_output(v) */

	public static final int FILE_OPEN = 1; /* Essential for startup */
	public static final int FILE_READ = 2;
	public static final int FILE_LSEEK = 3;
	public static final int FILE_WRITE = 4;
	public static final int FILE_FSTAT = 5; /* Essential for startup */
	public static final int FILE_PWD = 6; /* Essential for startup */
	public static final int FILE_READDIR = 7; /* Essential for startup */
	public static final int FILE_CHDIR = 8;
	public static final int FILE_FSYNC = 9;
	public static final int FILE_MKDIR = 10;
	public static final int FILE_DELETE = 11;
	public static final int FILE_RENAME = 12;
	public static final int FILE_RMDIR = 13;
	public static final int FILE_TRUNCATE = 14;
	public static final int FILE_READ_FILE = 15; /* Essential for startup */
	public static final int FILE_WRITE_INFO = 16;
	public static final int FILE_LSTAT = 19;
	public static final int FILE_READLINK = 20;
	public static final int FILE_LINK = 21;
	public static final int FILE_SYMLINK = 22;
	public static final int FILE_CLOSE = 23;
	public static final int FILE_PWRITEV = 24;
	public static final int FILE_PREADV = 25;
	public static final int FILE_SETOPT = 26;
	public static final int FILE_IPREAD = 27;
	public static final int FILE_ALTNAME = 28;
	public static final int FILE_READ_LINE = 29;

	/* Return codes */

	public static final byte FILE_RESP_OK = 0;
	/**
	 * 
	 */
	private static final byte[] FILE_RESP_OK_HEADER = new byte[]{ FILE_RESP_OK };
	public static final byte FILE_RESP_ERROR = 1;
	public static final byte FILE_RESP_DATA = 2;
	public static final byte FILE_RESP_NUMBER = 3;
	public static final byte FILE_RESP_INFO = 4;
	public static final byte FILE_RESP_NUMERR = 5;
	public static final byte FILE_RESP_LDATA = 6;
	public static final byte FILE_RESP_N2DATA = 7;
	public static final byte FILE_RESP_EOF = 8;

	/* Options */

	public static final int FILE_OPT_DELAYED_WRITE = 0;
	public static final int FILE_OPT_READ_AHEAD = 1;

	/* IPREAD variants */

	public static final int IPREAD_S32BU_P32BU = 0;

	/* Limits */

	public static final int FILE_SEGMENT_READ = (256 * 1024);
	public static final int FILE_SEGMENT_WRITE = (256 * 1024);

	public static final int FILE_TYPE_DEVICE = 0;
	public static final int FILE_TYPE_DIRECTORY = 2;
	public static final int FILE_TYPE_REGULAR = 3;
	public static final int FILE_TYPE_SYMLINK = 4;

	public static final int FILE_ACCESS_NONE = 0;
	public static final int FILE_ACCESS_WRITE = 1;
	public static final int FILE_ACCESS_READ = 2;
	public static final int FILE_ACCESS_READ_WRITE = 3;
//	private static final String SYS_INFO = null;

	private static final int THREAD_SHORT_CIRCUIT;

	/** initialize value of thread_short_circuit */
	static {
		String buf = System.getenv("ERL_EFILE_THREAD_SHORT_CIRCUIT");
		if (buf == null) {
			THREAD_SHORT_CIRCUIT = 0;
		} else {
			THREAD_SHORT_CIRCUIT = Integer.parseInt(buf);
		}
	}

	/**
	 * @param command
	 * @param driver 
	 */
	public EFile(EString command, Driver driver) {
		super(driver);
		this.command = command;

		this.fd = (FileChannel) null;
		this.key = port;
		this.flags = 0;
		this.invoke = null;
		this.cq = new LinkedList<FileAsync>();
		this.timer_state = TimerState.IDLE;
		// this.read_bufsize = 0;
		this.read_binp = (ByteBuffer) null;
		// this.read_offset = 0;
		//this.read_size = 0;
		this.write_delay = 0L;
		this.write_bufsize = 0;
		// this.write_error = 0;
		this.q_mtx = driver_pdl_create();
		this.write_buffered = 0;

	}

	/**
	 * @param replySize
	 * @throws Pausable 
	 */
	public void reply_Uint(int value) throws Pausable {
		ByteBuffer response = ByteBuffer.allocate(4);
		response.putInt(value);
		driver_output2(response, null);
	}

	public void reply_Uint_error(int value, int posix_error) throws Pausable {
		ByteBuffer response = ByteBuffer.allocate(256);
		String err = Posix.errno_id(posix_error);		
		response.putInt(value);
		IO.putstr(response, err, false);
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
	protected void flush() throws Pausable {
		int r = flush_write(null);
		assert (r == 0);
		cq_execute();
	}

	private abstract class SimpleFileAsync extends FileAsync {

		protected final File file;
		protected final String name;

		{
			level = 2;
			reply = true;
		}

		/**
		 * @param path
		 * @param cmd
		 */
		public SimpleFileAsync(byte command, String path) {
			this.command = command;
			this.name = path;
			this.file = new File(path);
		}

		@Override
		public final void async() {
			try {
				this.result_ok = false;
				run();
			} catch (OutOfMemoryError e) {
				posix_errno = Posix.ENOMEM;
			} catch (SecurityException e) {
				posix_errno = Posix.EPERM;
			} catch (Throwable e) {
				e.printStackTrace();
				posix_errno = Posix.EUNKNOWN;
			}
		}

		/**
		 * This is what does the real operation
		 */
		protected abstract void run();

		public void ready() throws Pausable {
			reply(EFile.this);
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.driver.EDriverInstance#outputv(java.nio.ByteBuffer[])
	 */
	@Override
	protected void outputv(EHandle caller, ByteBuffer[] ev) throws Pausable {

		if (ev.length == 0 || ev[0].remaining() == 0) {
			reply_posix_error(Posix.EINVAL);
			return;
		}

		byte command = ev[0].get();
		switch (command) {
		case FILE_PREADV: {
			
			ByteBuffer evin = flatten(ev);
			
			evin.getInt(); // skip first 4-byte			
			final int n = evin.getInt();
			
			final long[] offsets = new long[n];
			final ByteBuffer[] res_ev = new ByteBuffer[n+1];
			for (int i = 1; i < n+1; i++) {
				offsets[i-1] = evin.getLong();
				int len = (int) (evin.getLong() & 0x7fffffff);
			
				res_ev[i] = ByteBuffer.allocate(len);
				
				if (ERT.DEBUG_EFILE) {
					System.err.println(EFile.this+" :: pread "+len+" @ "+offsets[i-1]);
				}
			}
			
			res_ev[0] = ByteBuffer.allocate(4+4+8*n);
			res_ev[0].putInt(0);
			res_ev[0].putInt(n);
			
			if (n == 0) {
				reply_ev(FILE_RESP_LDATA, offsets, res_ev);
				
			} else {
				
				cq_enq(new FileAsync() {
					
					int cnt;
					
					{ 
						this.level = 1; 
						this.fd = EFile.this.fd; 
						this.cnt = 0;
						this.result_ok = true;
					}
					
					@Override
					public void async() {			
						
						// TODO: handle CHOP!
						
						for (int i = cnt; i < n; i++) {
							ByteBuffer res = res_ev[i+1];
							long pos = offsets[i] + res.position();

							int rem = res.remaining();
							if (rem > 0) {
								
								int bytes_read;
								try {
									fd.position(pos);
									bytes_read = fd.read(res);
									
									if (ERT.DEBUG_EFILE) {
										System.err.println(EFile.this + 
															":: did pread "+ bytes_read+ "@"+pos
															+" into res["+i+"]; missing="+res.remaining());
									}
								} catch (IOException e) {
									result_ok = false;
									this.posix_errno = IO.exception_to_posix_code(e);
									this.again = false;
									return;
								}
								
								if (bytes_read >= 0 && res.hasRemaining()) {
									this.again = true;
									return;
								}
								
								res_ev[0].putLong(res.position());
								cnt += 1;
							}
							
						}
						
						this.again = false;
					}
					
					@Override
					public void ready() throws Pausable {
						if (!result_ok) {
							reply_posix_error(posix_errno);
						} else {
							reply_ev(FILE_RESP_LDATA, offsets, res_ev);
						}
					}
					

				});
			}
			
		} break;
		
		
		case FILE_CLOSE: {
			if (ev.length > 1 && ev[0].hasRemaining()) {
				reply_posix_error(Posix.EINVAL);
				return;
			}
			ByteBuffer last = ev[ev.length - 1];

			if (last.hasRemaining()) {
				reply_posix_error(Posix.EINVAL);
				return;
			}
			
			if (ERT.DEBUG_EFILE) 
			System.err.println(""+this+" :: close");

			//TODO: Flush & check.

			// Is this check necessary?
			/*
			if (fd == null) {
				reply_posix_error(Posix.BADF);
				return;
			}
			*/

			FileAsync d = new FileAsync() {
				{
					this.fd = EFile.this.fd;
				}
				public void async() {
					try {
						fd.close();
						result_ok = true;
					} catch (IOException e) {
						result_ok = false;
						posix_errno = IO.exception_to_posix_code(e);
					}
				}

				@Override
				public void ready() throws Pausable {
					if (result_ok) {
						reply_ok();
					} else {
						reply_posix_error(posix_errno);
					}
				}
			};
			cq_enq(d);
			break;
		}

		case FILE_READ: {
			
			int[] errp = new int[1];
			if (flush_write_check_error(errp) < 0) {
				reply_posix_error(errp[0]);
				return;
			}
			
			if (ev.length > 1 && ev[0].hasRemaining()) {
				reply_posix_error(Posix.EINVAL);
				return;
			}
			ByteBuffer last = ev[ev.length - 1];

			if (last.remaining() != 8) {
				reply_posix_error(Posix.EINVAL);
				return;
			}

			final long size = last.getLong();
			if (size > Integer.MAX_VALUE || size < 0) {
				reply_posix_error(Posix.ENOMEM);
				return;
			}

			//TODO: Write-flush & check.

			FileAsync d = new FileAsync() {
				private ByteBuffer binp = null;
				{
					super.level = 2;
					super.again = true;
					this.fd = EFile.this.fd;
				}

				@Override
				public void async() {
					// first time only, initialize binp
					if (binp == null) {
						try {
							binp = ByteBuffer.allocate((int) size);
						} catch (OutOfMemoryError e) {
							result_ok = false;
							posix_errno = Posix.ENOMEM;
							return;
						}
					}

					if (binp != null && binp.hasRemaining()) {
						try {
							int want = binp.remaining();
							
							int bytes = fd.read(binp);

							if (ERT.DEBUG_EFILE) {
								System.err.println
								 	(EFile.this + ":: did read "+bytes+" bytes of "+want);
							}
							
							if (bytes == -1) {
								result_ok = true;
								again = false;
								return;
							}

							if (binp.hasRemaining()) {
								again = true;
								return;
							} else {
								result_ok = true;
							}
						} catch (IOException e) {
							result_ok = false;
							posix_errno = IO.exception_to_posix_code(e);
						}
					}

					again = false;
				}

				@Override
				public void ready() throws Pausable {
					if (!result_ok) {
						reply_posix_error(posix_errno);
						return;
					}

					reply_buf(binp);
					binp.flip();
				}
			};
			cq_enq(d);
			break;
		} //break;
		case FILE_READ_FILE: {
			if (ev.length > 1 && ev[0].hasRemaining()) {
				reply_posix_error(Posix.EINVAL);
				return;
			}

			ByteBuffer last = ev[ev.length - 1];
			final String name = IO.getstr(last, false);

			if (name.length() == 0) {
				reply_posix_error(Posix.ENOENT);
				return;
			}

			FileAsync d = new FileAsync() {
				private ByteBuffer binp = null;
				private long size = 0;
				{
					super.level = 2;
					super.again = true;
				}

				@Override
				public void async() {

					// first time only, initialize binp
					if (binp == null) {
						File file = new File(name);
						try {
							this.fd = new FileInputStream(file).getChannel();
						} catch (FileNotFoundException e) {
							this.again = false;
							result_ok = false;
							posix_errno = fileNotFound_to_posixErrno(file, EFILE_MODE_READ);

							this.again = false;
							return;
						}
						this.size = file.length();

						if (size > Integer.MAX_VALUE || size < 0) {
							result_ok = false;
							posix_errno = Posix.ENOMEM;
						} else {
							try {
								binp = ByteBuffer.allocate((int) size);
							} catch (OutOfMemoryError e) {
								result_ok = false;
								posix_errno = Posix.ENOMEM;
							}
						}
					}

					if (binp != null && binp.hasRemaining()) {

						try {
							int bytes = fd.read(binp);
							if (bytes == -1 && binp.hasRemaining()) {
								// urgh, file change size under our feet!
								result_ok = false;
								posix_errno = Posix.EIO;
							}

							if (binp.hasRemaining()) {
								again = true;
								return;
							} else {
								result_ok = true;
							}
						} catch (IOException e) {
							result_ok = false;
							posix_errno = IO.exception_to_posix_code(e);
						}

					}

					try {
						fd.close();
					} catch (IOException e) {
						result_ok = false;
						posix_errno = IO.exception_to_posix_code(e);
					}
					again = false;

				}

				@Override
				public void ready() throws Pausable {
					
					if (!result_ok) {
						reply_posix_error(posix_errno);
						return;
					}
					
					binp.flip();
					driver_output_binary(FILE_RESP_OK_HEADER, binp);
				}

			};

			cq_enq(d);
			break;
		}
		
		case FILE_PWRITEV: {
			int[] errp = new int[1];

			if (lseek_flush_read(errp) < 0) {
				reply_posix_error(errp[0]);
				return;
			}
			
			if (flush_write_check_error(errp) < 0) {
				reply_posix_error(errp[0]);
				return;
			}
			
			final int n = ev[0].getInt();
			
			if (n == 0) {
				if (ev.length > 1 || ev[0].hasRemaining()) {
					reply_posix_error(Posix.EINVAL);
				} else {
					reply_Uint(0);
				}
				return;
			}

			
			if (ev[0].remaining() != (8*2*n)) {
				reply_posix_error(Posix.EINVAL);
				return;
			}
			
			final long[] offsets = new long[n];
			final long[] sizes = new long[n];
			
			long total = 0;
			for (int i = 0; i < n; i++) {
				offsets[i] = ev[0].getLong();
				sizes[i] = ev[0].getLong();
				total += sizes[i];
			}

			if (total == 0) {
				reply_Uint(0);
				return;
			}
			
			q_mtx.lock();
			try {
				driver_enqv(ev);				
			} finally {
				q_mtx.unlock();
			}
			
			FileAsync d = new FileAsync() {
				
				int cnt = 0;
				
				{
					this.level = 1;
					this.fd = EFile.this.fd;
				}
				
				@Override
				public void async() {

					q_mtx.lock();
					ByteBuffer[] iov0 = driver_peekq();
					if (iov0 != null) {

						// copy the buffer list
						ByteBuffer[] iov = iov0.clone();
						q_mtx.unlock();
						int ip = 0;
						
						while (cnt < offsets.length && ip < iov.length) {
							
							if (sizes[cnt] == 0)
							{ cnt += 1; continue; }
							
							if(!iov[ip].hasRemaining()) { ip++; continue; }
							
							ByteBuffer o = iov[ip];
							if (o.remaining() > sizes[cnt]) {
								o = o.slice();
								o.limit((int) sizes[cnt]);
							}
							
							int bytes;
							try {
								fd.position(offsets[cnt]);
								bytes = fd.write(o);
								
								if (ERT.DEBUG_EFILE) {
									System.err.println(""+EFile.this+" :: wrote "+bytes);
								}

							} catch (IOException e) {
								EFile.this.posix_errno = IO.exception_to_posix_code(e);
								this.result_ok = false;
								return;
							}
							
							offsets[cnt] += bytes;
							sizes[cnt] -= bytes;
							
							if (o.hasRemaining())
							{
								this.again = true;
								return;
							}
							
							if (sizes[cnt] == 0) {
								cnt += 1;
							}
						}
						
						if (cnt != n) {
							this.result_ok = false;
							EFile.this.posix_errno = Posix.EINVAL;
							this.again = false;
						} else {
							this.again = false;
							this.result_ok = true;
						}
					}
					
				}

				@Override
				public void ready() throws Pausable {
					if (!result_ok) {
						reply_Uint_error(cnt, EFile.this.posix_errno);
					} else {
						reply_Uint(n);
					}
				}
				
			};

			cq_enq(d);
			break;
		}


		
		case FILE_WRITE: {
			int[] errp;
			int reply_size = 0;
			
			for (int i = 0; i < ev.length; i++) {
				reply_size += ev[i].remaining();
				if (ERT.DEBUG_EFILE)
					System.err.println(""+this+" :: write "+ev[i].remaining());
			}
			
			q_mtx.lock();
			driver_enqv(ev);
			write_buffered += reply_size;
			
			if (write_buffered < write_bufsize) {
				q_mtx.unlock();
				reply_Uint(reply_size);
				
				if (timer_state == TimerState.IDLE) {
					timer_state = TimerState.WRITE;
					driver_set_timer(write_delay);
				}					
			} else if (async_write(errp = new int[1], true, reply_size) != 0){
				reply_posix_error(errp[0]);
				q_mtx.unlock();
			} else {
				q_mtx.unlock();
			}
				
			break;
		}

		default:
			// undo the get() we did to find command
			ev[0].position(ev[0].position() - 1);
			output(caller, flatten(ev));
		}

		cq_execute();

	}
	
	private int flush_write_check_error(int[] errp) {
	    int r;
	    if ( (r = flush_write(errp)) != 0) {
			check_write_error(null);
			return r;
	    } else {
	    	return check_write_error(errp);
	    }
	}

	private int check_write_error(int[] errp) {
	    if (write_error) {
	    	if (errp != null) {
	    		errp[0] = this.posix_errno;
	    	}
	    	return -1;
        }
        return 0;
	}

	void flush_read() {
		this.read_binp = null;
	}

	private int lseek_flush_read(int[] errp) {
		int r = 0;
		int read_size = (read_binp == null ? 0 : read_binp.remaining());
		flush_read();
		if (read_size != 0) {
			if ((r = async_lseek(errp, false, -read_size, EFILE_SEEK_CUR)) < 0) {
			    return r;
			}
		}
		return r;
	}

	private int async_lseek(int[] errp, final boolean do_reply, final long off, final int whence) {
		
		try {
			FileAsync d = new FileAsync() {
	
				{
					this.reply = do_reply;
					this.level = 1;
					this.fd = EFile.this.fd;
				}
				
				long out_pos;
				
				@Override
				public void async() {
	
					if ((flags & EFILE_COMPRESSED) != 0) {
						this.result_ok = false;
						this.posix_errno = Posix.EINVAL;
					}
					
					try {
					
						switch (whence) {
						case EFILE_SEEK_SET:
							out_pos=off;
							break;
							
						case EFILE_SEEK_CUR:
							long cur = fd.position();
							out_pos= cur + off;
							break;
							
						case EFILE_SEEK_END:
							cur = fd.size();
							out_pos = cur - off;
							break;
							
						default:
							this.result_ok = false;
							this.posix_errno = Posix.EINVAL;
							return;
						}
	
						fd.position(out_pos);
						if (ERT.DEBUG_EFILE) {
							System.err.println(""+EFile.this+" :: seek "+out_pos);
						}
						
					} catch (IOException e) {
						this.result_ok = false;
						this.posix_errno = IO.exception_to_posix_code(e);
					}
					
					this.result_ok = true;
				}
	
				
				@Override
				public void ready() throws Pausable {
					if (reply) {
						if (result_ok) {
							EFile.this.fd = fd;
							ByteBuffer response = ByteBuffer.allocate(9);
							response.put(FILE_RESP_NUMBER);
							response.putLong(out_pos);
							driver_output2(response, null);
						} else {
							reply_posix_error(posix_errno);
						}
					}
				}
				
	
			};
			
			cq_enq(d);
		} catch (OutOfMemoryError e) {
			if (errp != null) {
				errp[0] = Posix.ENOMEM;
			}
			return -1;
		}
		return 0;
	}

	private void reply_ev(byte response, long[] offsets, ByteBuffer[] res_ev) throws Pausable {
		ByteBuffer tmp = ByteBuffer.allocate(1);
		tmp.put(response);
		driver_outputv(tmp, res_ev);
	}

	static FilenameFilter READDIR_FILTER = new FilenameFilter() {
		
		@Override
		public boolean accept(File dir, String name) {
			return !".".equals(name) && !"..".equals(name);
		}
	};

	@Override
	protected void output(EHandle caller, ByteBuffer buf) throws Pausable {
		FileAsync d;
		byte cmd = buf.get();
		switch (cmd) {
		
		case FILE_TRUNCATE: {
			d = new FileAsync() {
		
				{
					level = 2;
					fd = EFile.this.fd;
					command = FILE_TRUNCATE;
				}
				
				@Override
				public void async() {
					again = false;
					try {
						long pos = fd.position();
						fd.truncate(pos);					
						result_ok = true;
					} catch (IOException e) {
						result_ok = false;
						posix_errno = IO.exception_to_posix_code(e);
					}
				}

				@Override
				public void ready() throws Pausable {
					reply(EFile.this);
				}
				
			};
		} break;
		
		case FILE_FSYNC: {
			d = new FileAsync() {
		
				{
					level = 2;
					fd = EFile.this.fd;
					command = FILE_FSYNC;
				}
				
				@Override
				public void async() {
					again = false;
					try {
						fd.force(true);
						result_ok = true;
					} catch (IOException e) {
						result_ok = false;
						posix_errno = IO.exception_to_posix_code(e);
					}
				}

				@Override
				public void ready() throws Pausable {
					reply(EFile.this);
				}
				
			};
		} break;
		
		case FILE_MKDIR: {
			d = new SimpleFileAsync(cmd, IO.strcpy(buf)) {
				public void run() {
					result_ok = file.mkdir();
					if (!result_ok) {
						if (name.length() == 0) {
							posix_errno = Posix.ENOENT;
						} else if (file.exists()) {
							posix_errno = Posix.EEXIST;
						} else {
							posix_errno = Posix.EUNKNOWN;
						}
					}
				}

			};
			break;
		}

		case FILE_RMDIR: {
			d = new SimpleFileAsync(cmd, IO.strcpy(buf)) {
				public void run() {
					result_ok = file.isDirectory() && file.delete();
					if (!result_ok) {
						if (!file.exists()) {
							posix_errno = Posix.ENOENT;
						} else if (Posix.isCWD(name, file)) {
							posix_errno = Posix.EINVAL;
						} else if (file.exists()) {
							posix_errno = Posix.EEXIST;
						} else {
							posix_errno = Posix.EUNKNOWN;
						}
					}
				}

			};
			break;
		}
		
		case FILE_CHDIR: {
			d = new SimpleFileAsync(cmd, IO.strcpy(buf)) {
				public void run() {
					result_ok = file.isDirectory();
					if (!result_ok) {
						if (!file.exists()) {
							posix_errno = Posix.ENOENT;
						} else if (!file.isDirectory()) {
							posix_errno = Posix.EEXIST;
						} else {
							posix_errno = Posix.EUNKNOWN;
						}
					} else {
						System.setProperty("user.dir", this.name);
					}
				}
			};
			
			break;
		}

		case FILE_DELETE: {
			d = new SimpleFileAsync(cmd, IO.strcpy(buf)) {
				public void run() {
					result_ok = file.isFile() && file.delete();
					if (!result_ok) {
						if (!file.exists()) {
							posix_errno = Posix.ENOENT;
						} else if (file.isDirectory()) {
							posix_errno = Posix.EEXIST;
						} else {
							posix_errno = Posix.EUNKNOWN;
						}
					}
				}

			};
			break;
		}


		case FILE_PWD: {
			int drive = buf.get();
			char dr = drive==0 ? '?' : (char)('A'+drive);
			
			d = new FileAsync() {

				private String pwd;

				{
					this.command = FILE_PWD;
					super.level = 2;
				}
				
				@Override
				public void async() {
					File pwd = Posix.getCWD();
					if (pwd.exists() && pwd.isDirectory()) {
						this.pwd = pwd.getAbsolutePath();
						result_ok = true;
					} else {
						result_ok = false;
						posix_errno = Posix.ENOENT;
					}
					again = false;
				}

				@Override
				public void ready() throws Pausable {
					if (!result_ok) {
						reply_posix_error(posix_errno);
					} else {
						ByteBuffer reply = ByteBuffer.allocate(1+pwd.length());
						reply.put(FILE_RESP_OK);
						IO.putstr(reply, pwd, false);
						driver_output2(reply, null);
					}
				}

			};
			break;
		}

		case FILE_LSEEK: {
			final long off = buf.getLong();
			final int whence = buf.getInt();
			
			async_lseek(null, true, off, whence);
			
			return;
		}
		
		case FILE_OPEN: {
			final int mode = buf.getInt();
			final String file_name = IO.strcpy(buf);

			d = new SimpleFileAsync(cmd, file_name) {
				
				int res_fd = 1234;
				
				public void run() {
					boolean compressed = (mode & EFILE_COMPRESSED) > 0;
					if (compressed && ERT.DEBUG_EFILE) {
						System.err.println("EFile.open_compressed "+file_name);
					}
					boolean append = (mode & EFILE_MODE_APPEND) > 0;
					if ((mode & ~(EFILE_MODE_APPEND | EFILE_MODE_READ_WRITE)) > 0) {
						System.err.println("ONLY APPEND AND READ_WRITE OPTIONS ARE IMPLEMENTED!");
						throw new NotImplemented();
					}
					try {
						
						if (compressed) {
							if ((mode & EFILE_MODE_READ_WRITE) == EFILE_MODE_READ_WRITE && append) {
								posix_errno = Posix.EINVAL;
								return;
							}
							System.err.println("COMPRESSED NOT IMPLEMENTED!");
							throw new NotImplemented();
						} else {
							switch (mode & EFILE_MODE_READ_WRITE) {
							case EFILE_MODE_READ: {
								FileInputStream fo = new FileInputStream(file);
								fd = fo.getChannel();
								res_fd = getFDnumber(fo.getFD());
								break;
							}
							case EFILE_MODE_WRITE: {
								FileOutputStream fo = new FileOutputStream(file);
								fd = fo.getChannel();
								res_fd = getFDnumber(fo.getFD());
								break;
							}
							case EFILE_MODE_READ_WRITE: {
								RandomAccessFile rafff;
								fd = (rafff=new RandomAccessFile(file,"rw")).getChannel();
								res_fd = getFDnumber(rafff.getFD());
								break;
							}
							default:
								throw new NotImplemented();
							}//switch

							EFile.this.name = file;
							result_ok = true;
						}
						
					} catch (FileNotFoundException fnfe) {
						posix_errno = fileNotFound_to_posixErrno(file, mode);
					} catch (IOException e) {
						e.printStackTrace(System.err);
						posix_errno = fileNotFound_to_posixErrno(file, mode);						
					} catch (IllegalAccessException e) {
						e.printStackTrace(System.err);
						posix_errno = fileNotFound_to_posixErrno(file, mode);						
					}
				}

				@Override
				public void ready() throws Pausable {
					if (result_ok) {
						EFile.this.fd = fd;
						reply_Uint(res_fd); /* TODO: fd */
					} else {
						reply_posix_error(posix_errno);
					}
				}


			};
		} break;

		case FILE_FSTAT: 
		case FILE_LSTAT: {
			
			final String file_name = IO.strcpy(buf);
			final File file = new File(file_name);
			
			d = new FileAsync() {
				
				long file_size;
				int  file_type;
				
				long file_access_time;
				long file_modify_time;
				long file_create_time;
				
				int file_access;
				int file_mode;
				
				/** emulate fstat as close as possible */
				@Override
				public void async() {
					if (!file.exists()) {
						result_ok = false;
						posix_errno = Posix.ENOENT;
						return;
					}
					
					file_size = file.length();
					if (file.isDirectory()) {
						file_type = FT_DIRECTORY;
					} else if (file.isFile()) {
						file_type = FT_REGULAR;
					} else {
						file_type = FT_OTHER;
					}

					// this is as good as it gets...
					file_access_time = file_create_time = 
						file_modify_time = file.lastModified();

					file_mode   |= file.canExecute() ? 0000100 : 0;
					file_mode   |= file.canWrite() ? 0000200 : 0;
					file_mode   |= file.canRead() ? 0000400 : 0;
					
					file_access |= file.canRead() ? FA_READ : 0;
					file_access |= file.canWrite() ? FA_WRITE : 0;
					
					result_ok = true;
				}
				
				@Override
				public void ready() throws Pausable {
					if (!this.result_ok) {
						reply_posix_error(posix_errno);
						return;
					}
					
					final int RESULT_SIZE = (1 + (29 * 4));
					
					ByteBuffer res = ByteBuffer.allocate(RESULT_SIZE);
					res.order(ByteOrder.BIG_ENDIAN);
					
					res.put(FILE_RESP_INFO);
					res.putLong(file_size);
					res.putInt(file_type);
					
					put_time(res, file_access_time);
					put_time(res, file_modify_time);
					put_time(res, file_create_time);
					
					res.putInt(file_mode);
					res.putInt(1 /*file_links*/);
					res.putInt(0 /*file_major_device*/);
					res.putInt(0 /*file_minor_device*/);
					res.putInt(file_name.hashCode() /*file_inode*/);
					res.putInt(0 /*file_uid*/);
					res.putInt(0 /*file_gid*/);
					res.putInt(file_access);
					
					driver_output2(res, null);
				}

				private void put_time(ByteBuffer res, long time) {
					Calendar c = GregorianCalendar.getInstance();
					c.setTimeInMillis(time);
					
					int year = c.get(Calendar.YEAR);
					res.putInt(year);
					int month = c.get(Calendar.MONTH) - Calendar.JANUARY + 1;
					res.putInt(month);
					int day_of_month = c.get(Calendar.DAY_OF_MONTH);
					res.putInt(day_of_month);
					int hour_of_day = c.get(Calendar.HOUR_OF_DAY);
					res.putInt(hour_of_day);
					int minute_of_hour = c.get(Calendar.MINUTE);
					res.putInt(minute_of_hour);
					int seconds = c.get(Calendar.SECOND);
					res.putInt(seconds);
				}
			};
			
			break;
		}
		
		case FILE_READDIR: {
			
			final String dir_name = IO.strcpy(buf);
			//final File cwd = new File(System.getProperty("user.dir")).getAbsoluteFile();
			final File dir = new File(/*cwd, */dir_name);
			
			
			d = new FileAsync() {
				{
					super.level = 2;
				}
				
				String[] files;

				@Override
				public void async() {
					if (!dir.exists()) {
						this.posix_errno = Posix.ENOENT;
						this.result_ok = false;
						return;
					}
					if (!dir.isDirectory()) {
						this.posix_errno = Posix.EINVAL;
						this.result_ok = false;
						return;
					}
					
					try {
						files = dir.list(READDIR_FILTER);
						this.result_ok = true;
						
					} catch (SecurityException e) {
						this.posix_errno = Posix.EPERM;
						this.result_ok = false;
						return;
					}
				}

				@Override
				public void ready() throws Pausable {
					if (!this.result_ok) {
						reply_posix_error(posix_errno);
						return;
					}
										
					for (int i = 0; i < files.length; i++) {
						ByteBuffer resbuf = ByteBuffer.allocate(files[i].length()+1);
						resbuf.put(FILE_RESP_OK);
						resbuf.limit(resbuf.capacity());
						resbuf.position(1);
						
						IO.putstr(resbuf, files[i], false);
						
						driver_output2(resbuf, null);
					}
					
					ByteBuffer resbuf = ByteBuffer.allocate(1);
					resbuf.put(FILE_RESP_OK);
					driver_output2(resbuf, null);
					
				}
			};
			break;
		}
		
		case FILE_RENAME: {
			final String from_name = IO.getstr(buf, true);
			final File to_name   = new File(IO.getstr(buf, true));
			
			if (ERT.DEBUG_EFILE) 
				System.err.println(""+this+"rename "+from_name+" -> "+to_name);
			
			d = new SimpleFileAsync(cmd, from_name) {
				public void run() {
					this.result_ok = file.renameTo(to_name);
					if (!result_ok) {
						if (!file.exists()) {
							posix_errno = Posix.ENOENT;
						} else if (to_name.exists()) {
							posix_errno = Posix.EEXIST;
						} else {
							posix_errno = Posix.EUNKNOWN;
						}
					}
				}

			};
			break;
		}
		
		case FILE_SETOPT: {
			reply_ok();
			return;			
		}

		
		default:
			System.err.println ("file_output cmd:" + ((int) cmd) + " "
			+ EBinary.make(buf));
			
			throw new NotImplemented("file_output cmd:" + ((int) cmd) + " "
					+ EBinary.make(buf));
			/** ignore everything else - let the caller hang */
			// return;
		}

		if (d != null) {
			cq_enq(d);
		}

	}

	
	
	
	@Override
	public void processExit(ERef monitor) throws Pausable {
		// TODO Auto-generated method stub

	}

	@Override
	protected void readyAsync(EAsync data) throws Pausable {
		FileAsync d = (FileAsync) data;

		if (try_again(d))
			return;

		// do whatever for this kind of async job
		d.ready();

		if (write_buffered != 0 && timer_state == TimerState.IDLE) {
			timer_state = TimerState.WRITE;
			driver_set_timer(write_delay);
		}

		cq_execute();
	}

	/**
	 * @param d
	 * @return
	 */
	private boolean try_again(FileAsync d) {
		if (!d.again) {
			return false;
		}

		d.deq_free_size();

		if (timer_state != TimerState.IDLE) {
			driver_cancel_timer();
		}

		timer_state = TimerState.AGAIN;
		invoke = d;
		driver_set_timer(0);

		return true;
	}

	@Override
	protected void readyInput(SelectableChannel ch) throws Pausable {
		throw new InternalError("should not happen");
	}

	@Override
	protected void readyOutput(SelectableChannel evt) throws Pausable {
		throw new InternalError("should not happen");
	}

	@Override
	protected void timeout() throws Pausable {
		TimerState timer_state = this.timer_state;
		this.timer_state = TimerState.IDLE;
		switch (timer_state) {
		case IDLE:
			assert (false) : "timeout in idle state?";
			return;

		case AGAIN:
			assert (invoke != null);
			driver_async(invoke);
			break;

		case WRITE:
			int r = flush_write(null);
			assert (r == 0);
			cq_execute();
		}
	}

	/**
	 * @return
	 */
	private int flush_write(int[] errp) {
		int result;
		q_mtx.lock();
		try {
			if (this.write_buffered > 0) {
				result = async_write(null, false, 0);
			} else {
				result = 0;
			}
		} finally {
			q_mtx.unlock();
		}
		return result;
	}

	// // CQ OPERATIONS //

	/**
	 * @param errp
	 * @param reply
	 *            true if we should send a reply
	 * @param reply_size
	 *            value to send in reply
	 * @return
	 */
	private int async_write(int[] errp, boolean reply, int reply_size) {
		try {
			FileAsync cmd = new WriteAsync(reply, reply_size);
			cq_enq(cmd);
			write_buffered = 0;
			return 0;
		} catch (OutOfMemoryError e) {
			if (errp == null) {
				throw e;
			}
			if (errp != null)
				errp[0] = Posix.ENOMEM;
			return -1;
		}
	}

	private void cq_enq(FileAsync d) {
		cq.add(d);
	}

	private FileAsync cq_deq() {
		return cq.poll();
	}

	private void cq_execute() throws Pausable {
		if (timer_state == TimerState.AGAIN)
			return;

		FileAsync d;
		if ((d = cq_deq()) == null)
			return;

		d.again = false; /* (SYS_INFO.async_threads == 0); */

		if (THREAD_SHORT_CIRCUIT >= level) {
			d.async();
			this.readyAsync(d);
		} else {
			driver_async(d);
		}
	}

	//

	void reply_buf(ByteBuffer buf) throws Pausable {
		ByteBuffer header = ByteBuffer.allocate(1 + 4 + 4);
		header.put(FILE_RESP_DATA);
		header.putLong(buf.position());
		driver_output2(header, buf);
	}

	void reply_eof() throws Pausable {
		ByteBuffer header = ByteBuffer.allocate(1);
		header.put(FILE_RESP_EOF);
		driver_output2(header, null);
	}

	/**
	 * @throws Pausable 
	 * 
	 */
	public void reply_ok() throws Pausable {
		ByteBuffer header = ByteBuffer.allocate(1);
		header.put(FILE_RESP_OK);
		driver_output2(header, null);
	}

	protected static int fileNotFound_to_posixErrno(File file, int mode) {
		if (!file.exists() || !file.isFile())
			return Posix.ENOENT;
		else if ((mode & EFILE_MODE_READ) > 0 && !file.canRead())
			return Posix.EPERM;
		else if ((mode & EFILE_MODE_WRITE) > 0 && !file.canWrite())
			return Posix.EPERM;
		else
			return Posix.EUNKNOWN;
	}

	@Override
	public String toString() {
		String pos;
		try {
			pos = (fd==null?"?":"0x"+Long.toHexString (fd.position()));
		} catch (IOException e) {
			pos = "?";
		}
		return "EFile[name=\""+name+"\";pos="+pos+"]";
		
	}
	
}
