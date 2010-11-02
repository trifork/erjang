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

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import kilim.Pausable;
import kilim.Task;

import erjang.EAtom;
import erjang.EBinary;
import erjang.ECons;
import erjang.EHandle;
import erjang.EObject;
import erjang.EPID;
import erjang.ERT;
import erjang.ERef;
import erjang.ESeq;
import erjang.EString;
import erjang.ETuple2;
import erjang.ErlangError;
import erjang.NotImplemented;
import erjang.driver.EDriverTask.Mode;
import erjang.m.erlang.ErlPort;

/**
 * 
 */
public class ExecDriverInstance extends EDriverInstance {

	private ETuple2 name;
	private Process process;
	private DataOutputStream out;
	private DataInputStream in;
	private DataInputStream err;
	protected Thread stdin_thread;
	protected Thread stderr_thread;
	private boolean is_closing;

	/**
	 * @param name
	 */
	public ExecDriverInstance(ETuple2 name) {
		super(new ExecDriver(name));
		this.name = name;

	}

	@Override
	public void setup() {

		HashMap<String, String> env = task.env;
		String[] cmd = task.cmd;
		String cwd = task.cwd;

		String[] envp = new String[env.size()];
		int pos = 0;
		for (Map.Entry<String, String> e : env.entrySet()) {
			envp[pos++] = e.getKey() + "=" + e.getValue();
		}

		try {
			this.process = Runtime.getRuntime().exec(cmd, envp, new File(cwd));
		} catch (IOException e1) {
			throw new ErlangError(e1);
		}

		List<String> al = new ArrayList<String>();
		Collections.addAll(al, cmd);

		// System.err.println("EXEC " + al);

		this.out = new DataOutputStream(this.process.getOutputStream());
		this.in = new DataInputStream(this.process.getInputStream());
		this.err = new DataInputStream(this.process.getErrorStream());

		if (!task.is_out_only) {

			start_input_reader(in, true);

			// also read stderr
			if (task.stderr_to_stdout) {
				start_input_reader(err, false);
			}

		}

	}
	
	synchronized void do_close() {
		
		//System.err.println("closing: "+name);

		this.is_closing = true;
		
		Thread th = stderr_thread;
		if (th != null) {
			th.interrupt();
		}
		
		th = stdin_thread;
		if (th != null) {
			th.interrupt();
		}
		
		Process p = this.process;
		if (p != null) {
			p.destroy();
		}
		
	}
	
	synchronized boolean is_closing() {
		return this.is_closing;
	}

	private void start_input_reader(final DataInputStream stream,
			final boolean is_stdin) {
		new Thread() {
			
			{ setDaemon(true); start(); }
			
			public void run() {

				
				if (is_stdin) {
					stdin_thread = Thread.currentThread();

					try {
						
						boolean cont = false;
						do {
							try {
								cont = do_read();
							} catch (InterruptedIOException e) {
								cont = !is_closing();
							} catch (IOException e) {
								cont = false;
							}
						} while (cont);

					} finally {
						stdin_thread = null;
						
						if (task.send_eof) {
							task.eof_from_driver_b();
						}
						
						if (task.send_exit_status) {
							int code;
							while (true) {
								try {
									code = process.waitFor();
									break;
								} catch (InterruptedException e) {
									continue;
								}
							}
							task.exit_status_from_driver_b(code);
						}
					}

				} else {
					stderr_thread = Thread.currentThread();

					try {
						
						boolean cont = false;
						do {
							try {
								cont = do_read();
							} catch (InterruptedIOException e) {
								cont = !is_closing();
							} catch (IOException e) {
								cont = false;
							}
						} while (cont);

					} finally {
						stderr_thread = null;
					}

				}
			}

			private boolean do_read() throws IOException {

				byte[] data;

				int nbytes;

				if (task.mode == Mode.STREAM) {
					data = new byte[Math.max(stream.available(), 512)];
					try {
						nbytes = stream.read(data);
					} catch (IOException e) {
						nbytes = 0;
					}

				} else if (task.mode == Mode.PACKET) {

					switch (task.packet) {
					case 1:
						nbytes = stream.read();
						break;
					case 2:
						nbytes = stream.readUnsignedShort();
						break;
					case 4:
						nbytes = stream.readInt() & 0x7fffffff;
						break;
					default:
						throw new InternalError();
					}

					if (nbytes <= 0) {
						return false;
					}

					data = new byte[nbytes];

					try {
						stream.readFully(data);
					} catch (IOException e) {
						nbytes = 0;
					}

				} else {
					throw new NotImplemented("mode=" + task.mode);
				}

				if (nbytes <= 0) {
					return false;
				}

				if (task.send_binary_data) {
					task.output_from_driver_b(new EBinary(data, 0, nbytes));
				} else {
					task.output_from_driver_b(EString.make(data, 0, nbytes));
				}

				return true;
			}

		};
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.driver.EDriverInstance#call(int, erjang.EObject)
	 */
	@Override
	protected EObject call(EPID pid, int command, EObject data) throws Pausable {
		throw ERT.badarg(ERT.box(command), data);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.driver.EDriverInstance#flush()
	 */
	@Override
	protected void flush() throws Pausable {
		// TODO Auto-generated method stub

	}

	@Override
	protected void stop(EObject reason) throws Pausable {

		this.do_close();

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.driver.EDriverInstance#output(java.nio.ByteBuffer)
	 */
	@Override
	protected void output(EHandle caller, ByteBuffer buf) throws IOException,
			Pausable {

		byte[] data = buf.array();
		int data_off = buf.position() + buf.arrayOffset();
		int data_len = buf.remaining();

		//System.err.println("out: " + data_len + "; "
		//		+ EString.make(data, data_off, data_len));

		switch (task.mode) {
		case STREAM:
			this.out.write(data, data_off, data_len);
			this.out.flush();
			return;

		case PACKET:
			switch (task.packet) {
			case 1:
				for (int off = 0; off < data_len; off += 256) {
					int rest = Math.min(256, data_len - off);
					out.writeByte(rest);
					out.write(data, data_off + off, rest);
				}
				this.out.flush();
				return;

			case 2:
				for (int off = 0; off < data_len; off += (1 << 16)) {
					int rest = Math.min((1 << 16), data_len - off);
					out.writeShort(rest);
					out.write(data, data_off + off, rest);
				}
				this.out.flush();
				return;

			case 4:
				out.writeInt(data_len);
				out.write(data, data_off, data_len);
				this.out.flush();
				return;

			default:
				throw new Error("should not happen");
			}

		case LINE:
			throw new NotImplemented();

		}

		this.out.flush();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.driver.EDriverInstance#processExit(erjang.ERef)
	 */
	@Override
	public void processExit(ERef monitor) throws Pausable {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.driver.EDriverInstance#readyAsync(erjang.driver.EAsync)
	 */
	@Override
	protected void readyAsync(EAsync data) throws Pausable {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * erjang.driver.EDriverInstance#readyInput(java.nio.channels.SelectableChannel
	 * )
	 */
	@Override
	protected void readyInput(SelectableChannel ch) throws Pausable {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * erjang.driver.EDriverInstance#readyOutput(java.nio.channels.SelectableChannel
	 * )
	 */
	@Override
	protected void readyOutput(SelectableChannel evt) throws Pausable {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.driver.EDriverInstance#timeout()
	 */
	@Override
	protected void timeout() throws Pausable {
		// TODO Auto-generated method stub

	}

}
