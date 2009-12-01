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

import static erjang.EPort.am_arg0;
import static erjang.EPort.am_args;
import static erjang.EPort.am_binary;
import static erjang.EPort.am_cd;
import static erjang.EPort.am_env;
import static erjang.EPort.am_eof;
import static erjang.EPort.am_exit_status;
import static erjang.EPort.am_hide;
import static erjang.EPort.am_in;
import static erjang.EPort.am_line;
import static erjang.EPort.am_nouse_stdio;
import static erjang.EPort.am_out;
import static erjang.EPort.am_packet;
import static erjang.EPort.am_stream;
import static erjang.EPort.am_use_stdio;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;


/**
 * A n EPort instance that corresponds to an external executable
 */
public class EExecDriverTask extends EDriverTask {

	static enum Mode {
		STREAM, PACKET, LINE
	};

	private boolean send_binary_data;
	private boolean is_out_only;
	private boolean is_in_only;
	private boolean send_eof;
	private boolean hide;
	private int port_out_fd;
	private int port_in_fd;
	private boolean send_exit_status;
	private int packet;
	private int line_length;

	private Mode mode = Mode.STREAM;
	private Process process;
	private DataOutputStream out;
	private DataInputStream in;

	/**
	 * @param owner
	 * @param name
	 * @param portSetting
	 */
	public EExecDriverTask(EProc owner, ETuple2 name, EObject portSetting) {
		super(owner);

		// argument can be any list, ... turn it into a string
		ECons cons;
		if ((cons = name.elem2.testCons()) == null)
			throw ERT.badarg();
		EString es = EString.make(cons);

		// set by options
		String[] cmd = new String[] { es.stringValue() };
		this.packet = -1; // not set
		this.line_length = -1;

		String cwd = System.getProperty("user.dir");

		Map<String, String> env = new HashMap<String, String>(System.getenv());

		this.send_exit_status = false;
		this.port_in_fd = 1;
		this.port_out_fd = 2;

		this.hide = false;
		this.send_eof = false;

		this.is_in_only = false;
		this.is_out_only = false;
		this.send_binary_data = false;

		ECons settings = portSetting.testCons();
		if (settings == null)
			throw ERT.badarg();

		for (; settings != null && !settings.isNil(); settings = settings
				.tail().testCons()) {

			EObject val = settings.head();
			ETuple tup;
			if ((tup = val.testTuple()) != null) {
				ETuple2 tup2;
				if ((tup2 = ETuple2.cast(tup)) != null) {

					if (tup2.elem1 == am_args) {
						ESeq list = tup2.elem2.testWellformedList();
						EObject[] nargs = list.toArray();

						String[] new_cmd = new String[nargs.length + 1];
						new_cmd[0] = cmd[0];
						for (int i = 0; i < nargs.length; i++) {
							new_cmd[i + 1] = EString.make(nargs[i])
									.stringValue();
						}
						cmd = new_cmd;

					} else if (tup2.elem1 == am_arg0) {
						String[] new_cmd = new String[2];
						new_cmd[0] = cmd[0];
						new_cmd[1] = EString.make(tup2.elem2).stringValue();

					} else if (tup2.elem1 == am_packet) {
						packet = tup2.elem2.asInt();
						mode = Mode.PACKET;

					} else if (tup2.elem1 == am_cd) {
						cwd = EString.make(tup2.elem2).stringValue();

					} else if (tup2.elem1 == am_env) {

						ESeq ee;
						if ((ee = tup2.elem2.testWellformedList()) == null) {
							throw ERT.badarg();
						}

						EObject[] envs = ee.toArray();
						for (int i = 0; i < envs.length; i++) {
							ETuple2 e = ETuple2.cast(envs[i].testTuple());
							if (e.elem2 == ERT.FALSE) {
								env.remove(EString.make(e.elem1).stringValue());
							} else {
								env.put(EString.make(e.elem1).stringValue(),
										EString.make(e.elem2).stringValue());
							}
						}

					} else if (tup2.elem1 == am_line) {
						line_length = tup2.elem2.asInt();
						mode = Mode.LINE;

					} else {

						throw ERT.badarg();
					}

				}
			} else if (val == am_stream) {
				mode = Mode.STREAM;

			} else if (val == am_use_stdio) {
				port_in_fd = 1;
				port_out_fd = 2;

			} else if (val == am_nouse_stdio) {
				port_in_fd = 3;
				port_out_fd = 4;

			} else if (val == am_hide) {
				hide = true;

			} else if (val == am_exit_status) {
				send_exit_status = true;

			} else if (val == am_eof) {
				send_eof = true;

			} else if (val == am_in) {
				is_in_only = true;

			} else if (val == am_out) {
				is_out_only = true;

			} else if (val == am_binary) {
				send_binary_data = true;

			}
		}

		String[] envp = new String[env.size()];
		int pos = 0;
		for (Map.Entry<String, String> e : env.entrySet()) {
			envp[pos++] = e.getKey() + "=" + e.getValue();
		}

		File file = new File(cmd[0]);
		if (!file.exists()) {
			throw new ErlangError(EAtom.intern("enoent"));
		}

		if (!file.canExecute()) {
			throw new ErlangError(EAtom.intern("eaccess"));
		}

		try {
			this.process = Runtime.getRuntime().exec(cmd, envp, new File(cwd));
		} catch (IOException e1) {
			throw new ErlangError(e1);
		}

		this.out = new DataOutputStream(this.process.getOutputStream());
		this.in = new DataInputStream(this.process.getInputStream());
	}

	public void send(EObject val) {
		
		// first, grok argument
		byte[] data;
		int data_off;
		int data_len;
		
		ECons cons;
		EBitString bin;
		if ((cons=val.testCons()) != null) {
			EString str = EString.make(cons);
			data = str.data;
			data_off = str.off;
			data_len = data.length-data_off;
			
		} else if ((bin=val.testBinString()) != null) {
			if (!bin.isBinary()) throw ERT.badarg(val);
			
			if ((bin.bitOff % 8) == 0) {
				data = bin.data;
				data_off = bin.bitOff/8;
				data_len = bin.bits/8;
			} else {
				data = bin.toByteArray();
				data_off = 0;
				data_len = data.length;
				
			}
		} else {
			throw ERT.badarg(val);
		}
		
		try {
			internal_send(data, data_off, data_len);
		} catch (IOException e) {
			throw new ErlangExit(e);
		}
		
	}
	
	public synchronized void internal_send(byte[] data, int data_off, int data_len) throws IOException {
		
		switch (mode) {
		case STREAM:
			this.out.write(data, data_off, data_len);
			return;
			
		case PACKET: 
			switch (packet) {
			case 1:
				for (int off = 0; off < data_len; off += 256) {
					int rest = Math.min(256, data_len - off);
					out.writeByte(rest);
					out.write(data, data_off+off, rest);
				}
				return;

			case 2:
				for (int off = 0; off < data_len; off += (1 << 16)) {
					int rest = Math.min((1 << 16), data_len - off);
					out.writeShort(rest);
					out.write(data, data_off+off, rest);
				}
				return;

			case 4:
				out.writeInt(data_len);
				out.write(data, data_off, data_len);
				return;

			default:
				throw new Error("should not happen");
			}
		
		case LINE:
			throw new NotImplemented();
			
			
		}
	}
}
