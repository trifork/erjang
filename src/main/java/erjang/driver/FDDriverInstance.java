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

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SelectableChannel;

import erjang.EBinary;
import erjang.EObject;
import erjang.EPID;
import erjang.ERT;
import erjang.ERef;
import erjang.EString;
import erjang.NotImplemented;

/**
 * 
 */
public class FDDriverInstance extends EDriverInstance {

	public static final int CTRL_OP_GET_WINSIZE = 100;
	
	private final int in;
	private final int out;

	InputStream ins;
	OutputStream outs;
	
	/**
	 * @param in
	 * @param out
	 */
	public FDDriverInstance(int in, int out) {
		this.in = in;
		this.out = out;
		
		if (in == 0) {
			ins = System.in;
		} 
		
		if (out == 1) {
			outs = System.out;
		}
		
		if (out == 2) {
			outs = System.err;
		}
		// that's it.
	}

	public void setup() {
		if (ins != null) {
			new Thread() {
				{ setDaemon(true); start(); }
				
				@Override
				public void run() {
					byte[] buffer = new byte[1024];
					while (true) {
						try {
							int howmany = ins.read(buffer);
							
							if (howmany < 0) {
								if (task.send_eof) {
									task.eof_from_driver();
									return;
								}
							}
							
							byte[] ob = new byte[howmany];
							System.arraycopy(buffer, 0, ob, 0, howmany);
							
							if (task.send_binary_data) {
								EBinary out = new EBinary(ob);
								task.output_from_driver(out);
							} else {
								EString str = EString.make(ob);
								task.output_from_driver(str);
							}

						} catch (EOFException ex) {
							if (task.send_eof) {
								task.eof_from_driver();
							}
							
						} catch (IOException e) {
							
							if (e.getMessage().equals("Interrupted system call")) {
								// ignore //
							} else {
								// TODO: we should really just ignore this; maybe log?
								e.printStackTrace();
							}
						}
						
					}
				}
			};
		}
	}

	@Override
	protected ByteBuffer control(EPID caller, int command, ByteBuffer out) {
		if (command == CTRL_OP_GET_WINSIZE) {
			// TODO: hmm, how do we do that?
			// for now, we always respond 80x25.
			ByteBuffer bb = ByteBuffer.allocate(8);
			bb.order(ByteOrder.nativeOrder());
			bb.putInt(80);
			bb.putInt(25);
			return bb;
		} else {
			throw ERT.badarg();
		}
	}
	
	@Override
	protected EObject call(EPID caller, int command, EObject data) {
		// allways return null
		return null;
	}


	@Override
	protected void flush() {
	}

	@Override
	protected void output(ByteBuffer data) throws IOException {
		if (outs != null) {
			if (data.hasArray()) {
				outs.write(data.array(), data.arrayOffset(), data.remaining());
			} else {
				throw new NotImplemented();
			}
		}
	}

	@Override
	protected void processExit(ERef monitor) {
		// TODO Auto-generated method stub

	}

	@Override
	protected void readyAsync(EAsync data) {
		// only called if we issue async commands
	}

	@Override
	protected void readyInput(SelectableChannel ch) {

	}

	@Override
	protected void readyOutput(SelectableChannel evt) {

	}

	@Override
	protected void timeout() {

	}

}
