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
import java.nio.ByteBuffer;
import java.util.Map;

import kilim.Pausable;

import erjang.EAtom;
import erjang.EBitString;
import erjang.ECons;
import erjang.EHandle;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.EString;
import erjang.ETuple2;
import erjang.ErlangError;
import erjang.ErlangExit;
import erjang.NotImplemented;


/**
 * UNUSED CLASS I KEEP AROUND BECAUSE IT HAS SOME CODE I MAY USE LATER
 * 
 * A n EPort instance that corresponds to an external executable
 */
public class EExecDriverTask extends EDriverTask {

	private Process process;
	
	private DataOutputStream out;
	private DataInputStream in;

	private DataInputStream err;
	
	/**
	 * @param owner
	 * @param name
	 * @param portSetting
	 */
	public EExecDriverTask(EProc owner, ETuple2 name, EObject portSetting) {
			super(owner, null);

		// argument can be any list, ... turn it into a string
		ECons cons;
		if ((cons = name.elem2.testCons()) == null)
			throw ERT.badarg();
		EString es = EString.make(cons);

		parseOptions(es, portSetting);
		

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
		this.err = new DataInputStream(this.process.getErrorStream());
	}

	public void send(EObject val) {
		
		// first, grok argument
		ByteBuffer buf;
		
		ECons cons;
		EBitString bin;
		if ((cons=val.testCons()) != null) {
			EString str = EString.make(cons);
			// buf = str.collectIOList(null);
			
		} else if ((bin=val.testBinString()) != null) {
			if (!bin.isBinary()) throw ERT.badarg(val);
			// buf = bin.collectIOList(null);
		} else {
			throw ERT.badarg(val);
		}
		
		//try {
			//internal_output(buf);
		//} catch (IOException e) {
		//	throw new ErlangExit(e);
		//}
		
	}
	
	synchronized void internal_output(ByteBuffer buf) throws IOException {

		byte[] data = buf.array();
		int data_off = buf.position();
		int data_len = buf.remaining();
		
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
