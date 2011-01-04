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
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import kilim.Pausable;
import kilim.Task;

import erjang.EAtom;
import erjang.EBinList;
import erjang.EBinary;
import erjang.EBitString;
import erjang.ECons;
import erjang.EHandle;
import erjang.EObject;
import erjang.EPort;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.EString;
import erjang.ETask;
import erjang.ETuple2;
import erjang.ErlangError;
import erjang.NotImplemented;
import erjang.m.erlang.ErlPort;


/**
 * UNUSED CLASS I KEEP AROUND BECAUSE IT HAS SOME CODE I MAY USE LATER
 * 
 * A n EPort instance that corresponds to an external executable
 */
public class EExecDriverTask extends EDriverTask {

	private Process process;
	
	private DataOutputStream out;
	private InputStream in;

	private DataInputStream err;

	private ETuple2 name;

	private EObject command;
	
	public EObject getName() {
		return command;
	}
	
	/**
	 * @param owner
	 * @param name
	 * @param portSetting
	 */
	public EExecDriverTask(EProc owner, ETuple2 name, EObject command, EObject portSetting) {
			super(owner.self_handle(), new ExecDriverInstance(name));
			
			this.command = command;
			
			// argument can be any list, ... turn it into a string
			ESeq es = name.elem2.testString();
			if (es == null) {		
				ECons cons;
				EAtom am;
				if ((cons = name.elem2.testCons()) != null) {
					es = EString.make(cons);
				} else if ((am = name.elem2.testAtom()) != null) {
					es = EString.fromString(am.getName());
				} else {
					throw ERT.badarg(name, portSetting);
				}
			}
			
			String full_cmd = es.stringValue();
			
			String[] cmd;
			if (name.elem1 == ErlPort.am_spawn) {
				cmd = full_cmd.split(" +");
			} else {
				cmd = new String[] { full_cmd };
			}
			
			parseOptions(cmd, portSetting);

			File f = new File(cmd[0]);
			if (f.exists() && f.canExecute()) {
				// ok !
			} else if (f.isAbsolute()) {
				throw new ErlangError(EAtom.intern("enoent"));
			} else {

				String cmd_path = env.get("PATH");
                if (cmd_path != null) {
                    for (String elm : cmd_path.split(File.pathSeparator)) {
                        File dir = new File(elm);
                        f = new File(dir, cmd[0]);
                        if (f.exists()) {
                            cmd[0] = f.getAbsolutePath();
                            break;
                        } else {
                            f = null;
                        }
                    }
                }
			}

			// System.err.println("lauching "+cmd[0]+" "+(f==null?"(not found)":""));		
			
			if (f == null) {
				throw new ErlangError(EAtom.intern("enoent"));
			}

			if (!f.canExecute()) {
				throw new ErlangError(EAtom.intern("eaccess"));
			}


			super.setupInstance();
			
	}

	/* (non-Javadoc)
	 * @see kilim.Task#toString()
	 */
	@Override
	public String toString() {
		return String.valueOf(name) + "::" + super.toString();
	}

}
