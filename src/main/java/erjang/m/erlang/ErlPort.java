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

package erjang.m.erlang;

import erjang.BIF;
import erjang.EAtom;
import erjang.EExecDriverTask;
import erjang.EObject;
import erjang.EPort;
import erjang.EProc;
import erjang.ERT;
import erjang.ETask;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ErlangError;
import erjang.NotImplemented;

/**
 * 
 */
public class ErlPort {

	static EAtom am_spawn = EAtom.intern("spawn");
	static EAtom am_spawn_driver = EAtom.intern("spawn_driver");
	static EAtom am_spawn_executable = EAtom.intern("spawn_executable");
	
	@BIF
	static EPort open_port(EProc proc, EObject portName, EObject portSetting) {
		
		
		ETuple t;
		if ((t = portName.testTuple()) == null)
			throw ERT.badarg(portName, portSetting);

		ETuple2 name;
		if ((name = ETuple2.cast(t)) == null)
			throw ERT.badarg(portName, portSetting);

		if (name.elem1 == am_spawn) {
			throw new ErlangError(ERT.AM_NOT_IMPLEMENTED, portName, portSetting);
			
		} else if (name.elem1 == am_spawn_driver) {
			throw new NotImplemented();
			
		} else if (name.elem1 == am_spawn_executable) {
			ETask<? extends EPort> task = new EExecDriverTask(proc, name, portSetting);
			
			return task.self();
		}
			
		throw ERT.badarg(portName, portSetting);
	}

}
