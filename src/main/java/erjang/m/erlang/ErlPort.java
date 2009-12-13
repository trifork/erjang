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

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

import kilim.Pausable;

import erjang.BIF;
import erjang.EAtom;
import erjang.EInternalPort;
import erjang.EObject;
import erjang.EPort;
import erjang.EProc;
import erjang.ERT;
import erjang.ESmall;
import erjang.EString;
import erjang.ETask;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.NotImplemented;
import erjang.driver.EDriver;
import erjang.driver.EExecDriverTask;
import erjang.driver.ESpawnDriverTask;

/**
 * 
 */
public class ErlPort {

	static EAtom am_spawn = EAtom.intern("spawn");
	static EAtom am_spawn_driver = EAtom.intern("spawn_driver");
	static EAtom am_spawn_executable = EAtom.intern("spawn_executable");
	
	@BIF
	static EObject port_command(EProc proc, EObject port, EObject data) throws Pausable
	{
		EInternalPort p = port.testInternalPort();
		
		if (p == null) {
			port = ERT.whereis(port);
			if (port == ERT.am_undefined)
				port = null;
			
			p = port.testInternalPort();
		}

		List<ByteBuffer> ovec = new ArrayList<ByteBuffer>();
		if (p == null || !data.collectIOList(ovec)) { throw ERT.badarg(port, data); }
		
		ByteBuffer[] out = new ByteBuffer[ovec.size()];
		ovec.toArray(out);

		// System.err.println("packing "+data+"::"+data.getClass().getName()+" -> "+ovec);
		
		p.command(out);
		
		return ERT.TRUE;
	}
	
	@BIF
	static EObject port_control(EProc proc, EObject port, EObject operation, EObject data)
	{
		EInternalPort p = port.testInternalPort();
		
		if (p == null) {
			port = ERT.whereis(port);
			if (port == ERT.am_undefined)
				port = null;
			
			p = port.testInternalPort();
		}

		ESmall op = operation.testSmall();
		
		List<ByteBuffer> ovec = new ArrayList<ByteBuffer>();
		if (p == null || op == null || !data.collectIOList(ovec)) { throw ERT.badarg(port, operation, data); }
		
		ByteBuffer[] out = new ByteBuffer[ovec.size()];
		ovec.toArray(out);

		// TODO: improve exception handling/wrapping here so we get ErlangException types only!
		return p.control(op.value, out);
	}
	
	@BIF
	static EObject port_call(EProc proc, EObject port, EObject operation, EObject data)
	{
		EInternalPort p = port.testInternalPort();
		
		if (p == null) {
			port = ERT.whereis(port);
			if (port == ERT.am_undefined)
				port = null;
			
			p = port.testInternalPort();
		}

		ESmall op = operation.testSmall();
		
		if (p == null || op == null) { throw ERT.badarg(port, operation, data); }
		
		// TODO: improve exception handling/wrapping here so we get ErlangException types only!
		return p.call(op.value, data);
	}
	
	@BIF
	static EPort open_port(EProc proc, EObject portName, EObject portSetting) throws Pausable {
		
		
		ETuple t;
		if ((t = portName.testTuple()) == null)
			throw ERT.badarg(portName, portSetting);

		ETuple2 name;
		if ((name = ETuple2.cast(t)) == null)
			throw ERT.badarg(portName, portSetting);

		EString command = EString.make(name.elem2);
		
		
		ETask<? extends EPort> task = null;
		
		if (name.elem1 == am_spawn) {
			EDriver drv = ERT.find_driver(command);

			if (drv == null) {
				task = new EExecDriverTask(proc, name, portSetting);
			} else {
				task = new ESpawnDriverTask(proc, drv, command, portSetting);
			}

		} else if (name.elem1 == am_spawn_driver) {
			EDriver drv = ERT.find_driver(command);
			if (drv == null) {
				throw ERT.badarg(portName, portSetting);
			}
			task = new ESpawnDriverTask(proc, drv, command, portSetting);
			
			
		} else if (name.elem1 == am_spawn_executable) {
			task = new EExecDriverTask(proc, name, portSetting);
		}
			
		if (task != null) {
			// link this proc and the driver task
			//task.link_to(proc);
			ERT.run(task);
			
			return task.self_handle();
		}
		
		throw ERT.badarg(portName, portSetting);
	}

	@BIF
	static public EObject port_close(EProc proc, EObject port) throws Pausable
	{
		EPort p;
		if ((p=port.testPort())==null) {
			
			EObject obj = ERT.whereis(port);
			
			if (obj == ERT.am_undefined || ((p=obj.testPort())== null)) {
				throw ERT.badarg(port);
			}
		}
		
		if (!p.isOpen()) {
			throw ERT.badarg(port);
		}
		
		
		proc.unlink(p);
		
		p.send(new ETuple2(proc.self_handle(), EPort.am_close));
		
		return ERT.TRUE;
	}
	
}
