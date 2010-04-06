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

import java.nio.ByteBuffer;

import kilim.Pausable;
import erjang.driver.EDriverTask;

/**
 * 
 */
public class EInternalPort extends EPort implements ELocalHandle {

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "<port:" + task.id + ">";
	}
	
	// TODO: private final WeakReference<EDriverTask> task;
	private final EDriverTask task;
	
	public EInternalPort(EDriverTask task) {
		super(ERT.getLocalNode());
		this.task = task;
	}
	
	@Override
	public EInternalPort testInternalPort() {
		return this;
	}
	
	public ELocalHandle testLocalHandle() {
		return this;
	}

	@Override
	public boolean exists() {
		return task.exists();
	}
	


	/* (non-Javadoc)
	 * @see erjang.EHandle#link_oneway(erjang.EHandle)
	 */
	@Override
	public void link_oneway(EHandle other) throws Pausable {
		task.link_oneway(other);
	}
	
	public ERef add_monitor(EHandle target, EObject object) {
		// TODO: check if task is alive!
		return task.add_monitor(target, object);
	}

	@Override
	public void remove_monitor(ERef r, boolean flush) {
		task.remove_monitor(r, flush);
	}



	/* (non-Javadoc)
	 * @see erjang.EHandle#self()
	 */
	@Override
	public
	EDriverTask task() {
		return task;
	}


	/**
	 * @return
	 */
	public int internal_port_number() {
		throw new NotImplemented();
	}

	/**
	 * @param caller 
	 * @param op
	 * @param cmd
	 * @return
	 */
	public EObject control(EProc caller, int op, ByteBuffer cmd) {
		return task.control(caller, op, cmd);
	}

	/**
	 * @param value
	 * @param data
	 * @return
	 */
	public EObject call(EProc caller, int op, EObject data) {
		return task.call(caller, op, data);
	}

	/**
	 * @param out
	 * @return
	 * @throws Pausable 
	 */
	public void command(ByteBuffer[] out) throws Pausable {
		task.command(out);
	}

	/* (non-Javadoc)
	 * @see erjang.EPort#isOpen()
	 */
	@Override
	public boolean isOpen() {
		// TODO: this can be wrong in a race condition, but nothing to do about it.
		return !task.isDone();
	}
	
	@Override
	public EObject port_info(EAtom spec) {
		if (task.isDone()) return ERT.am_undefined;
		return task.port_info(spec);
	}

	@Override
	public EObject get_data() {
		EDriverTask dt = task;
		if (dt == null)
			return ERT.am_undefined;
		return dt.port_data;
	}

	@Override
	public void set_data(EObject data) {
		EDriverTask dt = task;
		if (dt != null)
			dt.port_data = data;		
	}
}
