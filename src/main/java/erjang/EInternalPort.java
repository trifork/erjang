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
	public boolean link_oneway(EHandle other) throws Pausable {
		return task.link_oneway(other);
	}
	
	@Override
	public void unlink_oneway(EHandle other) throws Pausable {
		task.unlink_oneway(other);
	}
	
	public boolean add_monitor(EHandle target, ERef ref) throws Pausable {
		// TODO: check if task is alive!
		return task.add_monitor(target, ref);
	}

	@Override
	public void remove_monitor(EHandle sender, ERef r, boolean flush) throws Pausable {
		task.remove_monitor(r, flush);
	}

	@Override
	public void send_monitor_exit(EHandle from, ERef ref, EObject reason)
			throws Pausable {
		EDriverTask task = this.task;
		if (task != null) {
			task.send_monitor_exit(from, ref, reason);
		}
		
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
	 * @throws Pausable 
	 */
	public EObject control(EProc caller, int op, ByteBuffer cmd) throws Pausable {
		return task.control(caller, op, cmd);
	}

	/**
	 * @param value
	 * @param data
	 * @return
	 * @throws Pausable 
	 */
	public EObject call(EProc caller, int op, EObject data) throws Pausable {
		return task.call(caller, op, data);
	}

	/**
	 * @param caller TODO
	 * @param out
	 * @return
	 * @throws Pausable 
	 */
	public void command(EHandle caller, ByteBuffer[] out) throws Pausable {
		task.command(caller, out);
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

	public void set_owner(EInternalPID ipid) {
		EDriverTask dt = task;
		if (dt != null)
			dt.owner(ipid);		
	}

	@Override
	public void close() throws Pausable {
		EDriverTask dt = task;
		if (dt != null)
			dt.close();		
	}
}
