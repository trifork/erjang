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

import erjang.ETask.State;
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
	
	private final EDriverTask task;

	public EInternalPort(EDriverTask task) {
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

	/* (non-Javadoc)
	 * @see erjang.EHandle#self()
	 */
	@Override
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
	 * @param op
	 * @param out
	 * @return
	 */
	public EObject control(int op, ByteBuffer[] out) {
		return task.control(op, out);
	}

	/**
	 * @param value
	 * @param data
	 * @return
	 */
	public EObject call(int op, EObject data) {
		return task.call(op, data);
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
}
