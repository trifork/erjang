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

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;
import java.util.concurrent.locks.ReentrantLock;

import kilim.Lock;
import kilim.Pausable;
import kilim.Task;

import erjang.EHandle;
import erjang.EInternalPort;
import erjang.EObject;
import erjang.EPID;
import erjang.ERef;

/**
 * 
 */
class LockingDriverInstance extends EDriverControl {

	private final ReentrantLock lock;
	private final EDriverControl target;

	/**
	 * 
	 */
	public LockingDriverInstance(EDriverControl target, ReentrantLock lock) {
		this.target = target;
		this.lock = lock;
	}

	@Override
	protected EObject call(EPID caller, int command, EObject data) throws Pausable {
		// System.err.println(Task.getCurrentTask() + ">>call");
		lock.lock();
		try {
			return target.call(caller, command, data);
		} finally {
			// System.err.println(Task.getCurrentTask() + "<<call");
			lock.unlock();
		}
	}

	@Override
	protected ByteBuffer control(EPID pid, int command, ByteBuffer buf) throws Pausable {
		// System.err.println(Task.getCurrentTask() + ">>control");
		lock.lock();
		try {
			return target.control(pid, command, buf);
		} finally {
			// System.err.println(Task.getCurrentTask() + "<<control");
			lock.unlock();
		}
	}


	@Override
	protected void outputv(EHandle caller, ByteBuffer[] ev) throws IOException, Pausable {
		// System.err.println(Task.getCurrentTask() + ">>outputv");
		lock.lock();
		try {
			target.outputv(caller, ev);
		} finally {
			// System.err.println(Task.getCurrentTask() + "<<outputv");
			lock.unlock();
		}
	}

	@Override
	public void processExit(ERef monitor) throws Pausable {
		// System.err.println(Task.getCurrentTask() + ">>processExit");
		lock.lock();
		try {
			target.processExit(monitor);
		} finally {
			// System.err.println(Task.getCurrentTask() + "<<processExit");
			lock.unlock();
		}
	}

	@Override
	protected void readyInput(SelectableChannel evt) throws Pausable {
		// System.err.println(Task.getCurrentTask() + ">>readyInput");
		lock.lock();
		try {
			target.readyInput(evt);
		} finally {
			// System.err.println(Task.getCurrentTask() + "<<readyInput");
			lock.unlock();
		}
	}

	@Override
	protected void readyOutput(SelectableChannel evt) throws Pausable {
		// System.err.println(Task.getCurrentTask() + ">>readyOutput");
		lock.lock();
		try {
			target.readyOutput(evt);
		} finally {
			// System.err.println(Task.getCurrentTask() + "<<readyOutput");
			lock.unlock();
		}
	}

	@Override
	protected void stop(EObject reason) throws Pausable {
		// System.err.println(Task.getCurrentTask() + ">>stop");
		lock.lock();
		try {
			target.stop(reason);
		} finally {
			// System.err.println(Task.getCurrentTask() + "<<stop");
			lock.unlock();
		}
	}

	@Override
	protected void stopSelect(SelectableChannel ch) throws Pausable {
		// System.err.println(Task.getCurrentTask() + ">>stopSelect");
		lock.lock();
		try {
			target.stopSelect(ch);
		} finally {
			// System.err.println(Task.getCurrentTask() + "<<stopSelect");
			lock.unlock();
		}
	}

	@Override
	protected void timeout() throws Pausable {
		// System.err.println(Task.getCurrentTask() + ">>timeout");
		lock.lock();
		try {
			target.timeout();
		} finally {
			// System.err.println(Task.getCurrentTask() + "<<timeout");
			lock.unlock();
		}
	}

	/* (non-Javadoc)
	 * @see erjang.driver.EDriverInstance#readyAsync(erjang.driver.EAsync)
	 */
	@Override
	protected void readyAsync(EAsync data) throws Pausable {
		// System.err.println(Task.getCurrentTask() + ">>readyAsync");
		lock.lock();
		try {
			target.readyAsync(data);
		} finally {
			// System.err.println(Task.getCurrentTask() + "<<readyAsync");
			lock.unlock();
		}
	}

	@Override
	public void readyAccept(SelectableChannel ch) throws Pausable {
		// System.err.println(Task.getCurrentTask() + ">>readyAccept");

		lock.lock();
		try {
			target.readyAccept(ch);
		} finally {
			// System.err.println(Task.getCurrentTask() + "<<readyAccept");
			lock.unlock();
		}
	}
	
	@Override
	public void readyConnect(SelectableChannel evt) throws Pausable {
		// System.err.println(Task.getCurrentTask() + ">>readyConnect");
		lock.lock();
		try {
			target.readyConnect(evt);
		} finally {
			// System.err.println(Task.getCurrentTask() + "<<readyConnect");
			lock.unlock();
		}
	}

	@Override
	void setTask(EDriverTask task) {
		target.setTask(task);
	}
	
	@Override
	public EDriver getDriver() {
		return target.getDriver();
	}
	 
	
	@Override
	public void setup() {
		target.setup();
	}
}
