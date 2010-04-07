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
import java.util.concurrent.locks.Lock;

import erjang.EObject;
import erjang.EPID;
import erjang.ERef;

/**
 * 
 */
class LockingDriverInstance extends EDriverInstance {

	private final Lock lock;
	private final EDriverInstance target;

	/**
	 * 
	 */
	public LockingDriverInstance(EDriverInstance target, Lock lock) {
		this.target = target;
		this.lock = lock;
	}

	@Override
	protected EObject call(EPID caller, int command, EObject data) {
		lock.lock();
		try {
			return target.call(caller, command, data);
		} finally {
			lock.unlock();
		}
	}

	@Override
	protected ByteBuffer control(EPID pid, int command, ByteBuffer buf) {
		lock.lock();
		try {
			return target.control(pid, command, buf);
		} finally {
			lock.unlock();
		}
	}


	@Override
	protected void flush() {
		lock.lock();
		try {
			target.flush();
		} finally {
			lock.unlock();
		}
	}

	@Override
	protected void output(ByteBuffer data) throws IOException {
		lock.lock();
		try {
			target.output(data);
		} finally {
			lock.unlock();
		}
	}

	@Override
	protected void outputv(EPID caller, ByteBuffer[] ev) throws IOException {
		lock.lock();
		try {
			target.outputv(null, ev);
		} finally {
			lock.unlock();
		}
	}

	@Override
	public void processExit(ERef monitor) {
		lock.lock();
		try {
			target.processExit(monitor);
		} finally {
			lock.unlock();
		}
	}

	@Override
	protected void readyInput(SelectableChannel evt) {
		lock.lock();
		try {
			target.readyInput(evt);
		} finally {
			lock.unlock();
		}
	}

	@Override
	protected void readyOutput(SelectableChannel evt) {
		lock.lock();
		try {
			target.readyOutput(evt);
		} finally {
			lock.unlock();
		}
	}

	@Override
	protected void stop() {
		lock.lock();
		try {
			target.stop();
		} finally {
			lock.unlock();
		}
	}

	@Override
	protected void stopSelect(SelectableChannel ch) {
		lock.lock();
		try {
			target.stopSelect(ch);
		} finally {
			lock.unlock();
		}
	}

	@Override
	protected void timeout() {
		lock.lock();
		try {
			target.timeout();
		} finally {
			lock.unlock();
		}
	}

	/* (non-Javadoc)
	 * @see erjang.driver.EDriverInstance#readyAsync(erjang.driver.EAsync)
	 */
	@Override
	protected void readyAsync(EAsync data) {
		lock.lock();
		try {
			target.readyAsync(data);
		} finally {
			lock.unlock();
		}
	}

	 
}
