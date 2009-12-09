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

import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.util.concurrent.locks.Lock;

import kilim.ReentrantLock;

import erjang.EObject;
import erjang.EPort;
import erjang.ERT;
import erjang.ERef;

/**
 * 
 */
public abstract class EDriverInstance {

	EDriverTask task;
	Lock pdl;

	static final int ERL_DRV_READ = SelectionKey.OP_READ;
	static final int ERL_DRV_WRITE = SelectionKey.OP_WRITE;
	static final int ERL_DRV_ACCEPT = SelectionKey.OP_ACCEPT;
	static final int ERL_DRV_CONNECT = SelectionKey.OP_CONNECT;
	static final int ERL_DRV_USE = 1 << 5;

	static private final int ALL_OPS = ERL_DRV_READ | ERL_DRV_WRITE
			| ERL_DRV_ACCEPT | ERL_DRV_CONNECT;

	/**
	 * Register selector
	 * 
	 * @param ch
	 *            {@link SelectableChannel} for which to perform select
	 * @param mode
	 *            bit-or of ERL_DRV_{READ,WRITE,ACCEPT,CONNECT}
	 * @param onOff
	 *            one of {@link SelectMode}.SET or {@link SelectMode}.CLEAR
	 */
	public void select(SelectableChannel ch, int mode, SelectMode onOff) {

		int selectOps = mode & ALL_OPS;
		if (onOff == SelectMode.SET) {
			NIOSelector.setInterest(ch, selectOps, task);
		} else if (onOff == SelectMode.CLEAR) {
			boolean releaseNotify = (mode & ERL_DRV_USE) == ERL_DRV_USE;
			NIOSelector.clearInterest(ch, selectOps, releaseNotify, task);
		}
	}

	protected void driver_async(EAsync job) {
		ERT.run_async(job, task);
	}

	protected void driver_output2(ByteBuffer header, ByteBuffer buf) {

	}

	/**
	 * @param port2
	 */
	protected void driver_cancel_timer(EPort port2) {
		// TODO Auto-generated method stub

	}

	protected void driver_set_timer(long howlong) {
		task.set_timer(howlong);
	}

	/**
	 * @return
	 */
	protected Lock driver_pdl_create() {
		if (pdl == null) {
			pdl = new ReentrantLock();
		}
		return pdl;
	}

	private ByteBuffer[] queue = null;

	/**
	 * @return
	 */
	protected ByteBuffer[] driver_peekq() {
		return queue;
	}

	protected void driver_deq(long size) {

		if (queue == null)
			return;
		
		int p = 0;
		for (p = 0; p < queue.length && !queue[p].hasRemaining(); p++) {
			/* skip */
		}

		if (p == queue.length)
			return;

		for (int i = 0; i < p; i++) {
			queue[i] = queue[p+i];
		}
		
		for (int i = p; i < queue.length; i++) {
			queue[i] = null;
		}
	}

	/*
	 * Called on behalf of driver_select when it is safe to release 'event'. A
	 * typical unix driver would call close(event)
	 */
	protected void stopSelect(SelectableChannel event) {
	}

	/**
	 * @param out
	 * @return
	 */
	public static ByteBuffer flatten(ByteBuffer[] out) {
		if (out.length == 0) {
			return ERT.EMPTY_BYTEBUFFER;
		} else if (out.length == 1) {
			return out[0];
		} else {
			int size = 0;
			for (int i = 0; i < out.length; i++) {
				size += out[i].position();
				out[i].flip();
			}
			ByteBuffer res = ByteBuffer.allocate(size);
			for (int i = 0; i < out.length; i++) {
				res.put(out[i]);
			}
			return res;
		}
	}

	/*
	 * called when port is closed, and when the emulator is halted. Default
	 * behavior is to do nothing.
	 */

	protected void stop() {
	}

	/*
	 * called when we have output from erlang to the port
	 */
	protected abstract void output(ByteBuffer data);

	/*
	 * called when we have output from erlang to the port, and the iodata()
	 * passed in contains multiple fragments. Default behavior is to flatten the
	 * input vector, and call EDriverInstance#output(ByteBuffer).
	 */
	protected void outputv(ByteBuffer[] ev) {
		output(flatten(ev));
	}

	/*
	 * called when we have input from one of the driver's handles)
	 */
	protected abstract void readyInput(SelectableChannel ch);

	/*
	 * called when output is possible to one of the driver's handles
	 */
	protected abstract void readyOutput(SelectableChannel evt);

	/* called when "action" is possible, async job done */
	protected abstract void readyAsync(EAsync data);

	/*
	 * "ioctl" for drivers - invoked by port_control/3)
	 */
	protected ByteBuffer control(int command, ByteBuffer buf) {
		throw ERT.badarg();
	}

	/* Handling of timeout in driver */
	protected abstract void timeout();

	/*
	 * called when the port is about to be closed, and there is data in the
	 * driver queue that needs to be flushed before 'stop' can be called
	 */
	protected abstract void flush();

	/*
	 * Works mostly like 'control', a syncronous call into the driver.
	 */
	protected abstract EObject call(int command, EObject data);

	/*
	 * Called when an event selected by driver_event() has occurred
	 */
	protected abstract void event(EDriverEvent event, Object eventData);

	protected abstract void processExit(ERef monitor);

	/**
	 * @param ch
	 */
	public void readyConnect(SelectableChannel evt) {
		// TODO Auto-generated method stub

	}

	/**
	 * @param ch
	 */
	public void readyAccept(SelectableChannel ch) {

	}

}
