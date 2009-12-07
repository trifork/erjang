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

import erjang.EObject;
import erjang.ERT;
import erjang.ERef;

/**
 * 
 */
public abstract class EDriverInstance {

	EDriverTask task;

	static final int ERL_DRV_READ = SelectionKey.OP_READ;
	static final int ERL_DRV_WRITE = SelectionKey.OP_WRITE;
	static final int ERL_DRV_ACCEPT = SelectionKey.OP_ACCEPT;
	static final int ERL_DRV_CONNECT = SelectionKey.OP_CONNECT;
	static final int ERL_DRV_USE = 1 << 5;

	static private final int ALL_OPS = ERL_DRV_READ | ERL_DRV_WRITE
			| ERL_DRV_ACCEPT | ERL_DRV_CONNECT;

	public void select(SelectableChannel event, int mode, SelectMode onOff) {

		int selectOps = mode & ALL_OPS;
		if (onOff == SelectMode.SET) {
			NIOSelector.setInterest(event, selectOps, task);
		} else if (onOff == SelectMode.CLEAR) {
			boolean releaseNotify = (mode & ERL_DRV_USE) == ERL_DRV_USE;
			NIOSelector.clearInterest(event, selectOps, releaseNotify, task);
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

	/* called when "action" is possible */
	protected abstract void readyAsync(SelectableChannel data);

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
		// TODO Auto-generated method stub
		
	}

}
