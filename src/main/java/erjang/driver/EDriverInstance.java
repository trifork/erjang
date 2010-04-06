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
import java.nio.channels.SelectionKey;
import java.util.concurrent.locks.Lock;

import kilim.ReentrantLock;
import erjang.EBinList;
import erjang.EBinary;
import erjang.EHandle;
import erjang.EInternalPort;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.ERT;
import erjang.ERef;
import erjang.EString;
import erjang.ETuple;
import erjang.ErlangException;

/**
 * 
 */
public abstract class EDriverInstance extends EDriverControl {

	EDriverTask task;
	Lock pdl;
	
	protected EInternalPort port() {
		return task.self_handle();
	}

	protected static final int ERL_DRV_READ = SelectionKey.OP_READ;
	protected static final int ERL_DRV_WRITE = SelectionKey.OP_WRITE;
	protected static final int ERL_DRV_ACCEPT = SelectionKey.OP_ACCEPT;
	protected static final int ERL_DRV_CONNECT = SelectionKey.OP_CONNECT;
	protected static final int ERL_DRV_USE = 1 << 5;

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
		header.flip();
		if (buf != null)
			buf.flip();

		EObject tail = null;
		if (buf == null || !buf.hasRemaining()) {
			tail = ERT.NIL;
		} else if (task.send_binary_data) {
			tail = EBinary.make(buf);
		} else {
			tail = EString.make(buf);
		}

		EBinList out = new EBinList(header, tail);
		task.output_from_driver(out);
	}

	/**
	 * @param fileRespOkHeader
	 * @param binp
	 */
	protected void driver_output_binary(byte[] header, ByteBuffer binp) {
		EObject out = EBinary.make(binp);
		if (header.length > 0) {
			out = new EBinList(header, out);
		}

		task.output_from_driver(out);
	}

	/**
	 * @param port2
	 */
	protected void driver_cancel_timer(EPort port2) {
		// TODO 
		task.cancel_timer(port2);
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
	private EPID caller;

	/**
	 * @return
	 */
	protected ByteBuffer[] driver_peekq() {
		return queue;
	}
	
	protected EPID driver_caller() {
		return this.caller;
	}

	protected boolean driver_demonitor_process(ERef monitor) {
		try {
			task.demonitor(monitor, false);
			return true;
		} catch (ErlangException e) {
			if (ERT.DEBUG_PORT) {
				e.printStackTrace();
			}
			return false;
		}
	}

	
	protected EHandle driver_get_monitored_process(ERef monitor) {
		return task.get_monitored_process(monitor);
	}

	
	protected ERef driver_monitor_process(EPID pid) {
		ERef ref = task.monitor(pid, pid);
		
		if (ref == null) {
			ref = ERT.getLocalNode().createRef();
			port().sendnb(ETuple.make(ERT.am_DOWN, ref, pid, ERT.am_noproc));
		}
		
		return ref;
	}


	protected int driver_sizeq() {
		if (queue == null) return 0;
		
		int size = 0;
		int p = 0;
		for (p = 0; p < queue.length && !queue[p].hasRemaining(); p++) {
			size += queue[p].remaining();
		}
		return size;
	}
	
	protected long driver_deq(long size) {

		if (queue == null)
			return 0;

		int p = 0;
		for (p = 0; p < queue.length && queue[p] != null && !queue[p].hasRemaining(); p++) {
			/* skip */
		}

		if (p == queue.length)
			return 0;

		long res = 0;
		for (int i = 0; i < p; i++) {
			queue[i] = queue[p + i];
			if (queue[i] != null) {
			 res += queue[i].remaining();
			}
		}

		for (int i = p; i < queue.length; i++) {
			queue[i] = null;
		}
		
		return res;
	}
	
	protected void driver_enqv(ByteBuffer[] q) {
		queue = q;
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
		return EDriverTask.flatten(out);
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
	protected abstract void output(ByteBuffer data) throws IOException;

	/*
	 * called when we have output from erlang to the port, and the iodata()
	 * passed in contains multiple fragments. Default behavior is to flatten the
	 * input vector, and call EDriverInstance#output(ByteBuffer).
	 */
	protected void outputv(ByteBuffer[] ev) throws IOException {
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
	protected ByteBuffer control(EPID pid, int command, ByteBuffer cmd) {
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
	protected EObject call(EPID caller, int command, EObject data) {
		throw ERT.badarg();
	}


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

	protected void set_busy_port(boolean b) {
		throw new erjang.NotImplemented();
		
	}


}
