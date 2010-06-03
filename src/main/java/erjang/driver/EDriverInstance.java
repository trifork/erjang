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
import java.util.ArrayList;
import java.util.concurrent.locks.Lock;

import kilim.Pausable;
import kilim.ReentrantLock;
import erjang.EAtom;
import erjang.EBinList;
import erjang.EBinary;
import erjang.EHandle;
import erjang.EInternalPort;
import erjang.EObject;
import erjang.EPID;
import erjang.EPeer;
import erjang.ERT;
import erjang.ERef;
import erjang.EString;
import erjang.ETask;
import erjang.ETuple;
import erjang.ErlangException;
import erjang.NotImplemented;
import erjang.driver.efile.Posix;
import erjang.m.erlang.ErlProc;

/**
 * 
 */
public abstract class EDriverInstance extends EDriverControl {

	EDriver driver;
	protected EDriverTask task;
	Lock pdl;
	
	public EDriverInstance(EDriver driver) {
		this.driver = driver;
	}

	public EDriver getDriver() {
		return driver;
	}

	public EInternalPort port() {
		return task.self_handle();
	}

	@Override
	void setTask(EDriverTask task) {
		this.task = task;
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

	protected void driver_exit(int i) {
		this.task.exit(i==0?ETask.am_normal:EAtom.intern(Posix.errno_id(i)));
	}


	protected void driver_async(EAsync job) {
		ERT.run_async(job, task);
	}

	protected void driver_outputv(ByteBuffer hdr, ByteBuffer[] ev) throws Pausable {
		
		EObject resp = ERT.NIL;
		
		if (task.send_binary_data) {
		
		if (ev.length > 0) {
			ev[ev.length-1].flip();
			resp = EBinary.make(ev[ev.length-1]);
		
			for (int i = ev.length-2; i >= 0; i--) {
				ev[i].flip();
				resp = resp.cons( EBinary.make(ev[i]) );
			}
		}
		
		} else {
			throw new NotImplemented();
		}
		
		hdr.flip();
		EBinList res = new EBinList(hdr, resp);

		task.output_from_driver(res);
	}
	
	protected void driver_output2(ByteBuffer header, ByteBuffer buf) throws Pausable {

		int status = task.status;
		
		if ((status & EDriverTask.ERTS_PORT_SFLG_CLOSING) != 0) {
			return;
		}
		
		header.flip();
		if (buf != null)
			buf.flip();
		

		if ((status & EDriverTask.ERTS_PORT_SFLG_DISTRIBUTION) != 0) {
			task.node().net_message(task.self_handle(), null, buf);
			return;
		}
		
		if ((status & EDriverTask.ERTS_PORT_SFLG_LINEBUF_IO) != 0) {
			throw new NotImplemented();
		}
		
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
	
	protected void driver_output(ByteBuffer buf) throws Pausable {
		
		int status = task.status;
		
		if ((status & EDriverTask.ERTS_PORT_SFLG_CLOSING) != 0) {
			return;
		}
		
		buf.flip();
		
		if ((status & EDriverTask.ERTS_PORT_SFLG_DISTRIBUTION) != 0) {
			task.node().net_message(task.self_handle(), null, buf);
			return;
		}
		
		if ((status & EDriverTask.ERTS_PORT_SFLG_LINEBUF_IO) != 0) {
			throw new NotImplemented();
		}
		

		EObject out;
		
		if (buf == null || !buf.hasRemaining()) {
			out = ERT.NIL;
		} else if (task.send_binary_data) {
			out = EBinary.make(buf);
		} else {
			out = EString.make(buf);
		}

		task.output_from_driver(out);
	}
	
	public void driver_output_term(EObject term) throws Pausable {
		task.output_term_from_driver(term);
	}

	public void driver_send_term(EHandle caller, ETuple msg) throws Pausable {
		if (caller != null) {
			caller.send(task.self_handle(), msg);
		}
	}


	/**
	 * @param fileRespOkHeader
	 * @param binp
	 * @throws Pausable 
	 */
	protected void driver_output_binary(byte[] header, ByteBuffer binp) throws Pausable {
		EObject out = EBinary.make(binp);
		if (header.length > 0) {
			out = new EBinList(header, out);
		}

		task.output_from_driver(out);
	}

	/**
	 */
	protected void driver_cancel_timer() {
		task.cancel_timer(port());
	}

	protected void driver_set_timer(long howlong) {
		task.set_timer(howlong);
	}

	protected long driver_read_timer() {
		return task.read_timer();
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

	protected boolean driver_demonitor_process(ERef monitor) throws Pausable {
		try {
			return ErlProc.demonitor(task, monitor, ERT.NIL) == ERT.TRUE;
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

	
	protected ERef driver_monitor_process(EPID pid) throws Pausable {
		ERef ref = ERT.getLocalNode().createRef();
		
		if (!task.monitor(pid, pid, ref)) {
			port().send(port(), ETuple.make(ERT.am_DOWN, ref, ErlProc.am_process, pid, ERT.am_noproc));
		}
		
		return ref;
	}


	protected int driver_sizeq() {
		if (queue == null) return 0;
		
		int size = 0;
		int p = 0;
		for (p = 0; p < queue.length; p++) {
			if (queue[p] != null)
				size += queue[p].remaining();
		}
		return size;
	}
	
	protected long driver_deq(long size) {

		ByteBuffer[] queue = this.queue;
		
		if (queue == null)
			return 0;

		int p = 0;
		for (p = 0; p < queue.length && queue[p] != null && !queue[p].hasRemaining(); p++) {
			/* skip */
		}

		if (p == queue.length)
			return 0;

		long res = 0;
		for (int i = 0; (p+i) < queue.length; i++) {
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
		if (queue == null || queue[0] == null) 
			queue = q;
		else {
			
			ArrayList<ByteBuffer> bbs = new ArrayList<ByteBuffer>();
			for (int i = 0; i < queue.length; i++) {
				if (queue[i] != null && queue[i].hasRemaining())
					bbs.add(queue[i]);
			}
			
			for (int i = 0; i < q.length; i++) {
				if (q[i] != null && q[i].hasRemaining())
					bbs.add(q[i]);
			}
			
			queue = bbs.toArray(new ByteBuffer[bbs.size()]);
			
		}
	}

	/*
	 * Called on behalf of driver_select when it is safe to release 'event'. A
	 * typical unix driver would call close(event)
	 */
	protected void stopSelect(SelectableChannel event) throws Pausable {
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

	protected void stop(EObject reason) throws Pausable {
		if ((task.status & EDriverTask.ERTS_PORT_SFLG_DISTRIBUTION) != 0) {
			
			EPeer node = task.node();
			
			if (node != null) {
				node.node_going_down(port(), reason);
			}
			
			task.status &= ~EDriverTask.ERTS_PORT_SFLG_DISTRIBUTION;
		}
	}

	/*
	 * called when we have output from erlang to the port
	 */
	protected abstract void output(EHandle caller, ByteBuffer buf) throws IOException, Pausable;

	/*
	 * called when we have output from erlang to the port, and the iodata()
	 * passed in contains multiple fragments. Default behavior is to flatten the
	 * input vector, and call EDriverInstance#output(ByteBuffer).
	 */
	protected void outputv(EHandle caller, ByteBuffer[] ev) throws IOException, Pausable {
		output(caller, flatten(ev));
	}

	/*
	 * called when we have input from one of the driver's handles)
	 */
	protected void readyInput(SelectableChannel ch) throws Pausable {};

	/*
	 * called when output is possible to one of the driver's handles
	 */
	protected void readyOutput(SelectableChannel evt) throws Pausable {};

	/* called when "action" is possible, async job done */
	protected void readyAsync(EAsync data) throws Pausable {}

	/*
	 * "ioctl" for drivers - invoked by port_control/3)
	 */
	protected ByteBuffer control(EPID pid, int command, ByteBuffer cmd) throws Pausable {
		throw ERT.badarg();
	}

	/* Handling of timeout in driver */
	protected void timeout() throws Pausable {};

	/*
	 * called when the port is about to be closed, and there is data in the
	 * driver queue that needs to be flushed before 'stop' can be called
	 */
	protected void flush() throws Pausable {};

	/*
	 * Works mostly like 'control', a syncronous call into the driver.
	 */
	protected EObject call(EPID caller, int command, EObject data) throws Pausable {
		throw ERT.badarg();
	}


	/**
	 * @param ch
	 * @throws Pausable 
	 */
	public void readyConnect(SelectableChannel evt) throws Pausable {
		// TODO Auto-generated method stub

	}

	/**
	 * @param ch
	 * @throws Pausable 
	 */
	public void readyAccept(SelectableChannel ch) throws Pausable {

	}

	protected void set_busy_port(boolean b) {
		throw new erjang.NotImplemented();
		
	}


}
