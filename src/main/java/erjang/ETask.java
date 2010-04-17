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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import erjang.m.erlang.ErlProc;

import kilim.Mailbox;
import kilim.Pausable;

/**
 * An ETask is what is common for processes and open ports
 */
public abstract class ETask<H extends EHandle> extends kilim.Task {

	static Logger log = Logger.getLogger(ETask.class.getName());
	
	public static final EAtom am_normal = EAtom.intern("normal");
	protected static final EAtom am_java_exception = EAtom
			.intern("java_exception");
	private static final EAtom am_DOWN = EAtom.intern("DOWN");
	private static final EAtom am_process = EAtom.intern("process");

	/**
	 * @return
	 */
	public abstract H self_handle();

	protected Set<EHandle> links = new ConcurrentSkipListSet<EHandle>();
	protected Map<ERef,ETuple2> monitors = new ConcurrentHashMap<ERef, ETuple2>();

	public void unlink(EHandle handle) {
		links.remove(handle);
	}
	
	/**
	 * @param task
	 * @throws Pausable 
	 */
	public void link_to(ETask<?> task) throws Pausable {
		link_oneway(task.self_handle());
		task.self_handle().link_oneway((EHandle) self_handle());
	}

	public void link_to(EHandle handle) throws Pausable {
		link_oneway(handle);
		handle.link_oneway((EHandle) self_handle());
	}

	public void link_oneway(EHandle h) throws Pausable {
		// TODO: check if h is valid.
		
		if (h.exists()) {
			links.add(h);
		} else {
			link_failure(h);
		}
	}

	/**
	 * @param h
	 * @throws Pausable 
	 * @throws Pausable 
	 */
	protected void link_failure(EHandle h) throws Pausable {
		throw new ErlangError(ERT.am_noproc);
	}

	protected void do_proc_termination(EObject exit_reason) throws Pausable {
		this.exit_reason = exit_reason;
		H me = self_handle();
		for (EHandle handle : links) {
			handle.exit_signal(me, exit_reason);
		}
		for (Map.Entry<ERef, ETuple2> ent : monitors.entrySet()) {
			ETuple2 pid_object = ent.getValue();
			ERef ref = ent.getKey();
			
			EPID pid = (EPID) pid_object.elm(1);
			EObject object = pid_object.elm(2);
			
			if (object == null) {
				object = me;
			}
			
			pid.send(ETuple.make(am_DOWN, ref, am_process, object, exit_reason));
		}
	}
	

	// this is not synchronized, as we only mess with it from this proc
	Map<ERef,EHandle> is_monitoring = new HashMap<ERef, EHandle>();

	/**
	 * @param selfHandle
	 * @param object
	 * @return
	 */
	public ERef monitor(EHandle observed, EObject object) {
		ERef ref = observed.add_monitor(self_handle(), object);
		if (ref == null) {
			return ref;
		}
		this.is_monitoring.put(ref, observed);
		return ref;
	}

	/**
	 * @param r
	 * @param flush
	 * @return
	 */
	public void demonitor(ERef r, boolean flush) {
		EHandle h = is_monitoring.get(r);
		if (h == null) {
			throw ERT.badarg(r, flush ? ERT.NIL.cons(ErlProc.am_flush) : ERT.NIL);
		}

		h.remove_monitor(r, flush);
	}
	
	/**
	 * @param self2
	 * @return
	 */
	public ERef add_monitor(EHandle target, EObject object) {
		ERef ref = ERT.getLocalNode().createRef();
		monitors.put(ref, new ETuple2(target, object));
		return ref;
	}
	
	/**
	 * @param r
	 */
	public void remove_monitor(ERef r, boolean flush) {
		ETuple2 val = monitors.remove(r);
		if (flush) {
			// TODO: do we need to represent flush somehow?
		}
	}
	
	public EHandle get_monitored_process(ERef monitor) {
		return is_monitoring.get(monitor);
	}




	static final int MAX_MAILBOX_SIZE = 1000;
	protected Mailbox<EObject> mbox = new Mailbox<EObject>(10, MAX_MAILBOX_SIZE);

	protected static enum State {
		INIT, // has not started yet
		RUNNING, // is live
		EXIT_SIG, // received exit signal
		SENDING_EXIT,
		DONE
		// done
	};

	protected State pstate = State.INIT;
	protected EObject exit_reason;

	public int reds;

	/**
	 * @throws Pausable
	 * 
	 */
	public void mbox_wait() throws Pausable {
		mbox.untilHasMessage();
	}

	/**
	 * @param longValue
	 */
	public boolean mbox_wait(long timeoutMillis) throws Pausable {
		return mbox.untilHasMessage(timeoutMillis);
	}

	/**
	 * @param msg
	 * @throws Pausable
	 */
	public void mbox_send(EObject msg) throws Pausable {
		mbox.put(msg);
	}

	/**
	 * @return
	 * @throws Pausable
	 */
	public void mbox_remove_one() throws Pausable {
		mbox.get();
	}

	/**
	 * @param from
	 * @param reason
	 */
	public final void send_exit(EHandle from, EObject reason) throws Pausable {

		log.fine("exit " + from + " -> " + this + ", reason="+reason);
		
		// ignore exit signals from myself
		if (from == self_handle()) {
			return;
		}
		
		// make sure we don't also send him an exit signal
		links.remove(from);

		synchronized (this) {
			switch (pstate) {

			// process is already "done", just ignore exit signal
			case DONE:
				return;

				// we have already received one exit signal, ignore
				// subsequent ones...
			case EXIT_SIG:
				// TODO: warn that this process is not yet dead. why?
				return;

				// the process is not running yet, this should not happen
			case INIT:
				throw new Error(
						"cannot receive exit signal before we're running");

			default:
				throw new Error("unknown state? "+pstate);

			case RUNNING:
			}
		}

		process_incoming_exit(from, reason);

	}

	protected abstract void process_incoming_exit(EHandle from, EObject reason)
			throws Pausable;

	/**
	 * will check if this process have received an exit signal (and we're not
	 * trapping)
	 */
	public final void check_exit() {
		if (this.pstate == State.EXIT_SIG) {
			if (exit_reason != null) {
				this.pstate = State.SENDING_EXIT;
				EObject reason = exit_reason;
				throw new ErlangExitSignal(reason);
			}
		}
	}
	

	/* (non-Javadoc)
	 * @see kilim.Task#checkKill()
	 */
	@Override
	public void checkKill() {
		check_exit();
	}

	/**
	 * @return
	 */
	public Mailbox<EObject> mbox() {
		return mbox;
	}

	/**
	 * @return
	 */
	public boolean exists() {
		return pstate == State.INIT || pstate == State.RUNNING;
	}

}
