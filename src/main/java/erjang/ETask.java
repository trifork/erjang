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
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.trifork.clj_ds.IPersistentSet;
import com.trifork.clj_ds.PersistentHashSet;

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

	private AtomicReference<PersistentHashSet<EHandle>> 
		linksref = new AtomicReference<PersistentHashSet<EHandle>>(PersistentHashSet.EMPTY);

	protected Map<ERef,EHandle> monitors = new ConcurrentHashMap<ERef, EHandle>();

	public void unlink(EHandle other) throws Pausable {
		unlink_oneway(other);
		other.unlink_oneway(self_handle());
	}
	
	public void unlink_oneway(EHandle handle) {
		PersistentHashSet old, links;
		try {

			do {
				old = linksref.get();
				links = (PersistentHashSet) old.disjoin(handle);
			} while (!linksref.weakCompareAndSet(old, links));

		} catch (Exception e) {
			throw new RuntimeException(e);
		}		
	}
	
	protected boolean has_no_links() {
		IPersistentSet<EHandle> links = linksref.get();
		return links.count() == 0;
	}
	
	/**
	 * @param task
	 * @throws Pausable 
	 */
	public void link_to(ETask<?> task) throws Pausable {
		link_to(task.self_handle());
	}

	public void link_to(EHandle handle) throws Pausable {
		if (!link_oneway(handle) || !handle.link_oneway((EHandle) self_handle())) {
			link_failure(handle);
		}		
	}

	public boolean link_oneway(EHandle h) {
		// TODO: check if h is valid.
		
		if (h.exists()) {

			PersistentHashSet old, links;
			try {
				
				do {
					old = linksref.get();
					links = (PersistentHashSet) old.cons(h);
				} while (!linksref.weakCompareAndSet(old, links));
				
			} catch (Exception e) {
				throw new RuntimeException(e);
			}		

			return true;
		} else {
			return false;
		}
	}

	public ESeq links() {
		ESeq res = ERT.NIL;
		for (EHandle h : linksref.get()) {
			res = res.cons(h);
		}
		return res;
	}


	/**
	 * @param h
	 * @throws Pausable 
	 * @throws Pausable 
	 * @throws Pausable 
	 * @throws Pausable 
	 */
	protected void link_failure(EHandle h) throws Pausable  {
		throw new ErlangError(ERT.am_noproc);
	}

	@SuppressWarnings("unchecked")
	protected void do_proc_termination(EObject exit_reason) throws Pausable {
		this.exit_reason = exit_reason;
		H me = self_handle();
		EAtom name = me.name;
		for (EHandle handle : linksref.get()) {
			try {
			handle.exit_signal(me, exit_reason, false);
			} catch (Error e) {
				System.err.println("EXCEPTION IN EXIT HANDLER");
				e.printStackTrace();
				throw e;
			} catch (RuntimeException e) {
				System.err.println("EXCEPTION IN EXIT HANDLER");
				e.printStackTrace();
				throw e;
			}
		}
		for (Map.Entry<ERef, EHandle> ent : monitors.entrySet()) {
			EHandle pid = ent.getValue();
			ERef ref = ent.getKey();

			pid.send_monitor_exit((EHandle)me, ref, exit_reason);
		}
		if (name != null) {
			ERT.unregister(name);
		}
	}
	

	public void send_monitor_exit(EHandle from, ERef ref, EObject reason) throws Pausable {
		ETuple2 pair = is_monitoring.get(ref);
		if (pair != null) {
			mbox_send(ETuple.make(am_DOWN, ref, am_process, pair.elem2, reason));
		}
	}
	
	

	// this is not synchronized, as we only mess with it from this proc
	Map<ERef,ETuple2> is_monitoring = new HashMap<ERef, ETuple2>();

	/**
	 * @param object
	 * @param ref TODO
	 * @param selfHandle
	 * @return
	 * @throws Pausable 
	 */
	public boolean monitor(EHandle observed, EObject object, ERef ref) throws Pausable {
		if (!observed.add_monitor(self_handle(), ref)) {
		//	System.err.println("unable to add monitor to self="+self_handle()+" pid="+observed+" ref="+ref);
			return false;
		}
		this.is_monitoring.put(ref, new ETuple2(observed, object));
		return true;
	}

	public boolean monitor(EObject object, ERef ref) throws Pausable {
		this.is_monitoring.put(ref, new ETuple2(object, object));
		return true;
	}

	/**
	 * @param r
	 * @return
	 * @throws Pausable 
	 */
	public EObject demonitor(ERef r) throws Pausable {
		ETuple2 pair = is_monitoring.remove(r);
		if (pair == null) {
			return null;
		}
		
		return pair.elem1;
	}
	
	/**
	 * @param ref TODO
	 * @param self2
	 * @return
	 */
	public boolean add_monitor(EHandle target, ERef ref) {
		monitors.put(ref, target);
		return true;
	}
	
	/**
	 * @param r
	 */
	public void remove_monitor(ERef r, boolean flush) {
		EHandle val = monitors.remove(r);
		if (flush) {
			// TODO: do we need to represent flush somehow?
		}
	}
	
	public EHandle get_monitored_process(ERef monitor) {
		ETuple2 tup = is_monitoring.get(monitor);
		if (tup == null) return null;
		return tup.elem1.testHandle();
	}

	public EObject get_monitored_object(ERef monitor) {
		ETuple2 tup = is_monitoring.get(monitor);
		return tup.elem2;
	}




	static final int MAX_MAILBOX_SIZE = 1000;
	protected final Mailbox<EObject> mbox = new Mailbox<EObject>(10, MAX_MAILBOX_SIZE);

	public static final int STATE_INIT = 0; // has not started yet
	public static final int STATE_RUNNING = 1; // is live
	public static final int STATE_EXIT_SIG = 2; // received exit signal
	public static final int STATE_SENDING_EXIT = 3;
	public static final int STATE_DONE = 4; // done

	protected volatile int pstate = STATE_INIT;
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
	 * @param is_erlang_exit2 TODO
	 */
	public final void send_exit(EHandle from, EObject reason, boolean is_erlang_exit2) throws Pausable {

		if (log.isLoggable(Level.FINE)) {
			log.log (Level.FINE, "exit " + from + " -> " + this + ", reason="+reason, new Throwable("trace"));
		}
		
		// ignore exit signals from myself
		if (from == self_handle()) {
			return;
		}
		
		// make sure we don't also send him an exit signal
		if (!is_erlang_exit2)
			unlink_oneway(from);

		synchronized (this) {
			switch (pstate) {

			// process is already "done", just ignore exit signal
			case STATE_DONE:
				return;

				// we have already received one exit signal, ignore
				// subsequent ones...
			case STATE_EXIT_SIG:
			case STATE_SENDING_EXIT:
				// TODO: warn that this process is not yet dead. why?
				return;

				// the process is not running yet, this should not happen
			case STATE_INIT:
				if (reason == EProc.am_kill) {
					this.exit_reason = EProc.am_killed;
				} else {
					this.exit_reason = reason;
				}
				this.pstate = STATE_EXIT_SIG;
				return;

			default:
				throw new Error("unknown state? "+pstate);

			case STATE_RUNNING:
			}
		}

		process_incoming_exit(from, reason, is_erlang_exit2);

	}

	protected abstract void process_incoming_exit(EHandle from, EObject reason, boolean is_erlang_exit2) throws Pausable
			;

	/**
	 * will check if this process have received an exit signal (and we're not
	 * trapping)
	 */
	public final void check_exit() {
		if (this.pstate == STATE_EXIT_SIG) {
			do_check_exit();
		}
	}

	/** because the above is called a bazillion times, we split this out to make it easier to inline */
	private void do_check_exit() throws ErlangExitSignal {
		if (exit_reason != null) {
			this.pstate = STATE_SENDING_EXIT;
			EObject reason = exit_reason;
			throw new ErlangExitSignal(reason);
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
		return pstate == STATE_INIT || pstate == STATE_RUNNING;
	}

}
