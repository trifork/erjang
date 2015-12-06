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
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;
import java.util.logging.Logger;

import kilim.Mailbox;
import kilim.Pausable;

import com.trifork.clj_ds.IPersistentSet;
import com.trifork.clj_ds.PersistentHashSet;
import kilim.Task;

/**
 * An ETask is what is common for processes and open ports
 */
public abstract class ETask<H extends EHandle> extends kilim.Task {

	private static Logger log = Logger.getLogger("erjang.proc");

    /*==================== Constants =============================*/

    public static final EAtom am_normal = EAtom.intern("normal");
	protected static final EAtom am_java_exception = EAtom
			.intern("java_exception");
	private static final EAtom am_DOWN = EAtom.intern("DOWN");
	private static final EAtom am_process = EAtom.intern("process");

    private static final int MAX_MAILBOX_SIZE = 1000;

    /*==================== Typedefs ==============================*/
    public enum STATE {
        INIT, RUNNING, EXITING, DONE
    };

    private static final int MUTATOR_COUNT_ONE = (1<<4);
    private static final int PSTATE_MASK = MUTATOR_COUNT_ONE - 1;
    private static final int MUTATOR_COUNT_MASK = ~PSTATE_MASK;

    /*==================== Process state ====================================*/

    /** Process lifecycle:
     *
     *   INIT -----> RUNNING --------------------> DONE
     *     \           \                       /
     *     v           v                      /
     *   INIT w/      RUNNING w/             /
     *   exit_reason  exit_reason           /
     *     \              \                /
     *      -------------------> EXITING --
     *
     * For reliability (trigger-once guarantee) of exit-actions, the following invariant is enforced:
     * Once the state is DONE, the set of exit-actions (links, monitors, and exit hooks) MUST NOT
     * change (as there would be e.g. add/perform race conditions in that case),
     * This is accompliced by keeping the pstate in an AtomicInteger together with the number of current exit-action
     * mutators, in such a manner that the (logical) transition to DONE can only occur when there are zero mutators.
     * (Value: <code>(n_mutators << 4) | (STATE value)</code>,  where n_mutators is the number of actual mutators
     * plus the number of wannabe mutators.
     *
     * The critical invariant here is:
     *     <em>When the state value part is DONE, the number of actual
     * mutators will not increase</em>.
     * (The number of wannabe mutators may increase, but immediate decreases
     * once the DONE-ness of the state is discovered; the distinction between
     * wannabe mutators and non-mutators is really a performance question,
     * allowing use of getAndAdd().)
     *
     * Changes to links/monitors/exit hooks must honour this updating pattern.
     * TODO: Follow through on this.
     */
    private AtomicInteger pstate_and_mutator_count = new AtomicInteger(STATE.INIT.ordinal());

    private AtomicReference<PersistentHashSet<EHandle>>
            linksref = new AtomicReference<PersistentHashSet<EHandle>>(PersistentHashSet.EMPTY);

    protected Map<ERef,EHandle> monitors = new ConcurrentHashMap<ERef, EHandle>();

    // this is not synchronized, as we only mess with it from this proc
    // TODO That is not accurate, cf. do_proc_termination->send_monitor_exit call. --ESS
    private Map<ERef,ETuple2> is_monitoring = new HashMap<ERef, ETuple2>();


    protected final Mailbox<EObject> mbox = new Mailbox<EObject>(10, MAX_MAILBOX_SIZE);

    /** Reduction counter. */
    private int reds;

    /** Exit signal and reason -
     *  null when task is alive or terminated normally; non-null when task is terminated/terminating abnormally.
     *  Killer and exit_reason are always set together.
     * */
    protected volatile EObject exit_reason;
    protected Throwable killer;

    private Mailbox<EObject> print_trace;

    /*==================== Instance methods ====================================*/

    /**
	 * @return
	 */
	public abstract H self_handle();

    /*--------- Link related ----------------------------*/

    public void unlink(EHandle other) throws Pausable {
		unlink_oneway(other);
		other.unlink_oneway(self_handle());
	}
	
	public boolean unlink_oneway(EHandle handle) {
        int ps = exit_action_mutator_lock();
        try {
            if (ps == STATE.DONE.ordinal()) return false; // Too late.

            PersistentHashSet old, links;
            try {

                do {
                    old = linksref.get();
                    links = (PersistentHashSet) old.disjoin(handle);
                } while (!linksref.weakCompareAndSet(old, links));
                return true;

            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        } finally {
            exit_action_mutator_unlock();
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
		if (link_oneway(handle)) {
          handle.link_oneway((EHandle) self_handle()); // Ignore failure if linker has received SIG_EXIT
        } else {
			link_failure(handle);
		}
	}

	public boolean link_oneway(EHandle h) {
        int ps = exit_action_mutator_lock();
        try {
            if (ps == STATE.DONE.ordinal()) return false; // Too late.

            PersistentHashSet old, links;
            try {
                do {
                    old = linksref.get();
                    links = (PersistentHashSet) old.cons(h);
                } while (!linksref.weakCompareAndSet(old, links));
                return true;
            } catch (Exception e) {
                throw new RuntimeException(e);
            }

        } finally {
            exit_action_mutator_unlock();
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

    /*--------- Monitor related -------------------------*/
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
        int ps = exit_action_mutator_lock();
        try {
            if (ps == STATE.DONE.ordinal()) return false; // Too late.

            monitors.put(ref, target);
            return true;
        } finally {
            exit_action_mutator_unlock();
        }
    }

    /**
     * @param r
     */
    public boolean remove_monitor(ERef r, boolean flush) {
        int ps = exit_action_mutator_lock();
        try {
            if (ps == STATE.DONE.ordinal()) return false; // Too late.

            EHandle val = monitors.remove(r);
        } finally {
            exit_action_mutator_unlock();
        }

        if (flush) {
            // TODO: do we need to represent flush somehow?
        }
        return true; //TODO
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


    /*--------- Process state ---------------------------*/

    /**
     * @return
     */
    public boolean exists() {
        int ps = get_state_dirtyread();
        return ps == STATE.INIT.ordinal() || ps == STATE.RUNNING.ordinal();
    }

    /*--------- Termination related ---------------------*/
    /* (non-Javadoc)
	 * @see kilim.Task#checkKill()
	 */
    @Override
    public void checkKill() {
        check_exit();
    }

    /**
     * will check if this process have received an exit signal (and we're not
     * trapping)
     */
    public final void check_exit() {
        do_check_exit();

        if (print_trace != null) {
            Mailbox<EObject> mb = print_trace;
            if (mb != null) {

                H handle = self_handle();
                ESeq trace = new ErlangError(ERT.am_ok).getTrace();
                mb.putb(new ETuple2(handle, trace));

            }
            print_trace = null;
        }
    }


    /** because the above is called a bazillion times, we split this out to make it easier to inline */
    private void do_check_exit() throws ErlangExitSignal {
        if (exit_reason != null) {
            set_state_if_later(STATE.EXITING); // The state might be done, in case
                                               // this is called from a subsequent yield

            // Build and throw exception
            EObject reason = exit_reason;
            exit_reason = null;
            //System.err.println("---- [" + killer + "]");
            ErlangExitSignal e = new ErlangExitSignal(reason, killer);
            //e.printStackTrace();
            //System.err.println("---- [ ... ]");
            throw e;
        }
    }

    @SuppressWarnings("unchecked")
	protected void do_proc_termination(EObject exit_reason) throws Pausable {
        // Precondition: pstate is DONE, exit-action mutator count is zero.
        this.exit_reason = exit_reason;
		H me = self_handle();
		EAtom name = me.name;
		for (EHandle handle : linksref.get()) {
			try {
			handle.exit_signal(me, exit_reason, false);
			} catch (Error e) {
				log.severe("EXCEPTION IN EXIT HANDLER");
				log.log(Level.FINE, "details: ", e);
				throw e;
			} catch (RuntimeException e) {
				log.severe("EXCEPTION IN EXIT HANDLER");
				log.log(Level.FINE, "details: ", e);
				throw e;
			}
		}
		for (Map.Entry<ERef, EHandle> ent : monitors.entrySet()) {
			EHandle pid = ent.getValue();
			ERef ref = ent.getKey();

			pid.send_monitor_exit((EHandle)me, ref, exit_reason);
		}
		if (name != ERT.am_undefined && name != null) {
			ERT.unregister(name);
		}
	}
	

	public void send_monitor_exit(EHandle from, ERef ref, EObject reason) throws Pausable {
		ETuple2 pair = is_monitoring.get(ref);
		if (pair != null) {
			mbox_send(ETuple.make(am_DOWN, ref, am_process, pair.elem2, reason));
		}
	}


    /*--------- Mbox related ----------------------------*/

    /**
     * @return
     */
    public Mailbox<EObject> mbox() {
        return mbox;
    }

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

        int ps = get_state_dirtyread();
        if (ps == STATE.DONE.ordinal() ||
                ps == STATE.EXITING.ordinal() ||
                exit_reason != null)
        {
            // we have already received one exit signal, ignore
            // subsequent ones...
            return;
        }
        process_incoming_exit(from, reason, is_erlang_exit2);
    }

	protected abstract void process_incoming_exit(EHandle from, EObject reason, boolean is_erlang_exit2) throws Pausable
			;

    /*--------- Reduction count ------------------------*/

    public int get_reductions() { return reds;}

    public void bump_reductions(int amount) throws Pausable {
        reds += amount;
        if (reds > 1000) {
            reds = 0;
            Task.yield();
        }

    }

    /*--------- Process state locking -------------------*/

    /** Returns the state value.
     * Must be paired with exit_action_mutator_unlock! */
    protected final int exit_action_mutator_lock() {
        int v = pstate_and_mutator_count.getAndAdd(MUTATOR_COUNT_ONE);
        return v & PSTATE_MASK;
    }

    protected final void exit_action_mutator_unlock() {
        pstate_and_mutator_count.getAndAdd(-MUTATOR_COUNT_ONE);
    }

    protected final void set_state_to_done_and_wait_for_stability() throws Pausable {
        // Set state to DONE:
        int state_and_mutator_count = try_set_state(STATE.DONE);

        // Wait for any exit-action mutator count to drop to zero:
        int iter_count = 0;
        while ((state_and_mutator_count & MUTATOR_COUNT_MASK) != 0) {
            if ((++iter_count) % 16 == 0) Task.yield();
            state_and_mutator_count = pstate_and_mutator_count.get();
        }

        // Invariant: state is DONE and there are no exit-action mutators.
        assert pstate_and_mutator_count.get() == STATE.DONE.ordinal();
    }

    protected final void set_state(STATE new_state) {
        if (try_set_state(new_state) == -1) {
            System.err.println("Internal consistency error: bad process state transition to "+new_state+" ("+pstate_and_mutator_count.get()+")");
            // But go on regardless...
        }
    }

    protected final void set_state_if_later(STATE new_state) {
        try_set_state(new_state); // ignore any error result
    }

    /** Returns old state and mutator count (packed) - or (-1) if state transition is invalid. */
    private final int try_set_state(STATE new_state) {
        int old_value, new_value;
        int new_state_value = new_state.ordinal();
        do {
            old_value = pstate_and_mutator_count.get();
            int old_state_value = old_value & PSTATE_MASK;
            if (old_state_value >= new_state_value) return -1;
            new_value = (old_value & MUTATOR_COUNT_MASK) | new_state_value;
        } while (!pstate_and_mutator_count.compareAndSet(old_value, new_value));
        return old_value;
    }

    protected final int get_state_dirtyread() {
        return pstate_and_mutator_count.get() & PSTATE_MASK;
    }

    protected final int get_pstate_for_debug() {
        return pstate_and_mutator_count.get();
    }

    /*--------- Miscellaneous ---------------------------*/

    public void printStackTrace(Mailbox<EObject> info) {
        this.print_trace = info;
        this.resume();
    }

}
