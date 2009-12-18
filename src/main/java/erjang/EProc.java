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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import erjang.ETask.State;
import erjang.m.erlang.ErlProc;
import erjang.util.WeakHashSet;

import kilim.Mailbox;
import kilim.Pausable;

/**
 * An erlang process
 */
public final class EProc extends ETask<EInternalPID> {
	public static final EObject TAIL_MARKER = new ETailMarker();

	public static final EAtom am_trap_exit = EAtom.intern("trap_exit");
	public static final EAtom am_messages = EAtom.intern("messages");
	public static final EAtom am_message_queue_len = EAtom.intern("message_queue_len");
	public static final EAtom am_dictionary = EAtom.intern("dictionary");
	public static final EAtom am_group_leader = EAtom.intern("group_leader");
	public static final EAtom am_links = EAtom.intern("links");
	public static final EAtom am_priority = EAtom.intern("priority");
	public static final EAtom am_registered_name = EAtom.intern("registered_name");

	public static final EAtom am_max = EAtom.intern("max");
	public static final EAtom am_normal = EAtom.intern("normal");
	public static final EAtom am_low = EAtom.intern("low");
	public static final EAtom am_high = EAtom.intern("high");

	private static final EObject am_kill = EAtom.intern("kill");
	private static final EObject am_killed = EAtom.intern("killed");

	public EFun tail;
	public EObject arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8;

	private EInternalPID self;

	private EPID group_leader;

	private EAtom spawn_mod;

	private EAtom spawn_fun;

	private int spawn_args;

	/**
	 * @param m
	 * @param f
	 * @param array
	 */
	public EProc(EPID group_leader, EAtom m, EAtom f, ESeq a) {
		self = new EInternalPID(this);

		// if no group leader is given, we're our own group leader
		this.group_leader = group_leader == null ? self : group_leader;
		this.spawn_mod = m;
		this.spawn_fun = f;
		this.spawn_args = a.length();
		
		int arity = spawn_args;
		EFun target = EModuleManager.resolve(new FunID(m,f,arity));
		
		if (target == null) {
			throw new ErlangUndefined(m, f, new ESmall(arity));
		}
		
		this.tail = target;
		a = a.reverse();
		switch (arity) {
		default:
			throw new NotImplemented();
		case 7: 
			this.arg6 = a.head(); a = a.tail();
		case 6: 
			this.arg5 = a.head(); a = a.tail();
		case 5: 
			this.arg4 = a.head(); a = a.tail();
		case 4: 
			this.arg3 = a.head(); a = a.tail();
		case 3: 
			this.arg2 = a.head(); a = a.tail();
		case 2: 
			this.arg1 = a.head(); a = a.tail();
		case 1: 
			this.arg0 = a.head(); a = a.tail();
		case 0:
		}
		
		all_tasks.add(this);
	}

	/**
	 * @return
	 */
	public EInternalPID self_handle() {
		return self;
	}

	/**
	 * @param key
	 * @param value
	 * @return
	 */

	Map<EObject, EObject> pdict = new HashMap<EObject, EObject>();

	private EAtom trap_exit = ERT.FALSE;

	public int midx;

	protected void link_failure(EHandle h) throws Pausable {
		if (trap_exit == ERT.TRUE || h.testLocalHandle()==null) {
			send_exit(h, ERT.am_noproc);
		} else {
			throw new ErlangError(ERT.am_noproc);
		}
	}

	static ExitHook[] NO_HOOKS = new ExitHook[0];
	
	@Override
	protected void do_proc_termination(EObject result) throws Pausable {
		super.do_proc_termination(result);
		
		ExitHook[] hooks = NO_HOOKS;
		synchronized (exit_hooks) {
			if (exit_hooks == null || exit_hooks.isEmpty()) {
				// do nothing //
			} else {
				hooks = exit_hooks.toArray(new ExitHook[exit_hooks.size()]);
			}
		}
		
		for (int i = 0; i < hooks.length; i++) {
			hooks[i].on_exit(self);
		}
		

	}
	
	protected void process_incoming_exit(EHandle from, EObject reason) throws Pausable
	{
		if (from == self_handle()) {
			return;
			
		} 

		if (reason == am_kill) {
			this.exit_reason = am_killed;
			this.pstate = State.EXIT_SIG;
			this.resume();

		} else if (trap_exit == ERT.TRUE) {
			// we're trapping exits, so we in stead send an {'EXIT', from,
			// reason} to self
			ETuple msg = ETuple.make(ERT.EXIT, from, reason);
			// System.err.println("kill message to self: "+msg);
			mbox_send(msg);
			
		} else if (reason != am_normal) {
			// System.err.println("kill signal: " +reason + " from "+from);
			// try to kill this thread
			this.exit_reason = reason;
			this.pstate = State.EXIT_SIG;
			this.resume();
		}
	}
	
	// private Thread runner;

	public EObject put(EObject key, EObject value) {
		EObject res = pdict.put(key, value);
		if (res == null)
			return ERT.NIL;
		return res;
	}

	public EObject get(EObject key) {
		EObject res = pdict.get(key);
		return (res == null) ? ERT.NIL : res;
	}

	/**
	 * @return list of the process dictionary
	 */
	public ESeq get() {
		ESeq res = ERT.NIL;
		for (Map.Entry<EObject, EObject> ent : pdict.entrySet()) {
			res.cons(ETuple.make(ent.getKey(), ent.getValue()));
		}
		return res;
	}

	/**
	 * @param key
	 * @return
	 */
	public EObject erase(EObject key) {
		EObject res = pdict.remove(key);
		if (res == null)
			res = ERT.NIL;
		return res;
	}

	/**
	 * @return
	 */
	public EPID group_leader() {
		return group_leader;
	}

	/**
	 * Only called from ELocalPID
	 * 
	 * @param group_leader
	 */
	void set_group_leader(EPID group_leader) {
		this.group_leader = group_leader;
	}

	/**
	 * @return
	 */
	public ELocalNode getLocalNode() {
		return ERT.getLocalNode();
	}

	static final EAtom[] priorities = new EAtom[] {
		EAtom.intern("max"),
		EAtom.intern("high"),
		EAtom.intern("normal"),
		EAtom.intern("low"),
	};

	/**
	 * @param testAtom
	 * @param a2
	 * @return
	 * @throws Pausable 
	 */
	public EObject process_flag(EAtom flag, EObject value) throws Pausable {

		if (flag == am_trap_exit) {
			EAtom old = this.trap_exit;
			trap_exit = value.testBoolean();
			return old;
		}

		if (flag == am_priority) {
			EAtom old = priorities[getPriority()];
			for (int i = 0; i < priorities.length; i++) {
				if (value == priorities[i]) {
					setPriority(i);
					return old;
				}
			}
			throw ERT.badarg(flag, value);
		}
		
		throw new NotImplemented("process_flag flag="+flag+", value="+value);
	}

	@Override
	public void execute() throws Pausable {
		try {

			EObject result = null;
			try {
				this.pstate = State.RUNNING;

				EObject tmp;
				while((tmp = this.tail.go(this)) == TAIL_MARKER) {
					/* skip */
				}
				 
				//System.out.println("proc "+this+" exited "+tmp);
				
				result = am_normal;

			} catch (ErlangException e) {
				System.err.print("exiting "+self_handle()+" with: ");
				e.printStackTrace(System.err);
				result = e.reason();

			} catch (ErlangExitSignal e) {
				System.err.print("exiting "+self_handle()+" with: ");
				e.printStackTrace(System.err);
				result = e.reason();

			} catch (Throwable e) {

				System.err.print("exiting "+self_handle()+" with: ");
				e.printStackTrace();

				ESeq erl_trace = ErlangError.decodeTrace(e.getStackTrace());
				ETuple java_ex = ETuple.make(am_java_exception, EString
						.fromString(ERT.describe_exception(e)));

				result = ETuple.make(java_ex, erl_trace);

			} finally {
				// this.runner = null;
				this.pstate = State.DONE;
			}

			//System.err.println("task "+this+" exited with "+result);
			
			do_proc_termination(result);

		} catch (ThreadDeath e) {
			throw e;
			
		} catch (Throwable e) {
			e.printStackTrace();
		}

	}

	/**
	 * @return
	 */
	public EObject process_info() {
		
		ESeq res = ERT.NIL;
		
		res = res.cons(new ETuple2(am_trap_exit, trap_exit));
		
		// get messages
		ESeq messages = EList.make((Object[])mbox.messages());
		res = res.cons(new ETuple2(am_messages, messages));
		res = res.cons(new ETuple2(am_message_queue_len, new ESmall(messages.length())));
		
		res = res.cons(new ETuple2(am_dictionary, get()));
		
		res = res.cons(new ETuple2(am_group_leader, group_leader));

		res = res.cons(new ETuple2(am_registered_name, self_handle().name));

		ESeq links = links();
		res = res.cons(new ETuple2(am_links, group_leader));
		
		
		if (res == ERT.NIL) return ERT.am_undefined;
		return res;
	}

	/**
	 * @return
	 */
	private ESeq links() {
		ESeq res = ERT.NIL;
		for (EHandle h : super.links) {
			res = res.cons(h);
		}
		return res;
	}

	/**
	 * @param spec
	 * @return
	 */
	public EObject process_info(EObject spec) {
		
		if (spec == am_registered_name) {
			return self_handle().name == null 
				? ERT.NIL 
				: new ETuple2(am_registered_name, self_handle().name);
		}
		
		System.err.println(spec);
		throw new NotImplemented();
	}

	/* (non-Javadoc)
	 * @see kilim.Task#toString()
	 */
	@Override
	public String toString() {
		return self.toString() + super.toString() + "::" + spawn_mod + ":" + spawn_fun + "/" + spawn_args; 
	}
	
	// this is not synchronized, as we only mess with it from this proc
	Map<ERef,EHandle> is_monitoring = new HashMap<ERef, EHandle>();

	/**
	 * @param selfHandle
	 * @param object
	 * @return
	 */
	public ERef monitor(EHandle handle, EObject object) {
		ERef ref = handle.add_monitor(self_handle(), object);
		this.is_monitoring.put(ref, handle);
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
		
		if (flush) {
			// TODO: wait for flush ack?
		}
		
	}
	
	
	private	static WeakHashSet<EProc> all_tasks = new WeakHashSet<EProc>();
	
	/**
	 * @return
	 */
	public static ESeq processes() {
		ESeq res = ERT.NIL;
		for (EProc proc : all_tasks) {
			if (proc.is_alive()) {
				res = res.cons(proc.self_handle());
			}
		}
		return res;
	}

	/**
	 * @return
	 */
	public boolean is_alive() {
		State ps = pstate;
		return ps == State.INIT || ps == State.RUNNING;
	}

	List<ExitHook> exit_hooks = new ArrayList<ExitHook>();
	
	/**
	 * @param hook
	 */
	public void add_exit_hook(ExitHook hook) {
		synchronized(exit_hooks) {
			exit_hooks.add(hook);
		}
	}

	/**
	 * @param hook
	 */
	public void remove_exit_hook(ExitHook hook) {
		synchronized(exit_hooks) {
			exit_hooks.remove(hook);
		}
	}
	
	



}

class ETailMarker extends EObject {

	@Override
	int cmp_order() {
		return CMP_ORDER_ERJANG_INTERNAL;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.EObject#compare_same(erjang.EObject)
	 */
	@Override
	int compare_same(EObject rhs) {
		if (rhs == EProc.TAIL_MARKER)
			return 0;
		return -1;
	}
}