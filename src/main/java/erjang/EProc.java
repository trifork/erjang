
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
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import kilim.Pausable;
import erjang.m.erlang.ErlFun;
import erjang.m.erlang.ErlProc;

/**
 * An erlang process
 */
public final class EProc extends ETask<EInternalPID> {
	
	static Logger log = Logger.getLogger(EProc.class.getName());
	
	public static final EObject TAIL_MARKER = new ETailMarker();
	
	public static final EAtom am_trap_exit = EAtom.intern("trap_exit");
	public static final EAtom am_sensitive = EAtom.intern("sensitive");
	public static final EAtom am_messages = EAtom.intern("messages");
	public static final EAtom am_message_queue_len = EAtom.intern("message_queue_len");
	public static final EAtom am_dictionary = EAtom.intern("dictionary");
	public static final EAtom am_group_leader = EAtom.intern("group_leader");
	public static final EAtom am_links = EAtom.intern("links");
	public static final EAtom am_heap_size = EAtom.intern("heap_size");
	public static final EAtom am_stack_size = EAtom.intern("stack_size");
	public static final EAtom am_reductions = EAtom.intern("reductions");
	public static final EAtom am_initial_call = EAtom.intern("initial_call");
	public static final EAtom am_current_function = EAtom.intern("current_function");
	public static final EAtom am_priority = EAtom.intern("priority");
	public static final EAtom am_memory = EAtom.intern("memory");
	public static final EAtom am_monitor_nodes = EAtom.intern("monitor_nodes");
	public static final EAtom am_registered_name = EAtom.intern("registered_name");

	public static final EAtom am_max = EAtom.intern("max");
	public static final EAtom am_normal = EAtom.intern("normal");
	public static final EAtom am_low = EAtom.intern("low");
	public static final EAtom am_high = EAtom.intern("high");

	static final EObject am_kill = EAtom.intern("kill");
	static final EObject am_killed = EAtom.intern("killed");

	private static final EObject am_status = EAtom.intern("status");
	private static final EObject am_waiting = EAtom.intern("waiting");
	private static final EObject am_running = EAtom.intern("running");
	private static final EObject am_runnable = EAtom.intern("runnable");

	private static final EAtom am_error_logger = EAtom.intern("error_logger");
	private static final EAtom am_info_report = EAtom.intern("info_report");

	private static final EObject am_DOWN = EAtom.intern("DOWN");
	private static final EObject am_noproc = EAtom.intern("noproc");

	public EFun tail;
	public EObject arg0, arg1, arg2, arg3, arg4, arg5, 
				   arg6, arg7, arg8, arg9, arg10, arg11, 
				   arg12, arg13, arg14, arg15, arg16, arg17;
	public ErlangException last_exception;

	private EInternalPID self;

	private EPID group_leader;

	private EAtom spawn_mod;

	private EAtom spawn_fun;

	private int spawn_args;

	// For interpreter use:
	public EObject[] stack = new EObject[10];
	public int sp = 0;
// 	double[] fregs = new double[16];
	public EDouble[] fregs = new EDouble[16];

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
		
		all_tasks.put(key(), this);
	}

	private int key() {
		int key = (self.serial() << 15) | (self.id() & 0x7fff);
		return key;
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
	private EAtom sensitive = ERT.FALSE;

	public int midx = 0;

	protected void link_failure(EHandle h) throws Pausable {
		if (trap_exit == ERT.TRUE || h.testLocalHandle()==null) {
			send_exit(h, ERT.am_noproc, false);
		} else {
			throw new ErlangError(ERT.am_noproc);
		}
	}

	static ExitHook[] NO_HOOKS = new ExitHook[0];
	
	@Override
	protected void do_proc_termination(EObject result) throws Pausable {
		
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
		
		super.do_proc_termination(result);

		self.done();
		
		all_tasks.remove(this.key());
		
		
	}
	
	protected void process_incoming_exit(EHandle from, EObject reason, boolean is_erlang_exit2) throws Pausable
	{
		if (pstate == STATE_EXIT_SIG || pstate == STATE_SENDING_EXIT) {
			if (log.isLoggable(Level.FINE)) {
				log.fine("Ignoring incoming exit reason="+reason+", as we're already exiting reason="+exit_reason);
			}
		}
		
		if (log.isLoggable(Level.FINE)) {
			log.log(Level.FINE, "incoming exit to "+this+" from "+from+" reason="+reason+"; is_exit2="+is_erlang_exit2);
		}
		
		if (is_erlang_exit2) {
			if (reason == am_kill) {
				exit_reason = am_killed;
				
			} else if (trap_exit == ERT.TRUE) {
				// we're trapping exits, so we in stead send an {'EXIT', from,
				// reason} to self
				ETuple msg = ETuple.make(ERT.am_EXIT, from, reason);
				// System.err.println("kill message to self: "+msg);
				
				mbox.put(msg);
				return;
				
			} else {
				this.exit_reason = reason;
			}
			
			this.pstate = STATE_EXIT_SIG;
			this.resume();
			return;
		}
		
		if (from == self_handle()) {
			return;
			
		} 

		if (reason == am_kill) {
			this.exit_reason = am_killed;
			this.pstate = STATE_EXIT_SIG;
			this.resume();

		} else if (trap_exit == ERT.TRUE) {
			// we're trapping exits, so we in stead send an {'EXIT', from,
			// reason} to self
			ETuple msg = ETuple.make(ERT.am_EXIT, from, reason);
			// System.err.println("kill message to self: "+msg);
			
			mbox.put(msg);
			
		} else if (reason != am_normal) {
			// System.err.println("kill signal: " +reason + " from "+from);
			// try to kill this thread
			this.exit_reason = reason;
			this.pstate = STATE_EXIT_SIG;
			this.resume();
		}
	}
	
	// private Thread runner;

	public EObject put(EObject key, EObject value) {
		EObject res = pdict.put(key, value);
		if (res == null)
			return ERT.am_undefined;
		return res;
	}

	public EObject get(EObject key) {
		EObject res = pdict.get(key);
		return (res == null) ? ERT.am_undefined : res;
	}

	/**
	 * @return list of the process dictionary
	 */
	public ESeq get() {
		ESeq res = ERT.NIL;
		for (Map.Entry<EObject, EObject> ent : pdict.entrySet()) {
			res = res.cons(ETuple.make(ent.getKey(), ent.getValue()));
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
			res = ERT.am_undefined;
		return res;
	}

	/**
	 * @return
	 */
    public EObject erase() {
		EObject res = get();
		pdict.clear();
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

	public ErlangException getLastException() {
		return last_exception;
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
	public EObject process_flag(EAtom flag, EObject value) {

		if (flag == am_trap_exit) {
			EAtom old = this.trap_exit;
			trap_exit = value.testBoolean();
			return ERT.box(old==ERT.TRUE);
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
		
		if (flag == am_monitor_nodes) {
			if (!value.isBoolean()) throw ERT.badarg(flag, value);
			boolean activate = value==ERT.TRUE;
 			Boolean old = EAbstractNode.monitor_nodes(self_handle(), activate, ERT.NIL);
			if (old == null) throw ERT.badarg(flag, value);
			return ERT.box(old.booleanValue());
		}

		if (flag == am_trap_exit) {
			EAtom old = this.trap_exit;
			trap_exit = value.testBoolean();
			return ERT.box(old==ERT.TRUE);
		}

		if (flag == am_sensitive) {
			EAtom old = this.sensitive;
			sensitive = value.testBoolean();
			return ERT.box(old==ERT.TRUE);
		}

		ETuple2 tup;
		if ((tup = ETuple2.cast(flag)) != null && tup.elem1==am_monitor_nodes) {
			ESeq opts = tup.elem2.testSeq();
			if (opts == null) throw ERT.badarg(flag, value);

			if (!value.isBoolean()) throw ERT.badarg(flag, value);
			boolean activate = value.testBoolean()==ERT.TRUE;

 			Boolean old = EAbstractNode.monitor_nodes(self_handle(), activate, opts);
			if (old == null) throw ERT.badarg(flag, value);
			return ERT.box(old.booleanValue());
		}

		throw new NotImplemented("process_flag flag="+flag+", value="+value);
	}

	@Override
	public void execute() throws Pausable {
		Throwable death = null;
		EObject result = null;
		try {

			try {
				this.check_exit();
				
//					synchronized(this) {
						this.pstate = STATE_RUNNING;
//					}
				
					boolean live = true;
					while (live) {
						try {
							while(this.tail.go(this) == TAIL_MARKER) {
								/* skip */
							}
						live = false;
					} catch (ErjangHibernateException e) {
						// noop, live = true //
					}
					
					if (live == true) {

						mbox_wait();

					}
				}
				 
				//System.out.println("proc "+this+" exited "+tmp);
				
				result = am_normal;

			} catch (NotImplemented e) {
				System.err.print("exiting "+self_handle()+" with: ");
				log.log(Level.SEVERE, "[fail] exiting "+self_handle(), e);
				result = e.reason();
				death = e;
				
			} catch (ErlangException e) {
				log.log(Level.FINE, "[erl] exiting "+self_handle(), e);
				last_exception = e;
				result = e.reason();
				death = e;

			} catch (ErlangExitSignal e) {
				log.log(Level.FINE, "[signal] exiting "+self_handle(), e);
				result = e.reason();
				death = e;
				
			} catch (Throwable e) {

				System.err.print("[java] exiting "+self_handle()+" with: ");
				e.printStackTrace();

				ESeq erl_trace = ErlangError.decodeTrace(e.getStackTrace());
				ETuple java_ex = ETuple.make(am_java_exception, EString
						.fromString(ERT.describe_exception(e)));

				result = ETuple.make(java_ex, erl_trace);
				death = e;

			} finally {
				// this.runner = null;
				this.pstate = STATE_DONE;
			}
			
			if (result != am_normal && monitors.isEmpty() && has_no_links() && !(death instanceof ErlangExitSignal)) {
					
					EFun fun = EModuleManager.resolve(new FunID(am_error_logger, am_info_report, 1));
					
					String msg = "Process " +self_handle()+ " exited abnormally without links/monitors\n"
						+ "exit reason was: " + result + "\n"
						+ (death == null ? "" : ERT.describe_exception(death));
					
					try {
						fun.invoke(this, new EObject[] { EString.fromString(msg) });
					} catch (ThreadDeath e) {
						throw e;
					} catch (Throwable e) {
						e.printStackTrace();
						// ignore //
					}

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
		
		res = res.cons(process_info(am_trap_exit));
		res = res.cons(process_info(am_messages));
		res = res.cons(process_info(am_message_queue_len));
		res = res.cons(process_info(am_dictionary));
		res = res.cons(process_info(am_group_leader));
		res = res.cons(process_info(am_links));
		res = res.cons(process_info(am_heap_size));
		res = res.cons(process_info(am_initial_call));
		res = res.cons(process_info(am_reductions));

		EObject reg_name = self_handle().name;
		if (reg_name != null)
			res = res.cons(new ETuple2(am_registered_name, reg_name));
		
		if (res == ERT.NIL) return ERT.am_undefined;
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
		} else if (spec == am_trap_exit) {
			return new ETuple2(am_trap_exit, trap_exit);
		} else if (spec == am_message_queue_len) {
			return new ETuple2(am_message_queue_len,
					   new ESmall(mbox.size()));
		} else if (spec == am_messages) {
			ESeq messages = EList.make((Object[])mbox.messages());
			return new ETuple2(am_messages, messages);
		} else if (spec == am_dictionary) {
			return new ETuple2(am_dictionary, get());
		} else if (spec == am_group_leader) {
			return new ETuple2(am_group_leader, group_leader);
		} else if (spec == am_links) {
			ESeq links = links();
			return new ETuple2(am_links, links);
		} else if (spec == am_status) {
			if (this.running) {
				return new ETuple2(am_status, am_running);
			} else if (this.pauseReason != null) {
				return new ETuple2(am_status, am_waiting);
			} else {
				return new ETuple2(am_status, am_runnable);
			}
		} else if (spec == am_heap_size) {
			return new ETuple2(am_heap_size, 
							   ERT.box(Runtime.getRuntime().totalMemory() 
									   - Runtime.getRuntime().freeMemory()));
		} else if (spec == am_stack_size) {
			// TODO: Maybe use HotSpotDiagnosticMXBean ThreadStackSize property?
			return new ETuple2(am_stack_size, 
							   ERT.box(0));
			
		} else if (spec == am_reductions) {
			return new ETuple2(am_reductions, ERT.box(this.reds));
			
		} else if (spec == am_initial_call) {
			return new ETuple2(am_initial_call, 
							   ETuple.make(spawn_mod, spawn_fun, ERT.box(spawn_args)));
			
		} else if (spec == am_current_function) {
			/** TODO: fix this so we return something meaningful... */
			return new ETuple2(am_current_function, 
							   ETuple.make(spawn_mod, spawn_fun, ERT.box(spawn_args)));
			
		} else if (spec == am_memory) {
			return new ETuple2(am_memory, ERT.box(50000));
			
		} else {
			System.err.println("NotImplemented: process_info("+spec+")");
			throw new NotImplemented("process_info("+spec+")");
		}
	}

	/* (non-Javadoc)
	 * @see kilim.Task#toString()
	 */
	@Override
	public String toString() {
		return self.toString() + super.toString() +
			"::" + spawn_mod + ":" + spawn_fun + "/" + spawn_args;
	}
	
	
	private	static ConcurrentHashMap<Integer,EProc> all_tasks 
		= new ConcurrentHashMap<Integer,EProc> ();
	
	/**
	 * @return
	 */
	public static ESeq processes() {
		ESeq res = ERT.NIL;
		for (EProc proc : all_tasks.values()) {
			if (proc.is_alive()) {
				res = res.cons(proc.self_handle());
			}
		}
		return res;
	}
	
	public static int process_count() {
		int count = 0;
		for (EProc proc : all_tasks.values()) {
			if (proc.is_alive()) {
				count += 1;
			}
		}
		return count;
	}



	/**
	 * @return
	 */
	public boolean is_alive() {
		int ps = pstate;
		return ps == STATE_INIT || ps == STATE_RUNNING;
	}

	List<ExitHook> exit_hooks = new ArrayList<ExitHook>();

	public long timeout_start;

	public boolean in_receive;

	public EObject[] regs;
	
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

	public Map<Integer, EAtom> getAtomCacheMap() {
		return null;
	}

	public static EInternalPID find(int id, int serial) {
		int key = (serial << 15) | (id & 0x7fff);
		EProc task = all_tasks.get(key);
		if (task != null) return task.self_handle();
		return null;
	}


	
	static {
		if (ErjangConfig.getBoolean("erjang.dump_on_exit"))
		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				System.err.println("===== LIVE TASKS UPON EXIT");
				for (EProc task : all_tasks.values()) {
					
					System.err.println("==" + task);
					System.err.println (task.fiber);
				}
				System.err.println("=====");				
			}
		});
	}



}

/** Eventually, EProc.TAIL_MARKER should just be NULL, but this may aid debugging */
class ETailMarker extends EPseudoTerm {

	@Override
	int compare_same(EObject rhs) {
		if (rhs == EProc.TAIL_MARKER)
			return 0;
		return -1;
	}

	@Override
	public int hashCode() { // Shouldn't be called.
		return 0;
	}
}
