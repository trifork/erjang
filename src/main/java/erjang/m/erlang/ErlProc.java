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

package erjang.m.erlang;

import java.lang.management.ManagementFactory;
import java.lang.management.MemoryMXBean;
import java.lang.management.MemoryPoolMXBean;
import java.lang.management.MemoryType;
import java.lang.management.MemoryUsage;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import kilim.Pausable;
import erjang.BIF;
import erjang.EAbstractNode;
import erjang.EAtom;
import erjang.ECons;
import erjang.EFun;
import erjang.EHandle;
import erjang.EInternalPID;
import erjang.EModuleManager;
import erjang.EObject;
import erjang.EPID;
import erjang.EPeer;
import erjang.EProc;
import erjang.ERT;
import erjang.ERef;
import erjang.ESeq;
import erjang.ESmall;
import erjang.EString;
import erjang.ETask;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ErjangHibernateException;
import erjang.ErlangException;
import erjang.ErlangExit;
import erjang.ErlangUndefined;
import erjang.FunID;
import erjang.Import;
import erjang.Main;
import erjang.NotImplemented;
import erjang.OTPMain;

/**
 * 
 */
public class ErlProc {
	
	private static Logger log = Logger.getLogger(ErlProc.class.getName());

	private static final EAtom am_smp_support = EAtom.intern("smp_support");
	private static final EAtom am_schedulers = EAtom.intern("schedulers");
	private static final EAtom am_break_ignored = EAtom.intern("break_ignored");
	private static final EAtom am_threads = EAtom.intern("threads");
	public static final EAtom am_process = EAtom.intern("process");
	private static final EAtom am_wordsize = EAtom.intern("wordsize");
	private static final EAtom am_thread_pool_size = EAtom
			.intern("thread_pool_size");
	private static final EAtom am_os_type = EAtom.intern("os_type");
	private static final EAtom am_win32 = EAtom.intern("win32");
	private static final EAtom am_unix = EAtom.intern("unix");
	private static final EAtom am_version = EAtom.intern("version");
	private static final EAtom am_undefined = EAtom.intern("undefined");
	private static final EAtom am_heap = EAtom.intern("heap");
	private static final EAtom am_non_heap = EAtom.intern("non_heap");
	private static final EAtom am_jvm = EAtom.intern("jvm");
	private static final EAtom am_allocated_areas = EAtom.intern("allocated_areas");
	private static final EAtom am_otp_release = EAtom.intern("otp_release");
	private static final EAtom am_driver_version = EAtom.intern("driver_version");
	private static final EAtom am_global_heaps_size = EAtom.intern("global_heaps_size");
	private static final EAtom am_process_count = EAtom.intern("process_count");
	private static final EAtom am_system_architecture = EAtom.intern("system_architecture");
	private static final EAtom am_logical_processors = EAtom.intern("logical_processors");
	private static final EAtom am_hipe_architecture = EAtom.intern("hipe_architecture");
	private static final EAtom am_machine = EAtom.intern("machine");
	private static final EAtom am_link = EAtom.intern("link");
	private static final EAtom am_monitor = EAtom.intern("monitor");
	private static final EAtom am_priority = EAtom.intern("priority");
	private static final EAtom am_system_version = EAtom.intern("system_version");
	public static final EAtom am_flush = EAtom.intern("flush");
	public static final EAtom am_shutdown = EAtom.intern("shutdown");
	private static final EAtom am_ets_alloc = EAtom.intern("ets_alloc");
	private static final EAtom am_error_checker = EAtom.intern("error_checker");
	private static final EAtom am_debug_compiled = EAtom.intern("debug_compiled");
	private static final EAtom am_lock_checking = EAtom.intern("lock_checking");
	private static final EAtom am_compat_rel = EAtom.intern("compat_rel");

	@BIF
	public static EObject process_info(EObject pid, EObject what) {
		EPID p = pid.testPID();
		if (p == null) throw ERT.badarg(pid,what);
		// TODO: validate WHAT locally before going remote?
		return p.process_info(what);
	}

	@BIF
	public static EObject process_info(EObject pid) {
		EPID p = pid.testPID();
		if (p == null) throw ERT.badarg(pid);
		return p.process_info();
	}

	@BIF
	public static EObject display(EProc proc, EObject obj) {
		System.out.println(obj);
		return ERT.TRUE;
	}

	@BIF
	public static ESeq get_stacktrace(EProc proc) {
	    ErlangException ex = proc.getLastException();
	    return ex != null ? ex.getTrace() : ERT.NIL;
	}

	@BIF
	public static EObject erase(EProc proc, EObject key) {
		return proc.erase(key);
	}

	@BIF
	public static EObject erase(EProc proc) {
		return proc.erase();
	}

	@BIF
	public static EObject register(EObject name, EObject pid) {
		EAtom aname;
		EHandle handle = pid.testHandle();
		if ((aname=name.testAtom()) == null
				|| handle == null) throw ERT.badarg(name, pid);
		ERT.register(aname, handle);
		return ERT.TRUE;
	}

	
	@BIF
	public static EObject unregister(EObject name) {
		EAtom aname;
		if ((aname=name.testAtom()) == null
			|| !ERT.unregister(aname)) throw ERT.badarg(name);
		return ERT.TRUE;
	}

	
	
	@BIF
	public static EObject spawn_link(EProc proc, EObject mod, EObject fun, EObject args) throws Pausable {
		
		EAtom m = mod.testAtom();
		EAtom f = fun.testAtom();
		ESeq  a = args.testSeq();
		
		if (m==null||f==null||a==null) 
			throw ERT.badarg(mod, fun, args);
		
		EProc p2 = new EProc(proc.group_leader(), m, f, a);
		
		p2.link_to(proc);
		
		ERT.run(p2);
		
		return p2.self_handle();
	}

	@BIF
	public static EObject spawn_opt(EProc self, EObject tup) throws Pausable {
		ETuple t;
		EAtom m;
		EAtom f;
		ESeq a;
		ESeq o;
		if ((t=tup.testTuple()) == null 
				|| t.arity() != 4
				|| (m=t.elm(1).testAtom()) == null
				|| (f=t.elm(2).testAtom()) == null
				|| (a=t.elm(3).testSeq()) == null
				|| (o=t.elm(4).testSeq()) == null
				) throw ERT.badarg(tup); 
		
		boolean link = false;
		boolean monitor = false;
		EAtom priority = null;
		
		for (; !o.isNil(); o = o.tail() ) {
			EObject val = o.head();
			
			ETuple2 t2;
			if (val == am_link) {
				link = true;
			} else if (val == am_monitor) {
				monitor = true;
			} else if ((t2 = ETuple2.cast(val)) != null) {
				
				if (t2.elm(1) == am_priority) {
					EAtom am = t2.elm(2).testAtom();
					if (am != null)
						priority = am;
				}
				
				// ignore full_sweep_after and min_heap_size
			}
			
		}
		

		EProc p2 = new EProc(self.group_leader(), m, f, a);
		
		if (link) {
			p2.link_to(self);
		}
		
		if (priority != null) {
			// may throw badarg!
			p2.process_flag(am_priority, priority);
		}
		
		ERef ref = null;
		if (monitor) {
			ref = ERT.getLocalNode().createRef();
			
			if (!self.monitor(p2.self_handle(), p2.self_handle(), ref)) {
				throw new InternalError("cannot monitor new process?");
				// self.mbox_send(ETuple.make(ERT.am_DOWN, ref, p2.self_handle(), ERT.am_noproc));
			}

		}
		
		ERT.run(p2);
		
		if (monitor) {
			return new ETuple2(p2.self_handle(), ref);
		} else {
			return p2.self_handle();
		}

	}

	@BIF
	public static EObject spawn(EProc proc, EObject mod, EObject fun, EObject args) {
		
		EAtom m = mod.testAtom();
		EAtom f = fun.testAtom();
		ESeq  a = args.testSeq();
		
		if (m==null||f==null||a==null) 
			throw ERT.badarg(mod, fun, args);
		
		EProc p2 = new EProc(proc.group_leader(), m, f, a);
				
		ERT.run(p2);
		
		return p2.self_handle();
	}
	
	@BIF
	public static EObject halt(EProc proc) {
		System.exit(0);
		return null;
	}
	
	@BIF
	public static EObject halt(EProc proc, EObject value) {
		ESmall val = value.testSmall();
		if (val != null) {
			System.exit(val.value);
			return null;
		}
		
		EString str = value.testString();
		if (str != null) {		
			// TODO: create crash file
			System.err.println(str.stringValue());
			System.exit(1);
		}
		
		throw ERT.badarg(value);
		

	}
	
	@BIF
	public static EObject unlink(EProc self, EObject pid) throws Pausable {
		EHandle h = EHandle.cast(pid);
		if (h != null) {
			self.unlink(h);
		}
		return pid;
	}
	
	@BIF
	static public EObject link(EProc self, EObject pid) throws Pausable {
		EHandle h = EHandle.cast(pid);
		if (h == null) throw ERT.badarg(pid);
		self.link_to(h);
		return ERT.TRUE;
	}

	@BIF
	static public EObject monitor(EProc self, EObject how, EObject object) throws Pausable {
		if (how != am_process)
			throw ERT.badarg(how, object);

		// case 1: object is a PID
		EHandle h = EHandle.cast(object);
		if (h != null) 
		{   
			ERef ref = ERT.getLocalNode().createRef();
			if (!self.monitor(h, h, ref)) {
				self.mbox_send(ETuple.make(ERT.am_DOWN, ref, am_process, object, ERT.am_noproc));
			}
			return ref;
		}

		// case 2: object is a name
		EAtom name;
		if (h == null && (name=object.testAtom()) != null) {
			ERef ref = ERT.getLocalNode().createRef();
			boolean success = false;
			
			object = new ETuple2(name, ErlDist.node());
			
			if ((h = ERT.whereis(name).testHandle()) != null) 
			{   
				success = self.monitor(h, object, ref);
			}	
			
			if (!success) {
				self.mbox_send(ETuple.make(ERT.am_DOWN, ref, am_process, object, ERT.am_noproc));
			}

			return ref;
		}

		// case 3: object is {name, node}
		ETuple tup;
		EAtom node;
		if ((tup=object.testTuple()) != null
			&& tup.arity()==2
			&& (name=tup.elm(1).testAtom()) != null
			&& (node=tup.elm(2).testAtom()) != null) 
		{
			if (node == ErlDist.node()) {

				ERef ref = ERT.getLocalNode().createRef();
				boolean success = false;
				
				if ((h = ERT.whereis(name).testHandle()) != null) 
				{   
					success = self.monitor(h, object, ref);
				}	
				
				if (!success) {
					self.mbox_send(ETuple.make(ERT.am_DOWN, ref, am_process, object, ERT.am_noproc));
				}

				return ref;
				
			} else {
				
				EPeer peer = (EPeer) EPeer.get(node);
				if (peer != null) {
					ERef ref = ERT.getLocalNode().createRef();
					self.monitor(tup, ref);
					peer.dsig_monitor(self.self_handle(), name, ref);
					return ref;
				}

				return ErlDist.dmonitor_p2_trap.invoke(self, new EObject[] {how, object});
			}
		}
		
		throw ERT.badarg(how, object);
		
	}

	@Import(module="erlang", fun="flush_monitor_message", arity=2)
	static EFun flush_monitor_message = null;

	@BIF
	static public EObject demonitor(EProc self, EObject ref) throws Pausable {
	    return demonitor(self, ref, ERT.NIL);
	}

	/* TODO: Split option parsing from the action; used the action
	 * part more directly in demonitor/1.
	 * TODO: Support the 'info' option.
	 */
	@BIF
	static public EObject demonitor(EProc self, EObject ref, EObject options) throws Pausable {
		return demonitor((ETask)self, ref, options);
	}

	static public EObject demonitor(ETask self, EObject ref, EObject options) throws Pausable {
		ERef r = ref.testReference();
		
		ESeq o = options.testSeq();
		
		if (r==null||o==null)
			throw ERT.badarg(ref, options);

		boolean flush = (!o.isNil() && o.head()==am_flush);

		EObject found = self.demonitor(r);

		if (found == null) {
			return ERT.FALSE;
		}
		
		EHandle h;
		ETuple tup;
		EAtom name;
		EAtom node;
		if ((h=found.testHandle()) != null) {
			h.remove_monitor(self.self_handle(), r, flush);
		} else if ((tup=found.testTuple()) != null 
					&& tup.arity()==2
					&& (name=tup.elm(1).testAtom()) != null
					&& (node=tup.elm(2).testAtom()) != null) {
			
			EAbstractNode n = EAbstractNode.get_or_connect(self, node);
			if (n != null) {
				n.dsig_demonitor(self.self_handle(), r, name);
			}
		}

		
		if (flush && (self instanceof EProc)) {
			flush_monitor_message.invoke((EProc) self, new EObject[] {ref, ERT.am_ok});
		}
		
		return ERT.TRUE;
	}

	@BIF
	public static EAtom exit(EProc proc, EObject p, EObject reason) throws Pausable {
		
//		System.err.println(proc.self_handle() + ":: erlang:exit(" + p + ", " + reason + ")");
		
		EHandle pid = p.testHandle();
		
		if (pid == null) 
			throw ERT.badarg(p, reason);
	
		if (pid == proc.self_handle()) {
			throw new ErlangExit(reason);
		}
		
		pid.exit_signal(proc.self_handle(), reason, true);
		
		return ERT.TRUE;
	}

	@BIF
	public static EObject exit(EObject a1) {
		throw new ErlangExit(a1);
	}

	@BIF
	public static ERef make_ref(EProc proc)
	{
		return ERT.getLocalNode().createRef();
	}
	
	@BIF
	static EObject group_leader(EProc proc) {
		return proc.group_leader();
	}

	@BIF
	static EObject group_leader(EObject group_leader, EObject pid)
	{
		EPID p = pid.testPID();
		EPID gl = group_leader.testPID();
		
		if (p==null || gl==null) throw ERT.badarg(group_leader, pid);
		
		p.set_group_leader(gl);
		
		return ERT.TRUE;
	}
	
	
	static EAtom am_allocator = EAtom.intern("allocator");
	static EAtom am_heap_type = EAtom.intern("heap_type");
	static EAtom am_shared = EAtom.intern("shared");

	@BIF
	static EObject system_info(EProc proc, EObject type) {

		if (type == am_machine) {
			// we report BEAM so that the compiler emits BEAM files
			return EString.fromString("BEAM");
		} else if (type == am_smp_support) {
			return ERT.TRUE;
		} else if (type == am_schedulers) {
			return ERT.box(ERT.threadPoolSize());
		} else if (type == am_threads) {
			return ERT.box(true);
		} else if (type == am_thread_pool_size) {
			return ERT.box(ERT.asyncThreadPoolSize());
		} else if (type == am_break_ignored) {
			return ERT.box(false);
		} else if (type == am_compat_rel) {
		    // we return same value as R14
		    return new ESmall(14);
		}
		
		ETuple2 tup;
		if (type == am_allocated_areas) {

			ECons res = ERT.NIL;

			List<MemoryPoolMXBean> bean2 = ManagementFactory
					.getMemoryPoolMXBeans();

			if (bean2 == null) {

				MemoryMXBean bean = ManagementFactory.getMemoryMXBean();
				if (bean != null) {

					MemoryUsage mu = bean.getHeapMemoryUsage();
					res = res.cons(ETuple.make(am_heap, ERT.box(mu.getCommitted()),
							ERT.box(mu.getUsed())));

					mu = bean.getNonHeapMemoryUsage();
					res = res.cons(ETuple.make(am_non_heap, ERT.box(mu
							.getCommitted()), ERT.box(mu.getUsed())));

				}
				
				return res;
			}
			
			for (MemoryPoolMXBean mb : bean2) {
				
				String name = mb.getName();
				MemoryUsage mu = mb.getUsage();
				if (mu == null) continue;
				
				String name2 = (mb.getType()==MemoryType.HEAP ? "heap:" : "non_heap:" ) + name;
				
				res = res.cons(ETuple.make(EAtom.intern(name2), ERT.box(mu
						.getCommitted()), ERT.box(mu.getUsed())));

				
			}
			
			return res;

		} else if (type == am_allocator) {
			return am_jvm;			
			
		} else if (type == am_heap_type) {
			return am_shared;
			
		} else if (type == am_smp_support) {
			return ERT.TRUE;
			
		} else if (type == am_thread_pool_size) {
			
			// TODO: hook up to thread pool
			return new ESmall(ERT.threadPoolSize());
			
		} else if (type == am_os_type) {
			String os = System.getProperty("os.name");
			if (os.startsWith("Windows")) {
				return ETuple.make(am_win32, new EString(os));
			} else {
				return ETuple.make(am_unix, new EString(os));
			}

		} else if (type == am_threads) {
			return ERT.TRUE;
			
		} else if (type == am_version) {
			return EString.fromString( erjang.Main.erts_version().substring("erts-".length()) );
			
		} else if (type == am_otp_release) {
			// TODO: be smarter somehow
			return new EString(Main.OTP_VERSION);
			
		} else if (type == am_logical_processors) {
			// TODO: be smarter somehow
			return ERT.box(Runtime.getRuntime().availableProcessors());
			
		} else if (type == am_global_heaps_size) {
			return ERT.box(Runtime.getRuntime().totalMemory());
			
		} else if (type == am_process_count) {
			return ERT.box(EProc.process_count());
			
		} else if (type == am_system_architecture) {
			return new EString(Main.SYSTEM_ARCHITECTURE);
			
		} else if (type == am_driver_version) {
			// TODO: be smarter somehow
			return new EString(Main.DRIVER_VERSION);
			
		} else if (type == am_wordsize) {
			return new ESmall(4);
			 
		} else if (type == am_debug_compiled || type == am_lock_checking) {
			 throw ERT.badarg(type);
			 
		} else if (type == am_hipe_architecture) {
			return am_undefined;
			
		} else if (type == am_system_version) {
			return new EString("Erjang ["+ erjang.Main.erts_version()+"]");
			
		} else if ((tup=ETuple2.cast(type)) != null) {
			
			if (tup.elem1 == am_allocator) {
				if (tup.elem2 == am_ets_alloc) {
					return ERT.FALSE;
				}
			} else if (tup.elem1 == am_error_checker) {
				throw ERT.badarg(type);
			}

			return am_undefined;

		} else {
			log.info("erlang:system_info("+type+") unknown");
			throw ERT.badarg(type);
		}

	}
	
	@BIF
	static EAtom module_loaded(EObject mod) {
		EAtom m;
		if ((m=mod.testAtom()) == null) throw ERT.badarg(mod);
		return EModuleManager.module_loaded(m) ? ERT.TRUE : ERT.FALSE;
	}
	
	@BIF
	static ESeq processes() {
		return EProc.processes();
	}
	
	@BIF
	public static EAtom is_process_alive(EObject p) {
		EPID pid = p.testPID();
		if (pid == null) throw ERT.badarg(p);
		return ERT.box(pid.is_alive());
	}
	
	@BIF
	public static EObject suspend_process(EObject a1, EObject a2) {
		throw new NotImplemented();
	}
	
	@BIF
	public static EAtom check_process_code(EObject pid_arg, EObject mod_arg) {
		EPID pid = pid_arg.testPID();
		EAtom mod = mod_arg.testAtom();
		
		if (pid == null || mod == null) { throw ERT.badarg(pid_arg, mod_arg); }
		
		log.log(Level.FINE, "check_process_code not implemented (" + pid + ", " + mod + ")");
	
		return ERT.FALSE;
	}
	
	@BIF
	public static EAtom purge_module(EObject mod_arg) {
		
		log.log(Level.FINE, "purge_module not implemented (" + mod_arg + ")");

		return ERT.TRUE;
	}
	
	@BIF
	public static EObject hibernate(EProc self, EObject a1, EObject a2, EObject a3) {
		
		EAtom m = a1.testAtom();
		EAtom f = a2.testAtom();
		ESeq  a = a3.testSeq();
		
		if (m == null || f == null || a == null) {
			throw ERT.badarg(a1, a2, a3);
		}
		
		int arity = a.length();
		
		EFun target = EModuleManager.resolve(new FunID(m,f,arity));
		
		if (target == null) {
			throw new ErlangUndefined(m, f, new ESmall(arity));
		}
		
		self.tail = target;
		a = a.reverse();
		switch (arity) {
		default:
			throw new NotImplemented("hibernate w/" + arity + " args");
		case 7: 
			self.arg6 = a.head(); a = a.tail();
		case 6: 
			self.arg5 = a.head(); a = a.tail();
		case 5: 
			self.arg4 = a.head(); a = a.tail();
		case 4: 
			self.arg3 = a.head(); a = a.tail();
		case 3: 
			self.arg2 = a.head(); a = a.tail();
		case 2: 
			self.arg1 = a.head(); a = a.tail();
		case 1: 
			self.arg0 = a.head(); // a = a.tail();
		case 0:
		}		
		
		throw ErjangHibernateException.INSTANCE;
		
	}
	
	
}
