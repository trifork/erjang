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

import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryMXBean;
import java.lang.management.MemoryManagerMXBean;
import java.lang.management.MemoryPoolMXBean;
import java.lang.management.MemoryType;
import java.lang.management.MemoryUsage;
import java.util.List;

import erjang.BIF;
import erjang.EAtom;
import erjang.ECons;
import erjang.EModule;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.EProc;
import erjang.ERT;
import erjang.ERef;
import erjang.ESmall;
import erjang.EString;
import erjang.ETuple;
import erjang.NotImplemented;

/**
 * 
 */
public class ErlProc {

	private static final EAtom am_smp_support = EAtom.intern("smp_support");
	private static final EAtom am_threads = EAtom.intern("threads");
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
	private static final EObject am_otp_version = EAtom.intern("am_otp_version");

	@BIF
	public static EObject display(EProc proc, EObject obj) {
		System.out.println(obj);
		return obj;
	}

	@BIF
	public static EObject erase(EProc proc, EObject key) {
		return proc.erase(key);
	}

	@BIF
	public static EObject register(EProc proc, EObject name, EObject pid) {
		EAtom aname;
		if ((aname=name.testAtom()) == null) throw ERT.badarg(name, pid);
		EPort pport = null;
		EPID ppid;
		if (((ppid=pid.testPID()) == null )
				&& ((pport=pid.testPort()) == null)) 
			throw ERT.badarg(name, pid);
		ERT.register(aname, pport==null?ppid:pport);
		return ERT.TRUE;
	}

	@BIF
	public static EObject unlink(EProc proc, EObject pid) {
		throw new NotImplemented();
	}

	@BIF
	public static EObject exit(EProc proc, EObject a1, EObject a2) {
		throw new NotImplemented();
	}

	@BIF
	public static EObject exit(EProc proc, EObject a1) {
		throw new NotImplemented();
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

	static EAtom am_allocator = EAtom.intern("allocator");
	static EAtom am_heap_type = EAtom.intern("heap_type");
	static EAtom am_shared = EAtom.intern("shared");

	@BIF
	static EObject system_info(EProc proc, EObject type) {

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
			return new ESmall(8);
			
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
			// TODO: be smarter somehow
			return new EString("5.7.3");
			
		} else if (type == am_otp_version) {
			// TODO: be smarter somehow
			return new EString("R13B");
			
		} else if (type == am_wordsize) {
			return new ESmall(32);
			
		} else {
			return am_undefined;
		}

	}
	
	@BIF
	static EObject function_exported(EObject mod, EObject fun, EObject arity) {
		EAtom m = mod.testAtom();
		EAtom f = fun.testAtom();
		ESmall a = arity.testSmall();
		
		if (m==null||f==null||a==null) throw ERT.badarg(mod,fun,arity);
		
		return ERT.box( EModule.function_exported (m,f,a.value) );
	}
}
