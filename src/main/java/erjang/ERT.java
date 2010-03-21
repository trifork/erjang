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

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import kilim.Pausable;
import kilim.Task;
import erjang.beam.Compiler;
import erjang.driver.Drivers;
import erjang.driver.EAsync;
import erjang.driver.EDriver;
import erjang.driver.EDriverTask;

@Module(value = "erlang")
public class ERT {

	static Logger log = Logger.getLogger("erjang");
	public static EAtom am_badsig = EAtom.intern("badsig");

	public static ErlangException raise(EObject kind, EObject value,
			EObject trace) throws ErlangException {

		// System.err.println("raise "+trace);

		EAtom clazz = kind.testAtom();
		ESeq traz = trace.testSeq();

		if (clazz == null || traz == null)
			throw badarg(kind, value, trace);

		throw new ErlangRaise(clazz, value, traz);
	}

	public static final EAtom am_badarg = EAtom.intern("badarg");
	public static final EAtom AM_BADMATCH = EAtom.intern("badmatch");
	public static final EAtom AM_BADARITH = EAtom.intern("badarith");
	public static final EAtom am_module = EAtom.intern("module");

	public static ECons cons(EObject h, EObject t) {
		return t.cons(h);
	}

	public static ErlangError badarg() {
		throw new ErlangError(am_badarg);
	}

	public static ErlangError badarg(EObject... args) {
		throw new ErlangError(am_badarg, args);
	}

	/** Utility method throws <code>erlang:error('badarg', [o1, o2])</code>. */
	public static ErlangError badarg(EObject o1, EObject o2) throws ErlangError {
		throw new ErlangError(am_badarg, NIL.cons(o2).cons(o1));
	}

	public static ErlangError badarith(EObject... args) {
		throw new ErlangError(AM_BADARITH, args);
	}

	public static ErlangError badarith(EObject o1, EObject o2) throws ErlangError {
		throw new ErlangError(AM_BADARITH, NIL.cons(o2).cons(o1));
	}

	public static ErlangError badarith(int o1, EObject o2) {
		throw new ErlangError(AM_BADARITH, NIL.cons(o2).cons(o1));
	}

	public static ErlangError badarith(EObject o1, int o2) {
		throw new ErlangError(AM_BADARITH, NIL.cons(o2).cons(o1));
	}

	public static ErlangError badarith(double o1, EObject o2) {
		throw new ErlangError(AM_BADARITH, NIL.cons(o2).cons(o1));
	}

	public static ErlangError badarith(EObject o1, double o2) {
		throw new ErlangError(AM_BADARITH, NIL.cons(o2).cons(o1));
	}

	public static ErlangError badarith(BigInteger o1, EObject o2) {
		throw new ErlangError(AM_BADARITH, NIL.cons(o2).cons(o1));
	}


	public static final EAtom TRUE = EAtom.intern("true");
	public static final EAtom FALSE = EAtom.intern("false");

	public static boolean eq(EObject o1, EObject o2) {
		return o1 == null ? o2 == null : o1.equals(o2);
	}

	public static boolean eq(EAtom o1, EAtom o2) {
		return o1 == o2;
	}

	public static EAtom as_atom_or_null(EObject o) {
		return o == null ? null : o.testAtom();
	}

	public static ECons as_nonempty_list_or_null(EObject o) {
		return o == null ? null : o.testNonEmptyList();
	}

	public static ENil as_nil_or_null(EObject o) {
		return o == null ? ERT.NIL : o.testNil();
	}

	public static EDouble as_float_or_null(EObject o) {
		return o == null ? null : o.testFloat();
	}

	/**
	 * @param s
	 * @return
	 */
	public static EPID loopkup_pid(ESeq name) {
		throw new NotImplemented();
	}

	// "definer" holds a reference to ClassLoader#defineClass
	static private final Method definer;
	static {
		try {
			definer = ClassLoader.class.getDeclaredMethod("defineClass",
					new Class[] { String.class, byte[].class, int.class,
							int.class });
			definer.setAccessible(true);
		} catch (Exception e) {
			throw new ErlangError(e);
		}
	}

	/**
	 * @param classLoader
	 * @param name
	 * @param data
	 * @param i
	 * @param length
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <T> Class<? extends T> defineClass(ClassLoader classLoader,
			String name, byte[] data, int i, int length) {

		/*
		 * Class<? extends ETuple> res = (Class<? extends ETuple>)
		 * loader2.define(name, data);
		 */
		Class<? extends T> res;
		try {
			res = (Class<? extends T>) definer.invoke(ETuple.class
					.getClassLoader(), name.replace('/', '.'), data, 0,
					data.length);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}

		if (!name.equals(res.getName())) {
			throw new Error();
		}

		return res;
	}

	public static ESmall box(int i) {
		return new ESmall(i);
	}

	/**
	 * Boxes a <code>long</code> value to an EInteger (EBig or ESmall)
	 * 
	 * @param longValue
	 * @return
	 */
	public static EInteger box2(long longVal) {

		// very simple: see if the longValue can be converted
		// to an int and back again without loosing it's value

		int intVal = (int) longVal;
		if (longVal == (long) intVal) {
			return new ESmall(intVal);
		} else {
			return new EBig(longVal);
		}
	}

	public static EInteger box(long longVal) {

		// compute l's offset from Integer.MIN_VALUE
		long offset_from_int_min = longVal - (long) Integer.MIN_VALUE;

		// strip sign bit
		long unsigned_offset = offset_from_int_min & Long.MAX_VALUE;

		if (unsigned_offset >= 0x100000000L) {
			return new EBig(longVal);
		} else {
			return new ESmall((int) longVal);
		}

	}

	/**
	 * @param doubleVal
	 * @return
	 */
	public static EDouble box(double doubleVal) {
		return new EDouble(doubleVal);
	}

	static BigInteger INT_MIN_AS_BIG = BigInteger.valueOf(Integer.MIN_VALUE);
	static BigInteger INT_MAX_AS_BIG = BigInteger.valueOf(Integer.MAX_VALUE);
	private static ENode localNode = new ENode();

	/**
	 * @param add
	 * @return
	 */
	public static EInteger box(BigInteger res) {

		if (res.compareTo(INT_MIN_AS_BIG) < 0)
			return new EBig(res);

		if (res.compareTo(INT_MAX_AS_BIG) > 0)
			return new EBig(res);

		return new ESmall(res.intValue());
	}

	/**
	 * @param b
	 * @return
	 */
	public static EAtom box(boolean bool) {
		return bool ? TRUE : FALSE;
	}

	/**
	 * @param b
	 * @return
	 */
	public static EAtom guard(boolean bool) {
		return bool ? TRUE : FALSE;
	}

	public static final ENil NIL = new ENil();
	public static final EAtom EXIT = EAtom.intern("EXIT");
	public static final EAtom IGNORED = EAtom.intern("ignored");
	private static final EAtom am_badmatch = EAtom.intern("badmatch");
	private static final EAtom am_case_clause = EAtom.intern("case_clause");
	public static final EObject am_undefined = EAtom.intern("undefined");
	public static final EObject am_receive_clause = EAtom
			.intern("receive_clause");
	public static final EObject AM_NOT_IMPLEMENTED = EAtom
			.intern("not_implemented");
	public static final EAtom AM_TIMEOUT = EAtom.intern("timeout");
	public static final EAtom am_try_case_clause = EAtom
			.intern("try_case_clause");
	public static final EAtom am_if_clause = EAtom.intern("if_clause");
	public static final boolean DEBUG = false;
	public static final boolean DEBUG2 = false;
	public static final boolean DEBUG_WAIT = false;
	public static final EBinary EMPTY_BINARY = new EBinary(new byte[0]);
	public static final ByteBuffer[] EMPTY_BYTEBUFFER_ARR = new ByteBuffer[0];
	public static final ByteBuffer EMPTY_BYTEBUFFER = ByteBuffer.allocate(0);
	public static final EAtom am_infinity = EAtom.intern("infinity");
	public static final EAtom am_noproc = EAtom.intern("noproc");
	public static final EAtom am_error = EAtom.intern("error");
	public static final EAtom am_value = EAtom.intern("value");
	public static final EAtom am_timeout = EAtom.intern("timeout");
	public static final EAtom am_function_clause = EAtom
			.intern("function_clause");
	public static final EObject am_ok = EAtom.intern("ok");
	private static final EObject am_noconnect = EAtom.intern("noconnect");
	public static final boolean DEVEL = true;

	public static EBitStringBuilder bs_init(int size, int flags) {
		return new EBitStringBuilder(size, flags);
	}

	public static EBitStringBuilder bs_initBits(int size, int flags) {
		return new EBitStringBuilder(size/8, size%8, flags);
	}

	/**
	 * @param e
	 * @return
	 */
	public static String describe_exception(Throwable e) {
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		e.printStackTrace(pw);
		pw.close();
		return sw.toString();
	}

	
	static EInteger max_send_time = ERT.box(4294967295L);
	static ESmall zero = ERT.box(0);
	
	public static final <T> boolean gt(Comparable<T> v1, T v2) {
		return v1.compareTo(v2) > 0;
	}
	
	public static final <T> boolean lt(Comparable<T> v1, T v2) {
		return v1.compareTo(v2) < 0;
	}
	
	public static final <T> boolean le(Comparable<T> v1, T v2) {
		return v1.compareTo(v2) <= 0;
	}
	
	@BIF
	public static EObject cancel_timer(EObject ref)
	{
		// check arguments 
		ERef timer_ref = ref.testReference();
		if (timer_ref == null) throw ERT.badarg();
		
		long time_left = ETimerTask.cancel(timer_ref);
		
		if (time_left > 0) {
			return ERT.box(time_left);
		} else {
			return ERT.FALSE;
		}
	}
	
	@BIF
	public static EObject read_timer(EObject ref)
	{
		// check arguments 
		ERef timer_ref = ref.testReference();
		if (timer_ref == null) throw ERT.badarg();
		
		long time_left = ETimerTask.read_timer(timer_ref);
		
		if (time_left > 0) {
			return ERT.box(time_left);
		} else {
			return ERT.FALSE;
		}
	}
	
	@BIF
	public static EObject send_after(EObject time, final EObject rcv, final EObject msg)
	{
		// check arguments 
		EInteger when = time.testInteger();
		final EInternalPID rcv_pid = rcv.testInternalPID();
		EAtom rcv_atom = rcv.testAtom();
		
		if (when == null 
			|| gt(when, max_send_time)
			|| lt(when, zero)
			|| (rcv_pid == null && rcv_atom == null)) {
			throw ERT.badarg(time, rcv, msg);
		}
			
		ETimerTask send_task = new ETimerTask(rcv_pid) {
			@Override
			public void on_timeout() {
				
				EHandle p;
				if ((p = rcv.testHandle()) != null) {
					p.sendb(msg);
					return;
				}

				p = register.get(rcv);
				if (p != null) {
					p.sendb(msg);
				}
			}
		};
		
		send_task.schedule(when.longValue());

		return send_task.ref;
	}

	@BIF
	public static EObject start_timer(EObject time, final EObject rcv, final EObject msg)
	{
		// check arguments 
		EInteger when = time.testInteger();
		final EInternalPID rcv_pid = rcv.testInternalPID();
		EAtom rcv_atom = rcv.testAtom();
		
		if (when == null 
			|| gt(when, max_send_time)
			|| lt(when, zero)
			|| (rcv_pid == null && rcv_atom == null)) {
			throw ERT.badarg(time, rcv, msg);
		}
			
		ETimerTask send_task = new ETimerTask(rcv_pid) {
			@Override
			public void on_timeout() {
				
				ETuple3 timeout_msg = new ETuple3();
				timeout_msg.elem1 = am_timeout;
				timeout_msg.elem2 = this.ref;
				timeout_msg.elem3 = msg;
				
				EHandle p;
				if ((p = rcv.testHandle()) != null) {
					p.sendb(timeout_msg);
					return;
				}

				p = register.get(rcv);
				if (p != null) {
					p.sendb(timeout_msg);
				}
			}
		};
		
		send_task.schedule(when.longValue());

		return send_task.ref;
	}
	/**
	 * @param owner
	 * @param make
	 * @throws Pausable
	 */
	@BIF(name = "!")
	public static EObject send(EProc proc, EObject pid, EObject msg)
			throws Pausable {
		// TODO handle ports also?
		proc.check_exit();
		
		//System.out.println(""+proc+" :: "+pid+" ! "+msg);

		EHandle p;
		if ((p = pid.testHandle()) != null) {
			p.send(msg);
			return msg;
		}

		p = register.get(pid);
		if (p != null) {
			p.send(msg);
			return msg;
		}

		if (pid.testAtom() == null) {
			throw badarg(pid, msg);
		}

		return msg;
	}

	@BIF
	public static EObject send(EProc proc, EObject pid, EObject msg,
			EObject options) throws Pausable {
		// TODO handle ports also?
		proc.check_exit();

		log.log(Level.FINER, "ignored options to send: " + options);
		
		EHandle p;
		if ((p = pid.testHandle()) != null) {
			p.send(msg);
			return am_ok;
		}

		p = register.get(pid);
		if (p != null) {
			p.send(msg);
			return am_ok;
		}

		if (pid.testAtom() == null) {
			throw badarg(pid, msg);
		}

		return am_noconnect;
	}

	/**
	 * @return
	 */
	public static ENode getLocalNode() {
		return localNode;
	}

	static EAtom am_undef = EAtom.intern("undef");

	/**
	 * @param fun
	 * @return
	 */
	public static ErlangException undef(FunID fun, EObject... args) {
		throw new ErlangError(am_undef, args);
	}

	public static EFun resolve_fun(EObject mod, EObject fun, int arity) {
		EAtom m = mod.testAtom();
		EAtom f = fun.testAtom();
		
		if (m == null || f == null )
			throw ERT.badarg(mod, fun, new ESmall(arity));

		EFun efun = EModuleManager.resolve(new FunID(m, f, arity));

		return efun;
	}
	
	public static EObject apply_list(EProc proc, EObject mod, EObject fun,
			ESeq seq, int len) throws Pausable {
		EAtom m = mod.testAtom();
		EAtom f = fun.testAtom();
		ESeq a = seq.testSeq();

		if (m == null || f == null || a == null)
			throw ERT.badarg(mod, fun, seq);

		EFun efun = EModuleManager.resolve(new FunID(m, f, len));
		return efun.apply(proc, a);
	}

	public static EObject apply_list_last(EProc proc, EObject mod, EObject fun,
			ESeq seq, int len) throws Pausable {
		EAtom m = mod.testAtom();
		EAtom f = fun.testAtom();
		ESeq a = seq.testSeq();

		if (m == null || f == null || a == null)
			throw ERT.badarg(mod, fun, seq);

		EFun found = EModuleManager.resolve(new FunID(m, f, len));

		if (len > 9) {
			// TODO: make it real tail recursion in stead
			return found.invoke(proc, a.toArray());
		}

		proc.tail = found;
		a = a.reverse();

		switch (len) {
		default:
			throw new NotImplemented();
		case 9:
			proc.arg8 = a.head();
			a = a.tail();
		case 8:
			proc.arg7 = a.head();
			a = a.tail();
		case 7:
			proc.arg6 = a.head();
			a = a.tail();
		case 6:
			proc.arg5 = a.head();
			a = a.tail();
		case 5:
			proc.arg4 = a.head();
			a = a.tail();
		case 4:
			proc.arg3 = a.head();
			a = a.tail();
		case 3:
			proc.arg2 = a.head();
			a = a.tail();
		case 2:
			proc.arg1 = a.head();
			a = a.tail();
		case 1:
			proc.arg0 = a.head();
			a = a.tail();
		case 0:
		}

		return EProc.TAIL_MARKER;
	}

	static Map<EAtom, EHandle> register = new ConcurrentHashMap<EAtom, EHandle>();

	/**
	 * @param aname
	 * @param handle
	 */
	public static void register(EAtom aname, EHandle handle) {
		register.put(aname, handle);
		handle.setName(aname);
	}

	public static boolean unregister(EAtom aname) {
		EHandle val = register.remove(aname);
		if (val != null) {
			val.setName(null);
			return true;
		} else {
			return false;
		}		
	}

	/**
	 * @param regname
	 * @return
	 */
	public static EObject whereis(EObject regname) {
		EObject result = register.get(regname);
		if (result == null) {
			// System.out.println(regname + " => " + am_undefined);
			return am_undefined;
		} else {
			// System.out.println(regname + " => " + result);
			return result;
		}
	}

	public static EObject badmatch(EObject val) {
		throw new ErlangError(new ETuple2(am_badmatch, val));
	}

	public static void paranoiaCheck(final EObject e, String details) {
		if (e == null) throw new Error("Bif returned null: "+details);
	}

	public static EObject decode_exception2(final ErlangException e) {
		return e.getCatchValue();
	}

	public static ETuple3 decode_exception3(final ErlangException e) {
		return e.getTryValue();
	}

	public static EObject case_end(EObject val) {
		throw new ErlangError(ETuple.make(am_case_clause, val));
	}

	public static EObject if_end() {
		throw new ErlangError(am_if_clause);
	}

	public static EObject try_case_end(EObject val) {
		throw new ErlangError(ETuple.make(am_try_case_clause, val));
	}

	static kilim.Scheduler scheduler = new kilim.Scheduler(threadPoolSize());
	public static EAtom am_io = EAtom.intern("io");
	public static EAtom am_attributes = EAtom.intern("attributes");
	public static EAtom am_badfun = EAtom.intern("badfun");

	public static EAtom am_name = EAtom.intern("name");
	public static EAtom am_arity = EAtom.intern("arity");
	public static EAtom am_env = EAtom.intern("env");

	public static EAtom am_index = EAtom.intern("index");
	public static EAtom am_new_index = EAtom.intern("new_index");
	public static EAtom am_new_uniq = EAtom.intern("new_uniq");
	public static EAtom am_uniq = EAtom.intern("uniq");
	public static EAtom am_pid = EAtom.intern("pid");
	public static EAtom am_type = EAtom.intern("type");
	public static EAtom am_local = EAtom.intern("local");
	public static EAtom am_external = EAtom.intern("external");

	public static void run(Task task) {
		task.setScheduler(scheduler);
		task.start();
	}
	
	 /*
	  * Skeleton for receive statement:
	  *
	  *      L1:          <-------------------+
	  *                   <-----------+       |
	  *     	     	       	  |   	  |
	  *             loop_rec L2 ------+---+   |
	  *             ...               |   |   |
	  *             remove_message 	  |   |	  |
	  *             jump L3           |   |   |
	  *		...	          |   |   |
	  *		loop_rec_end L1 --+   |   |
	  *      L2:          <---------------+   |
	  *	   	wait L1  -----------------+      or wait_timeout
	  *		timeout
	  *
	  *	 L3:    Code after receive...
	  *
	  */

	/** peek mbox at current index (proc.midx), which is 0 upon entry to the loop. */
	public static EObject loop_rec(EProc proc) {
		int idx = proc.midx;
		EObject msg = proc.mbox.peek(idx);		
		if (DEBUG_WAIT) System.err.println("WAIT| entered loop #"+idx+" message="+msg);
		return msg;
	}

	/** remove current message, and reset message index */
	public static void remove_message(EProc proc) {
		proc.mbox.remove(proc.midx);
		proc.midx = 0;
	}

	/** message did not match incoming, goto next message (will be followed by goto top-of-loop)*/
	public static void loop_rec_end(EProc proc) {
		proc.midx += 1;
	}

	/** wait for howlong, for one more message to be available */
	public static boolean wait_timeout(EProc proc, EObject howlong)
			throws Pausable {
		if (ERT.DEBUG_WAIT) System.err.println("WAIT| "+proc+" waits for messages for "+howlong+" ms");
			if (howlong == am_infinity) {
				proc.mbox.untilHasMessages(proc.midx+1);
				if (ERT.DEBUG_WAIT) System.err.println("WAIT| "+proc+" wakes up on message");
				return true;
			} else {
				EInteger ei;
				if ((ei = howlong.testInteger()) == null)
					throw new ErlangError(EAtom.intern("timeout_value"));

				boolean res = proc.mbox
					.untilHasMessages(proc.midx + 1, ei.longValue());
				if (ERT.DEBUG_WAIT) System.err.println("WAIT| "+proc+" wakes up "+(res?"on message" : "after timeout"));
				return res;
			}
	}

	/** wait forever, for one more message to be available */
	public static void wait(EProc proc) throws Pausable {
		int idx = proc.midx + 1;
		if (DEBUG_WAIT) System.err.println("WAIT| "+proc+" waits for "+idx+" messages");
		proc.mbox.untilHasMessages(idx);
		if (DEBUG_WAIT) System.err.println("WAIT| "+proc+" wakes up after timeout; now has "+(idx));
	}

	/** message reception timed out, reset message index */
	public static void timeout(EProc proc) {
		if (DEBUG_WAIT) System.err.println("WAIT| "+proc+" timed out");
		proc.midx = 0;
	}

	public static int unboxToInt(EInteger i) {
		return i.intValue();
	}

	public static double unboxToDouble(EInteger i) {
		return i.doubleValue();
	}

	public static int unboxToInt(EObject i) {
		ESmall ii;
		if ((ii = i.testSmall()) == null)
			throw ERT.badarg(i);
		return ii.value;
	}

	public static int unboxToInt(ENumber i) {
		return i.intValue();
	}

	public static double unboxToDouble(ENumber i) {
		return i.doubleValue();
	}

	public static double unboxToDouble(EObject i) {
		ENumber num;
		if ((num = i.testNumber()) == null)
			throw ERT.badarg(i);
		return num.doubleValue();
	}

	public static double unboxToDouble(int i) {
		return i;
	}

	public static void check_exit(EProc p) {
		p.check_exit();
	}

	public static EObject func_info(EAtom mod, EAtom fun, int arity) {
		throw new ErlangError(am_function_clause);
	}

	public static EObject func_info(EAtom mod, EAtom fun, ESeq args) {
		throw new ErlangError(am_function_clause, args);
	}

	static void load_module(EAtom module) throws IOException {
		File f = Compiler.find_and_compile(module.getName());
		EModuleManager.load_module(module, f.toURI().toURL());
	}

	public static void load_module(EAtom module, EBinary bin)
			throws IOException {
		File f = Compiler.compile(module.getName(), bin);
		EModuleManager.load_module(module, f.toURI().toURL());
	}

	/**
	 * @param command
	 * @return
	 */
	public static EDriver find_driver(EString command) {
		return Drivers.getDriver(command.stringValue());
	}

	/**
	 * @param job
	 */
	public static void run_async(final EAsync job, final EDriverTask dt) {
		run(new Task() {
			@Override
			public void execute() throws Pausable, Exception {
				job.async();
				dt.async_done(job);
			}
		});
	}

	/**
	 * @return
	 */
	public static ESeq getRemoteNodes() {
		// TODO: implement
		return ERT.NIL;
	}

	/**
	 * @return
	 */
	public static int threadPoolSize() {
		return Integer.parseInt(System.getProperty("erj.threads", "4"));
	}

}
