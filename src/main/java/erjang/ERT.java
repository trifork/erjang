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

import kilim.Pausable;
import kilim.Task;
import erjang.beam.Compiler;
import erjang.driver.EAsync;
import erjang.driver.EDriver;
import erjang.driver.EDriverTask;
import erjang.driver.efile.Driver;

@Module(value = "erlang")
public class ERT {

	public static EAtom am_badsig = EAtom.intern("badsig");

	@BIF
	public static ErlangException raise(EObject kind, EObject value,
			EObject trace) throws ErlangException {

		EAtom clazz = kind.testAtom();
		ESeq traz = trace.testSeq();

		if (clazz == null || traz == null)
			throw badarg(kind, value, trace);

		throw new ErlangRaise(clazz, value, traz);
	}

	public static final EAtom AM_BADARG = EAtom.intern("badarg");
	public static final EAtom AM_BADMATCH = EAtom.intern("badmatch");
	public static final EAtom AM_BADARITH = EAtom.intern("badarith");
	public static final EAtom AM_MODULE = EAtom.intern("module");

	public static ECons cons(EObject h, EObject t) {
		return t.cons(h);
	}

	public static ErlangError badarg() {
		throw new ErlangError(AM_BADARG);
	}

	public static ErlangError badarg(EObject... args) {
		throw new ErlangError(AM_BADARG, args);
	}

	public static ErlangError badarg(EObject o1, EObject o2) {
		throw new ErlangError(AM_BADARG, NIL.cons(o2).cons(o1));
	}

	public static ErlangError badarg(int o1, EObject o2) {
		throw new ErlangError(AM_BADARG, NIL.cons(o2).cons(o1));
	}

	public static ErlangError badarg(EObject o1, int o2) {
		throw new ErlangError(AM_BADARG, NIL.cons(o2).cons(o1));
	}

	public static ErlangError badarg(double o1, EObject o2) {
		throw new ErlangError(AM_BADARG, NIL.cons(o2).cons(o1));
	}

	public static ErlangError badarg(EObject o1, double o2) {
		throw new ErlangError(AM_BADARG, NIL.cons(o2).cons(o1));
	}

	public static ErlangError badarg(BigInteger o1, EObject o2) {
		throw new ErlangError(AM_BADARG, NIL.cons(o2).cons(o1));
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
	public static EPID loopkup_pid(EString name) {
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

	/**
	 * @param mod
	 * @param bin
	 */
	public static ETuple2 load_module(EAtom mod, EBinary bin) {

		EModule.load_module(mod, bin);

		return (ETuple2) ETuple.make(AM_MODULE, mod);
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
	private static ENode localNode;

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
		return bool ? TRUE : null;
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
	public static final boolean DEBUG = true;
	public static final EBinary EMPTY_BINARY = new EBinary(new byte[0]);
	public static final ByteBuffer EMPTY_BYTEBUFFER = ByteBuffer.allocate(0);
	public static final EAtom am_infinity = EAtom.intern("infinity");
	public static final EAtom am_noproc = EAtom.intern("noproc");

	public EBitStringBuilder bs_init(int size, int flags) {
		return new EBitStringBuilder(size, flags);
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

	/**
	 * @param e
	 * @return
	 */
	public static ESmall get_posix_code(IOException e) {

		// TODO Auto-generated method stub
		return null;
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

		EHandle p;
		if ((p = pid.testHandle()) != null) {
			p.send(msg);
			return msg;
		}

		EObject val = whereis(pid);
		if ((p = val.testHandle()) != null) {
			p.send(msg);
			return msg;
		}

		throw badarg(pid, msg);
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

	public static EObject apply(EProc proc, EObject arg1, EObject mod,
			EObject fun) throws Pausable {
		EAtom m = mod.testAtom();
		EAtom f = fun.testAtom();

		if (m == null || f == null)
			throw ERT.badarg(mod, fun, arg1);

		EFun efun = EModule.resolve(new FunID(m, f, 1));
		return efun.invoke(proc, new EObject[] { arg1 });
	}

	public static EObject apply(EProc proc, EObject mod, EObject fun)
			throws Pausable {
		EAtom m = mod.testAtom();
		EAtom f = fun.testAtom();

		if (m == null || f == null)
			throw ERT.badarg(mod, fun);

		EFun efun = EModule.resolve(new FunID(m, f, 0));
		return efun.invoke(proc, new EObject[0]);
	}

	public static EObject apply$last(EProc proc, EObject arg1, EObject mod,
			EObject fun) {
		EAtom m = mod.testAtom();
		EAtom f = fun.testAtom();

		if (m == null || f == null)
			throw ERT.badarg(mod, fun, arg1);

		proc.arg0 = arg1;
		proc.tail = EModule.resolve(new FunID(m, f, 1));
		return EProc.TAIL_MARKER;

		// .invoke(proc, NIL.cons(arg1).toArray());
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

	/**
	 * @param regname
	 * @return
	 */
	public static EObject whereis(EObject regname) {
		EObject result = register.get(regname);
		if (result == null) {
			System.out.println(regname + " => " + am_undefined);
			return am_undefined;
		} else {
			System.out.println(regname + " => " + result);
			return result;
		}
	}

	public static EObject badmatch(EObject val) {
		throw new ErlangError(am_badmatch, val);
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

	static kilim.Scheduler scheduler = new kilim.Scheduler(4);

	public static void run(Task task) {
		task.setScheduler(scheduler);
		task.start();
	}

	/** peek mbox */
	public static EObject receive_peek(EProc proc) {
		return proc.mbox.peek();
	}

	public static void remove_message(EProc proc) throws Pausable {
		proc.mbox.get();
	}

	public static void wait_forever(EProc proc) throws Pausable {
		proc.mbox.untilHasMessage();
	}

	public static EObject loop_rec_end(EProc proc) {
		EObject msg = proc.mbox.peek();
		throw new ErlangError(am_receive_clause, proc.self(), msg);
	}

	public static int unboxToInt(EInteger i) {
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
		throw new ErlangError(AM_BADMATCH);
	}

	public static boolean wait_timeout(EProc proc, EObject howlong)
			throws Pausable {
		if (howlong == am_infinity) {
			proc.mbox.untilHasMessage();
			return true;
		} else {
		EInteger ei;
		if ((ei = howlong.testInteger()) == null)
			throw badarg(howlong);
		return proc.mbox.untilHasMessage(ei.longValue());
		}
	}

	public static void timeout() {
		// skip //
	}

	static void load(EAtom module) throws IOException {
		File f = Compiler.find_and_compile(module.getName());
		EModule.load_module(module, f.toURI().toURL());
	}

	/**
	 * @param command
	 * @return
	 */
	public static EDriver find_driver(EString command) {
		if (command.equals(EString.fromString("efile"))) {
			return new Driver();
		}
		return null;
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
	  	  }});
	}
}
