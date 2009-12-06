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

import erjang.ETask.State;

import kilim.Mailbox;
import kilim.Pausable;

/**
 * An erlang process
 */
public final class EProc extends ETask<EInternalPID> {
	public static final EObject TAIL_MARKER = new ETailMarker();

	private static final EAtom am_trap_exit = EAtom.intern("trap_exit");

	public EFun tail;
	public EObject arg0, arg1, arg2, arg3, arg4, arg5, arg6;

	private EInternalPID self;

	private EPID group_leader;

	/**
	 * @param m
	 * @param f
	 * @param array
	 */
	public EProc(EPID group_leader, EAtom m, EAtom f, ESeq a) {
		self = new EInternalPID(this);

		// if no group leader is given, we're our own group leader
		this.group_leader = group_leader == null ? self : group_leader;
		//this.run_mod = m;
		//this.run_fun = f;
		//this.run_args = a;
		
		int arity = a.length();
		EFun target = EModule.resolve(new FunID(m,f,arity));
		
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
		

	}

	/**
	 * @return
	 */
	public EInternalPID self() {
		return self;
	}

	/**
	 * @param key
	 * @param value
	 * @return
	 */

	Map<EObject, EObject> pdict = new HashMap<EObject, EObject>();

	private EAtom trap_exit = ERT.FALSE;


	protected void process_incoming_exit(EHandle from, EObject reason) throws Pausable
	{
		if (trap_exit == ERT.TRUE) {
			// we're trapping exits, so we in stead send an {'EXIT', from,
			// reason} to self
			System.err.println("kill message");
			mbox_send(ETuple.make(ERT.EXIT, from, reason));
		} else {
			System.err.println("kill signal");
			// try to kill this thread
			this.exit_reason = reason;
			this.pstate = State.EXIT_SIG;
			this.kill(new ErlangExitSignal(reason));
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
	public ECons get() {
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

	/**
	 * @param testAtom
	 * @param a2
	 * @return
	 */
	public EObject process_flag(EAtom flag, EObject value) {

		if (flag == am_trap_exit) {
			EAtom old = this.trap_exit;
			trap_exit = value.testBoolean();
			return old;
		}

		throw new NotImplemented();
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
				e.printStackTrace();
				result = e.reason();

			} catch (ErlangExitSignal e) {
				e.printStackTrace();
				result = e.reason();

			} catch (Throwable e) {

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
			
			send_exit_to_all_linked(result);

		} catch (ThreadDeath e) {
			throw e;
			
		} catch (Throwable e) {
			e.printStackTrace();
		}

	}



}

class ETailMarker extends EObject {

	@Override
	int cmp_order() {
		return -1;
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