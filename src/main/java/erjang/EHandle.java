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

import java.util.logging.Level;

import kilim.Pausable;
import kilim.Task;

/**
 * An EHandle is either an EPort or an EPID.  EHandles can be sent messages
 */
public abstract class EHandle extends EObject {

	protected final EAbstractNode node;
	protected EAtom name = null;
	
	protected EHandle(EAbstractNode node) {
		if (node == null) throw new IllegalArgumentException("node cannot be null");
		this.node = node;
	}

	public EHandle testHandle() { return this; }
	
	ETask<?> task() {
		throw new Error("only local handles can provide task reference");
	}

	public boolean exists() {
		ETask<?> task = task();
		return task != null && task.exists();
	}
	
	/**
	 * @param sender TODO
	 * @param msg
	 * @throws Pausable 
	 */
	public int send(EHandle sender, EObject msg) throws Pausable {
		ETask<?> task = task();
		if (task != null) {
			task.mbox.put(msg);
			return task.mbox.size();
		} else {
			if (ERT.log.isLoggable(Level.FINE)) {
				ERT.log.fine("sending message to dead process/port ignored "+this+" ! "+msg);
			}
			return 0;
		}
	}

	public void sendb(EObject msg) {
		ETask<?> task = task();
		if (task != null) {
			task.mbox().putb(msg);
		}
	}

	/**
	 * @param is_erlang_exit2 TODO
	 * @param self
	 * @param result
	 * @throws Pausable 
	 * @throws Pausable 
	 */
	public void exit_signal(EHandle from, EObject reason, boolean is_erlang_exit2) throws Pausable {
		ETask<?> task = task();
		if (task != null) {
			task.send_exit(from, reason, is_erlang_exit2);
		}
	}

	/**
	 * A one-way link message.  (other is already linked to this handle).
	 * 
	 * @param other
	 * @throws Pausable 
	 * @throws Pausable 
	 */
	public abstract boolean link_oneway(EHandle other) throws Pausable;

	public abstract void unlink_oneway(EHandle other) throws Pausable;
	
	/**
	 * @param ref TODO
	 * @param selfHandle
	 * @throws Pausable 
	 */
	public abstract boolean add_monitor(EHandle observer, ERef ref) throws Pausable;


	/**
	 * @param other
	 * @return
	 */
	public static EHandle cast(EObject other) {
		
		EHandle h = other.testHandle();
		
		if (h == null) {
			h = ERT.whereis(other).testHandle();
		}

		return h;
	}

	/**
	 * @param aname
	 */
	public void setName(EAtom aname) {
		this.name = aname;
	}

	/**
	 * @return
	 */
	public ELocalHandle testLocalHandle() {
		return null;
	}

	/**
	 * @return
	 */
	public EAtom node() {
		return node.node();
	}

	/**
	 * @param r
	 * @throws Pausable 
	 */
	public abstract void remove_monitor(EHandle sender, ERef r, boolean flush) throws Pausable;

	public abstract void send_monitor_exit(EHandle from, ERef ref, EObject reason) throws Pausable;
	
}
