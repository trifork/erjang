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

import kilim.Pausable;

/**
 * An EHandle is either an EPort or an EPID.  EHandles can be sent messages
 */
public abstract class EHandle extends EObject {

	protected ENode node;
	protected EObject name = null;
	
	protected EHandle(ENode node) {
		this.node = node;
	}

	public EHandle testHandle() { return this; }
	
	ETask<?> task() {
		throw new Error("only local handles can provide task reference");
	}

	public boolean exists() {
		return task().exists();
	}
	
	/**
	 * @param msg
	 * @throws Pausable 
	 */
	public void send(EObject msg) throws Pausable {
		task().mbox_send(msg);
	}

	public void sendb(EObject msg) {
		task().mbox().putb(msg);
	}

	/**
	 * @param self
	 * @param result
	 * @throws Pausable 
	 */
	public void exit_signal(EHandle from, EObject reason) throws Pausable {
		task().send_exit(from, reason);
	}

	/**
	 * A one-way link message.  (other is already linked to this handle).
	 * 
	 * @param other
	 * @throws Pausable 
	 */
	public abstract void link_oneway(EHandle other) throws Pausable;

	/**
	 * @param selfHandle
	 * @param object
	 */
	public abstract ERef add_monitor(EPID selfHandle, EObject object);


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
	 */
	public abstract void remove_monitor(ERef r, boolean flush);
	
}
