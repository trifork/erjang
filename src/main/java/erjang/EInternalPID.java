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
 * This is a PID on this node
 */
public class EInternalPID extends EPID implements ELocalHandle {

	private final EProc task;
	
	public EInternalPID testInternalPID() {
		return this;
	}

	public EInternalPID(EProc self) {
		super(ERT.getLocalNode());
		this.task = self;
	}
	
	/* (non-Javadoc)
	 * @see erjang.EPID#is_alive()
	 */
	@Override
	public boolean is_alive() {
		return task.is_alive();
	}
	
	public ELocalHandle testLocalHandle() {
		return this;
	}

	/* (non-Javadoc)
	 * @see erjang.EHandle#self()
	 */
	@Override
	EProc task() {
		return task;
	}
	
	@Override
	public void send(EObject msg) throws Pausable {
		task.mbox.put(msg);
	}
	
	/* (non-Javadoc)
	 * @see erjang.EPID#compare_same(erjang.EObject)
	 */
	@Override
	int compare_same(EObject rhs) {
		return rhs.r_compare_same(this);
	}

	/* (non-Javadoc)
	 * @see erjang.EObject#r_compare_same(erjang.EInternalPID)
	 */
	@Override
	int r_compare_same(EInternalPID lhs) {
		return task.id - lhs.task.id;
	}
	

	/* (non-Javadoc)
	 * @see erjang.EHandle#link_oneway(erjang.EHandle)
	 */
	@Override
	public void link_oneway(EHandle other) throws Pausable {
		task.link_oneway(other);
	}
	
	public ERef add_monitor(EPID target, EObject object) {
		// todo: check if task is alive!
		return task.add_monitor(target, object);
	}
	
	@Override
	public void remove_monitor(ERef r, boolean flush) {
		task.remove_monitor(r, flush);
	}


	/* (non-Javadoc)
	 * @see erjang.EPID#set_group_leader(erjang.EPID)
	 */
	@Override
	public void set_group_leader(EPID group_leader) {
		task.set_group_leader(group_leader);
	}

	/**
	 * @return
	 */
	public int internal_pid_number() {
		throw new NotImplemented();
	}
	
	@Override
	public String toString() {
		return "PID<" + (name==null?"":name)  + ":" + task().id + ">";
	}
	
	@Override
	public EObject process_info() {
		return task.process_info();
	}
	
	@Override
	public EObject process_info(EObject spec) {
		return task.process_info(spec);
	}
	
	@Override
	public boolean equalsExactly(EObject rhs) {
		return rhs==this;
	}

	/**
	 * @param eTimerTask
	 */
	public void add_exit_hook(ExitHook hook) {
		if (is_alive())
			task.add_exit_hook(hook);
	}

	/**
	 * @param eTimerTask
	 */
	public void remove_exit_hook(ExitHook hook) {
		task.remove_exit_hook(hook);
	}
}
