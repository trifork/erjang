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

import erjang.m.erlang.DistEntry;
import kilim.Mailbox;
import kilim.Pausable;
import kilim.Task;

/**
 * This is a PID on this node
 */
public class EInternalPID extends EPID implements ELocalHandle {

	private EProc task;
	private int id;
	private DistEntry distEntry;
	
	public EInternalPID testInternalPID() {
		return this;
	}
	
	@Override
	protected int id() {
		return id & 0x7fff;
	}
	
	@Override
	protected int serial() {
		return id >>> 15;
	}
	
	@Override
	public EAtom node() {
		return ERT.getLocalNode().node;
	}
	
	@Override
	protected int creation() {
		return ERT.getLocalNode().creation;
	}

	public EInternalPID(EProc self) {
		super(ERT.getLocalNode());
		this.task = self;
		this.id = task.id;
	}
	
	/* (non-Javadoc)
	 * @see erjang.EPID#is_alive()
	 */
	@Override
	public boolean is_alive() {
		return task != null && task.is_alive();
	}
	
	public ELocalHandle testLocalHandle() {
		return this;
	}

	/* (non-Javadoc)
	 * @see erjang.EHandle#self()
	 */
	@Override
	public
	EProc task() {
		return task;
	}
		
	/* (non-Javadoc)
	 * @see erjang.EHandle#link_oneway(erjang.EHandle)
	 */
	@Override
	public boolean link_oneway(EHandle other) throws Pausable {
		EProc task = this.task;
		if (task != null)
			return task.link_oneway(other);
		return false;
	}
	
	@Override
	public void unlink_oneway(EHandle other) throws Pausable {
		EProc task = this.task;
		if (task != null)
			task.unlink_oneway(other);
	}
	
	public synchronized boolean add_monitor(EHandle observer, ERef ref) throws Pausable {
		EProc task = this.task;
		if (task != null) 
			return task.add_monitor(observer, ref);
		else 
			return false;
	}
	
	@Override
	public void send_monitor_exit(EHandle from, ERef ref, EObject reason) throws Pausable {
		EProc task = this.task;
		if (task != null) {
			task.send_monitor_exit(from, ref, reason);
		}
		
	}
	
	@Override
	public void remove_monitor(EHandle sender, ERef r, boolean flush) throws Pausable {
		EProc task = this.task;
		if (task != null) 
			task.remove_monitor(r, flush);
	}


	/* (non-Javadoc)
	 * @see erjang.EPID#set_group_leader(erjang.EPID)
	 */
	@Override
	public void set_group_leader(EPID group_leader) {
		EProc task = this.task;
		if (task != null) 
			task.set_group_leader(group_leader);
	}

	/**
	 * @return
	 */
	public int internal_pid_number() {
		return id;
	}

	/*
	@Override
	public String toString() {
		
		return "<" + (name==null?"":name)  + ":" + id + ">";
	}
	*/
	
	@Override
	public EObject process_info() {
		EProc task = this.task;
		if (task == null) return ERT.am_undefined;
		return task.process_info();
	}
	
	@Override
	public EObject process_info(EObject spec) {
		EProc task = this.task;
		if (task == null) return ERT.am_undefined;
		return task.process_info(spec);
	}
	
	@Override
	public boolean equalsExactly(EObject rhs) {
		return rhs==this;
	}

	@Override
	public int hashCode() {
		return System.identityHashCode(this);
	}

	/**
	 * @param eTimerTask
	 */
	public void add_exit_hook(ExitHook hook) {
		EProc task = this.task;
		if (task != null && is_alive())
			task.add_exit_hook(hook);
	}

	/**
	 * @param eTimerTask
	 */
	public void remove_exit_hook(ExitHook hook) {
		EProc task = this.task;
		if (task != null)
			task.remove_exit_hook(hook);
	}

	/**
	 * 
	 */
	public void done() {
		this.task = null;
	}

	public void set_dist_entry(DistEntry distEntry) {
		this.distEntry = distEntry;
	}

	
}
