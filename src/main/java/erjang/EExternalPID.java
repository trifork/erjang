/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
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

public class EExternalPID extends EPID {

	private int id;
	private int serial;
	private int creation;

	public EExternalPID(EPeer peer, int id, int serial, int creation) {
		super(peer);
		this.id = id;
		this.serial = serial;
		this.creation = creation;
	}


	protected int serial() {
		return serial;		
	}

	protected int id() {
		return id;
	}

	protected int creation() {
		return creation;
	}

	EPeer peer() { return (EPeer) this.node; }
	
	/** assume it does... */
	public boolean exists() {
		return true;
	}
	
	@Override
	public boolean is_alive() {
		throw new erjang.NotImplemented();

	}

	@Override
	public EObject process_info() {
		throw new erjang.NotImplemented();

	}

	@Override
	public EObject process_info(EObject spec) {
		throw new erjang.NotImplemented();

	}

	@Override
	public void set_group_leader(EPID gl) {
		throw new erjang.NotImplemented();

	}

	@Override
	public void exit_signal(EHandle from_pid, EObject reason, boolean exitToSender) throws Pausable  {
		peer().dsig_exit(from_pid, this, reason);
	}
	
	@Override
	public boolean add_monitor(EHandle from_pid, ERef ref) throws Pausable {
		peer().dsig_monitor(from_pid, this, ref);
		return true;
	}

	@Override
	public boolean link_oneway(EHandle other) throws Pausable  {
		peer().dsig_link(other, this);
		return true;
	}

	@Override
	public void unlink_oneway(EHandle other) throws Pausable {
		peer().dsig_unlink(other, this);
	}
	
	@Override
	public void remove_monitor(EHandle sender, ERef ref, boolean flush) throws Pausable {
		peer().dsig_demonitor(sender, ref, this);
	}
	
	@Override
	public int send(EHandle sender, EObject msg) throws Pausable {
		return peer().dsig_send(sender, this, msg);
	}
	
	@Override
	public void send_monitor_exit(EHandle from, ERef ref, EObject reason) throws Pausable {
		peer().dsig_send_monitor_exit(from, this, ref, reason);
	}
	
	@Override
	public void sendb(EObject msg) {
		throw new erjang.NotImplemented();
		
	}

}
