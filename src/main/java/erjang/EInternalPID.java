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

/**
 * This is a PID on this node
 */
public class ELocalPID extends EPID {

	private final EProc proc;

	public ELocalPID(EProc self) {
		this.proc = self;
	}
	
	/* (non-Javadoc)
	 * @see erjang.EHandle#self()
	 */
	@Override
	ETask<?> self() {
		return proc;
	}
	
	@Override
	public void send(EObject msg) {
		proc.mbox_send(msg);
	}
	
	/* (non-Javadoc)
	 * @see erjang.EPID#send_exit(erjang.EPID, erjang.EObject)
	 */
	@Override
	public void send_exit(EHandle from, EObject reason) {
		proc.send_exit(from, reason);
	}

	/* (non-Javadoc)
	 * @see erjang.EHandle#link_oneway(erjang.EHandle)
	 */
	@Override
	public void link_oneway(EHandle other) {
		proc.link_oneway(other);
	}
}
