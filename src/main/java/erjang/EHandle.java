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
 * An EHandle is either an EPort or an EPID.  EHandles can be sent messages
 */
public abstract class EHandle extends EObject {

	abstract ETask<?> self();

	/**
	 * @param msg
	 */
	public void send(EObject msg) {
		self().mbox_send(msg);
	}

	/**
	 * @param self
	 * @param result
	 */
	public void send_exit(EHandle from, EObject reason) {
		self().send_exit(from, reason);
	}

	/**
	 * @param self
	 */
	public abstract void link_oneway(EHandle other);
	
}
