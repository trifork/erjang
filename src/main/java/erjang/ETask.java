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

import java.util.Set;
import java.util.TreeSet;

/**
 * An ETask is what is common for processes and open ports
 */
public abstract class ETask<H extends EHandle> {
	
	public abstract void mbox_send(EObject msg);

	/**
	 * @param from
	 * @param reason
	 */
	public abstract void send_exit(EHandle from, EObject reason);

	/**
	 * @return
	 */
	public abstract H self();

	

	Set<EHandle> links = new TreeSet<EHandle>();

	/**
	 * @param task
	 */
	public void link_to(ETask<?> task) {
		link_oneway(task.self());
		task.self().link_oneway((EHandle)self());
	}

	public void link_oneway(EHandle h) {
		links.add(h);
	}
	
}
