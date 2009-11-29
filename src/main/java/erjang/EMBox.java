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

import java.util.concurrent.ConcurrentLinkedQueue;

/**
 * 
 */
public class EMBox {

	private static final EAtom am_interrupted = EAtom.intern("interrupted");
	ConcurrentLinkedQueue<EObject> queue = new ConcurrentLinkedQueue<EObject>();

	/**
	 * @return
	 */
	public EObject peek() {
		return queue.peek();
	}

	/**
	 * @param proc 
	 * 
	 */
	public void wait_forever(EProc proc) {
		while (queue.isEmpty()) {
			synchronized (this) {
				try {
					wait();
				} catch (InterruptedException e) {
					proc.check_exit();
				}
			}
		}
	}

	public void send(EObject msg) {
		queue.add(msg);
		synchronized (this) {
			notify();
		}
	}

	/**
	 * 
	 */
	public void remove_one() {
		queue.remove();
	}

}
