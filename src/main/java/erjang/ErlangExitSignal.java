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
 * Used in erlang when an thread receives an untrapped exit signal
 */
public class ErlangExitSignal extends ThreadDeath {

	private final EObject exitReason;

	/**
	 * @param exitReason
	 */
	public ErlangExitSignal(EObject exitReason) {
		this.exitReason = exitReason;
	}

	/**
	 * @return
	 */
	public EObject reason() {
		return exitReason;
	}

	/* (non-Javadoc)
	 * @see java.lang.Throwable#getMessage()
	 */
	@Override
	public String getMessage() {
		return exitReason.toString();
	}
}
