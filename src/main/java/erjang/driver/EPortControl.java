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


package erjang.driver;

import kilim.Pausable;
import erjang.EPseudoTerm;

/**
 * Class internal to the driver subsystem, used for async messages to a port task
 */
public abstract class EPortControl extends EPseudoTerm {

	public EPortControl testPortControl() {
		return this;
	}

	@Override
	public int hashCode() { // Shouldn't be called.
		return System.identityHashCode(this);
	}

	/**
	 * 
	 */
	public abstract void execute() throws Exception, Pausable;
	
	
}
