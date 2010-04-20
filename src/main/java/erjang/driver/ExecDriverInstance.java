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

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;

import kilim.Pausable;

import erjang.EObject;
import erjang.EPID;
import erjang.ERef;
import erjang.ETuple2;

/**
 * 
 */
public class ExecDriverInstance extends EDriverInstance {

	/**
	 * @param name
	 */
	public ExecDriverInstance(ETuple2 name) {
		super(new ExecDriver(name));
	}

	/* (non-Javadoc)
	 * @see erjang.driver.EDriverInstance#call(int, erjang.EObject)
	 */
	@Override
	protected EObject call(EPID pid, int command, EObject data) throws Pausable {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see erjang.driver.EDriverInstance#flush()
	 */
	@Override
	protected void flush() throws Pausable {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see erjang.driver.EDriverInstance#output(java.nio.ByteBuffer)
	 */
	@Override
	protected void output(ByteBuffer data) throws IOException, Pausable {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see erjang.driver.EDriverInstance#processExit(erjang.ERef)
	 */
	@Override
	public void processExit(ERef monitor) throws Pausable {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see erjang.driver.EDriverInstance#readyAsync(erjang.driver.EAsync)
	 */
	@Override
	protected void readyAsync(EAsync data) throws Pausable {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see erjang.driver.EDriverInstance#readyInput(java.nio.channels.SelectableChannel)
	 */
	@Override
	protected void readyInput(SelectableChannel ch) throws Pausable {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see erjang.driver.EDriverInstance#readyOutput(java.nio.channels.SelectableChannel)
	 */
	@Override
	protected void readyOutput(SelectableChannel evt) throws Pausable {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see erjang.driver.EDriverInstance#timeout()
	 */
	@Override
	protected void timeout() throws Pausable {
		// TODO Auto-generated method stub

	}

}
