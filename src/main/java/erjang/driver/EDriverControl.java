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

import erjang.EHandle;
import erjang.EObject;
import erjang.EPID;
import erjang.ERef;

/**
 * 
 */
public abstract class EDriverControl {

	abstract void setTask(EDriverTask task);
	
	public void setup() {}

	/**
	 * @param reason TODO
	 * @throws Pausable 
	 * 
	 */
	protected void stop(EObject reason) throws Pausable {
		// TODO Auto-generated method stub
		
	}

	/**
	 * @throws Pausable 
	 * 
	 */
	protected void timeout() throws Pausable {
		// TODO Auto-generated method stub
		
	}

	/**
	 * @param caller TODO
	 * @param bufv
	 * @throws IOException 
	 * @throws Pausable 
	 */
	protected void outputv(EHandle caller, ByteBuffer[] bufv) throws IOException, Pausable {
		// TODO Auto-generated method stub
		
	}

	/**
	 * @param pid TODO
	 * @param op
	 * @param cmd2
	 * @return
	 * @throws Pausable 
	 */
	protected ByteBuffer control(EPID pid, int op, ByteBuffer cmd2) throws Pausable {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @param ch
	 * @throws Pausable 
	 */
	protected void readyInput(SelectableChannel ch) throws Pausable {
		// TODO Auto-generated method stub
		
	}

	/**
	 * @param ch
	 * @throws Pausable 
	 */
	protected void readyOutput(SelectableChannel ch) throws Pausable {
		// TODO Auto-generated method stub
		
	}

	/**
	 * @param ch
	 * @throws Pausable 
	 */
	public void readyConnect(SelectableChannel ch) throws Pausable {
		// TODO Auto-generated method stub
		
	}

	/**
	 * @param ch
	 * @throws Pausable 
	 */
	public void readyAccept(SelectableChannel ch) throws Pausable {
		// TODO Auto-generated method stub
		
	}

	/**
	 * @param ch
	 * @throws Pausable 
	 */
	protected void stopSelect(SelectableChannel ch) throws Pausable {
		// TODO Auto-generated method stub
		
	}

	/**
	 * @param job
	 * @throws Pausable 
	 */
	protected void readyAsync(EAsync job) throws Pausable {
		// TODO Auto-generated method stub
		
	}

	/**
	 * @param op
	 * @param data
	 * @return
	 * @throws Pausable 
	 */
	protected EObject call(EPID caller, int op, EObject data) throws Pausable {
		// TODO Auto-generated method stub
		return null;
	}

	public void processExit(ERef ref) throws Pausable {
		
	}

	public abstract EDriver getDriver();


}
