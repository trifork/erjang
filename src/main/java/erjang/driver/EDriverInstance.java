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

import java.nio.ByteBuffer;
import java.util.concurrent.locks.Lock;

import erjang.EObject;
import erjang.ERef;

/**
 * 
 */
public abstract class EDriverInstance {
	
	/*
	 * called when port is closed, and when the emulator is halted.
	 */

	protected abstract void stop();

	/*
	 * called when we have output from erlang to the port
	 */
	protected abstract void output(ByteBuffer data);

	/*
	 * called when we have output from erlang to the port
	 */
	protected abstract void outputv(ByteBuffer[] ev);

	/*
	 * called when we have input from one of the driver's handles)
	 */
	protected abstract void readyInput(EDriverEvent evt);

	/*
	 * called when output is possible to one of the driver's handles
	 */
	protected abstract void readyOutput(EDriverEvent evt);

	/* called when "action" is possible */
	protected abstract void readyAsync(Object data);

	/*
	 * "ioctl" for drivers - invoked by port_control/3)
	 */
	protected abstract ByteBuffer control(int command, ByteBuffer buf);

	/* Handling of timeout in driver */
	protected abstract void timeout();

	/*
	 * called when the port is about to be closed, and there is data in the
	 * driver queue that needs to be flushed before 'stop' can be called
	 */
	protected abstract void flush();

	/*
	 * Works mostly like 'control', a syncronous call into the driver.
	 */
	protected abstract EObject call(int command, EObject data);

	/*
	 * Called when an event selected by driver_event() has occurred
	 */
	protected abstract void event(EDriverEvent event, Object eventData);

	protected abstract void processExit(ERef monitor);

	/*
	 * Called on behalf of driver_select when it is safe to release 'event'. A
	 * typical unix driver would call close(event)
	 */
	protected abstract void stopSelect();

}
