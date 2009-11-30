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

import erjang.ERef;

/**
 * 
 */
public interface EDriverInstance {

    /* called when port is closed, and when the
	   emulator is halted. */

	 void stop();
	
	/* called when we have output from erlang to 
	   the port */
	 void output(ByteBuffer data);
	
	/* called when we have input from one of 
	   the driver's handles) */
	void readyInput(EDriverEvent evt);
	
	/* called when output is possible to one of 
	   the driver's handles */
	void readyOutput(EDriverEvent evt);

	/* "ioctl" for drivers - invoked by 
	   port_control/3) */
	void control(int command, ByteBuffer buf, ByteBuffer rbuf);

	/* Handling of timeout in driver */
	void timeout();

		/* called when we have output from erlang
	   to the port */
	void outputv(ByteBuffer[] ev);
	
    /* called when the port is about to be 
	   closed, and there is data in the 
	   driver queue that needs to be flushed
	   before 'stop' can be called */
	void flush();

	void readyAsync(Object data);

    /* Works mostly like 'control', a syncronous
	   call into the driver. */
	void call(int command, ByteBuffer buf, ByteBuffer rbuf, int[] flags);

    /* Called when an event selected by 
	   driver_event() has occurred */
	void event(EDriverEvent event, Object eventData);

	void processExit(ERef monitor);
	
     /* Called on behalf of driver_select when
	   it is safe to release 'event'. A typical
	   unix driver would call close(event) */
	void stopSelect();
	
}
