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

import java.nio.ByteBuffer;

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
	void ready_input(EDriverEvent evt);
	
	/* called when output is possible to one of 
	   the driver's handles */
	void ready_output(EDriverEvent evt);

	/* "ioctl" for drivers - invoked by 
	   port_control/3) */
	void control(int command, ByteBuffer buf, ByteBuffer rbuf);

	/* Handling of timeout in driver */
	void timeout();

		/* called when we have output from erlang
	   to the port */
	void outputv(ByteBuffer[] ev);

     void (*ready_async)(ErlDrvData drv_data, ErlDrvThreadData thread_data);
     void (*flush)(ErlDrvData drv_data);
                                 /* called when the port is about to be 
 				   closed, and there is data in the 
 				   driver queue that needs to be flushed
 				   before 'stop' can be called */
     int (*call)(ErlDrvData drv_data, unsigned int command, char *buf, 
 		   int len, char **rbuf, int rlen, unsigned int *flags); 
                                 /* Works mostly like 'control', a syncronous
 				   call into the driver. */
     void (*event)(ErlDrvData drv_data, ErlDrvEvent event,
 		  ErlDrvEventData event_data);
                                 /* Called when an event selected by 
 				   driver_event() has occurred */
     int extended_marker;	/* ERL_DRV_EXTENDED_MARKER */
     int major_version;		/* ERL_DRV_EXTENDED_MAJOR_VERSION */
     int minor_version;		/* ERL_DRV_EXTENDED_MINOR_VERSION */
     int driver_flags;		/* ERL_DRV_FLAGs */
     void *handle2;              /* Reserved -- Used by emulator internally */
     void (*process_exit)(ErlDrvData drv_data, ErlDrvMonitor *monitor);
                                 /* Called when a process monitor fires */
     void (*stop_select)(ErlDrvEvent event, void* reserved);
     	                        /* Called on behalf of driver_select when
 				   it is safe to release 'event'. A typical
 				   unix driver would call close(event) */
	
}
