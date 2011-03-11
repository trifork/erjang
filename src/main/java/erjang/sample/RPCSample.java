/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2011 by Trifork
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

package erjang.sample;

import erjang.EAtom;
import erjang.EList;
import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;
import erjang.EString;
import erjang.RPC;

public class RPCSample {

	static EAtom am_erlang = EAtom.intern("erlang");
	static EAtom am_display = EAtom.intern("display");
	static EAtom am_started = EAtom.intern("started");
	static EAtom am_init = EAtom.intern("init");
	static EAtom am_stop = EAtom.intern("stop");
	static EAtom am_io = EAtom.intern("io");
	static EAtom am_format = EAtom.intern("format");
	
	public static void main(String[] args) throws Exception {
		
		/** Launch erjang by running erjang.Main.main(String[]) in a fresh thread */
		new Thread() {
			
			{
				setDaemon(true);
				start();
			}
			
			public void run() {

				String[] ARGS = {
						"-progname", "ej",
						"-home", System.getProperty("user.home"),
						"-root", "/Users/krab/Projects/OTP_R13B04",
						"+A", "2",
						"+S", "1",
						"+e", "5.7.5",
						"-s", "rpc", "erjang_started"
					};
					
				try {
					erjang.Main.main(ARGS);
				} catch (Exception e) {
					e.printStackTrace();
				}
			};
		};

		//
		//  The   -s rpc erjang_started    argument makes the loader call rpc:erjang_started/0
		//  which is the trigger that will release 'wait_for_erjang_started'
		//
		System.err.println("did launch Erjang, ... waiting");
		RPC.wait_for_erjang_started(60*1000L);		

		//
		// Call erlang:display( ["Hello, Joe!~n", []] )
		// 
		EString hello_str = EString.fromString("Hello, Joe!~n");
		ESeq format_args = EList.make( hello_str, ERT.NIL ); 		
		RPC.call(am_erlang, am_display, (EObject)format_args );

		
		RPC.call(am_io, am_format, hello_str, ERT.NIL );

		System.err.println("calling init:stop(1)");
		RPC.call(am_init, am_stop, ERT.box(1));
		
		System.err.println("sleeping 5secs....");
		Thread.sleep(5 * 1000);

	}

}
