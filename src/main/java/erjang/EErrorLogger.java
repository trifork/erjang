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

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;

/**
 * 
 */
public class EErrorLogger {


	public static void start()
	{
		try {
			EModuleLoader.find_and_load_module("erjang");
		} catch (IOException e) {
			throw new Error(e);
		}
		
		EProc proc = new EProc(null, EAtom.intern("erjang"), EAtom.intern("error_loop"), ERT.NIL);
		ERT.register(EAtom.intern("error_logger"), proc.self_handle());
		ERT.run(proc);		
	}

	
}
