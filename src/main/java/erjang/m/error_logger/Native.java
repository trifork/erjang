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


package erjang.m.error_logger;

import erjang.BIF;
import erjang.EAtom;
import erjang.ENative;

/**
 * BIFs for error_logger
 */
public class Native extends ENative {

	private static final EAtom am_error = EAtom.intern("error");
	private static final EAtom am_warning = EAtom.intern("warning");
	private static final EAtom am_info = EAtom.intern("info");
	
	@BIF
	public static EAtom warning_map() {
		// TODO: implement, handle command-line argument +W
		
		return am_error;
	}
	
}
