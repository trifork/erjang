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


package erjang.m.erlang;

import erjang.BIF;
import erjang.EObject;
import erjang.EProc;
import erjang.NotImplemented;

/**
 * 
 */
public class ErlProc {

	@BIF
	public static EObject display(EProc proc, EObject obj) {
		System.out.println(obj);
		return obj;
	}
	
	@BIF
	public static EObject erase(EProc proc, EObject key) {
		return proc.erase(key);
	}
	

	@BIF
	public static EObject register(EProc proc, EObject name, EObject pid) {
		throw new NotImplemented();
	}
	
	@BIF
	public static EObject unlink(EProc proc, EObject pid) {
		throw new NotImplemented();
	}
	
	@BIF
	public static EObject exit(EProc proc, EObject a1, EObject a2) {
		throw new NotImplemented();
	}
	
	@BIF
	public static EObject exit(EProc proc, EObject a1) {
		throw new NotImplemented();
	}
	
}
