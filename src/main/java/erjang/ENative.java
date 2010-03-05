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

/**
 * Base class for native code.  
 * 
 * To implement a native (BIF) for module named <code>foo</code>, create a subclass 
 * of this named <code>erjang.m.foo.Native</code>.  
 * 
 * See {@link erjang.m.ets.Native} (the BIFs for <code>ets</code>), which is 
 * the appointed "good style to copy" for coding conventions.  
 * 
 */
public abstract class ENative {
		
	/**
	 * if you want to put the BIFs in multiple class files, you can tell which
	 * class files here.  Normally that is not needed, but the module <code>erlang</code>
	 * has so many BIFs that it is very inconvenient to have them in one large 
	 * file.
	 */
	
	public Class<?>[] getNativeClasses() {
		return new Class[] { getClass() };
	}
}
