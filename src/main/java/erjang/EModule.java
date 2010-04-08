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


public abstract class EModule {


	/** Base constructor for modules.  Will register <code>this</code> in the system. */
	public EModule() {
		this(false);
	}

	public EModule(boolean delay_setup) {
		// TODO: handle if there is a module of this name already!
		if (!delay_setup) setup();
	}

	protected void setup() {EModuleManager.setup_module(this);}

	/**
	 * This method is code-generated in subclasses
	 * @return module name, as a Java string
	 */
	public abstract String module_name();

	/**
	 * This method is code-generated in subclasses
	 * 
	 * @return the attributes associated with this module
	 */
	public ESeq attributes() {
		return ERT.NIL;
	}

	/**
	 * This method is used by EModuleManager in function resolution.
	 */
	public abstract void registerImportsAndExports() throws Exception;

        public abstract ClassLoader getModuleClassLoader();
}
