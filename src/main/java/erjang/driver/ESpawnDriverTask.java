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

import erjang.EObject;
import erjang.EProc;
import erjang.EString;

/**
 * 
 */
public class ESpawnDriverTask extends EDriverTask {

	private final EString command;
	private final EObject portSetting;

	public EObject getName() {
		return command;
	}
	
	/**
	 * @param proc
	 * @param portSetting 
	 * @param command 
	 */
	public ESpawnDriverTask(EProc proc, EDriver driver, EString command, EObject portSetting) {
		super(proc.self_handle(), driver.start(command));
		this.command = command;
		this.portSetting = portSetting;
		super.parseOptions(new String[]{command.stringValue()}, portSetting);
	}
}
