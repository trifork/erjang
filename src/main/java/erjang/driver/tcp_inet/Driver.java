/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
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

package erjang.driver.tcp_inet;

import java.util.concurrent.locks.ReentrantLock;

import erjang.EString;
import erjang.driver.EDriver;
import erjang.driver.EDriverControl;
import erjang.net.Protocol;

public class Driver implements EDriver {

	@Override
	public String driverName() {
		return "tcp_inet";
	}

	@Override
	public void finish() {
	}

	@Override
	public EDriverControl start(EString command) {
		TCPINet inst = new TCPINet(Protocol.TCP, this);
		return inst;
	}

	@Override
	public boolean useDriverLevelLocking() {
		return false;
	}

	@Override
	public ReentrantLock getLock() {
		throw new erjang.NotImplemented();
		
	}
}
