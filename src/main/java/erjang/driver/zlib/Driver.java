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

package erjang.driver.zlib;

import java.util.concurrent.locks.ReentrantLock;

import erjang.EString;
import erjang.driver.EDriver;
import erjang.driver.EDriverControl;

public class Driver implements EDriver {

	@Override
	public String driverName() {
		return "zlib_drv";
	}

	@Override
	public boolean useDriverLevelLocking() {
		return false;
	}

	@Override
	public void finish() {
	}

	@Override
	public ReentrantLock getLock() {
		throw new erjang.NotImplemented();

	}

	@Override
	public EDriverControl start(EString command) {
		return new ZLibDriver(this, command);
	}

}
