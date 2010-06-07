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

package erjang.console;

import java.io.PrintStream;
import java.util.concurrent.locks.ReentrantLock;

import javax.swing.JTextPane;

import erjang.ERT;
import erjang.EString;
import erjang.driver.EDriver;
import erjang.driver.EDriverControl;

public class TTYTextAreaDriver implements EDriver {

	private kilim.ReentrantLock lock;
	final JTextPane text;
	final String message;
	private TTYTextAreaDriverControl control;

	public TTYTextAreaDriver(JTextPane text2, String message) {
		this.text = text2;
		this.message = message;
		this.control = new TTYTextAreaDriverControl(this, text, message);

		ERT.set_stdio(control.getInputStream(), 
				new PrintStream(control.getOutputStream()), 
				new PrintStream(control.getErrorStream()));

	}
	
	@Override
	public String driverName() {
		// appear to be the tty line driver
		return "tty_sl";
	}

	@Override
	public void finish() {
	}

	@Override
	public ReentrantLock getLock() {
		if (lock == null) {
			lock = new kilim.ReentrantLock();
		}
		return lock;
	}

	@Override
	public EDriverControl start(EString command) {
		return control;
	}

	@Override
	public boolean useDriverLevelLocking() {
		return false;
	}

}
