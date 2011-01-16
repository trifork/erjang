/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Erik Søe Sørensen
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

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.IOException;

class OutputCollectorThread extends Thread {
	InputStream in;
	ByteArrayOutputStream acc = new ByteArrayOutputStream();
	byte[] buf = new byte[1024];
	public OutputCollectorThread(InputStream in) {
		this.in = in;
	}

	@Override public void run() {
		try {
			int len;
			while ((len = in.read(buf)) > 0) {
				acc.write(buf, 0, len);
			}
		} catch (IOException ioe) {
			System.err.println("I/O error: "+ioe);
			try { in.close(); } catch (IOException ioe2) {}
		}
	}

	public byte[] getResult() {
		return acc.toByteArray();
	}
}