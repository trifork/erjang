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

package erjang.util;

import java.io.IOException;
import java.util.concurrent.atomic.AtomicInteger;

public class Progress {

	static ProgressListener listener;

	public static void setListener(ProgressListener listener) {
		Progress.listener = listener;
	}
	
	static AtomicInteger step = new AtomicInteger();

	static byte[][] wheel = new byte[][] { new byte[] { '|', '\b' },
			new byte[] { '/', '\b' }, new byte[] { '-', '\b' },
			new byte[] { '\\', '\b' }, };

	static public void activity(String string) {
		if (listener != null) {
			listener.progress(string);
		} else {
			int next = step.incrementAndGet();
			try {
                //there is a global System.property to suppress the output
                String suppress = System.getProperty("erjang.progress.suppress");
                if (suppress == null) {
                    suppress = "false";
                }

                if (!Boolean.parseBoolean(suppress)) {
				    System.err.write(wheel[next % 4]);
                }
			} catch (IOException e) {
				// ignore
			}
		}
	}

	public static void done() {
	}

}
