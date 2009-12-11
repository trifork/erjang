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

import java.nio.ByteBuffer;

/**
 * Can grow a byte-string backwards
 */
public class IOListBuilder {

	byte[] data = new byte[10];
	int pos = 10;
	int len = 0;
	
	ByteBuffer toByteBuffer() {
		return ByteBuffer.wrap(data, pos, len);
	}
	
	void prepend(byte value) {
		if (pos == 0) {
			byte[] new_data = new byte[len*2];
			System.arraycopy(data, 0, new_data, len, len);
			data = new_data;
			pos = len;
		}
		
		data[--pos] = value;
		len += 1;
	}
	
	
	
}
