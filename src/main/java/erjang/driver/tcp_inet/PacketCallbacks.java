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

import kilim.Pausable;

public abstract class PacketCallbacks<T> {
	abstract int http_error(T src, byte[] data, int pos, int len) throws Pausable;

	abstract int http_response(T arg, int major, int minor, int status,
			byte[] data, int ptr, int n) throws Pausable;

	abstract int http_request(T arg, Packet.HTTPAtom method, byte[] data,
			int meth_ptr, int meth_len, PacketHttpURI uri, int major, int minor) throws Pausable;
	
	abstract int http_header(T arg, Packet.HTTPAtom name, byte[] name_buf, int name_ptr, int name_len, byte[] val_buf, int val_ptr, int val_len) throws Pausable;
	
	abstract int http_eoh(T arg) throws Pausable;
}
