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

import kilim.Pausable;


public abstract class EPort extends EHandle {

	int id;
	int creation;
	
	public EPort(ENode node) {
		super(node);
		this.id = node.createPortID();
		this.creation = node.creation();
	}
	
	@Override
	int cmp_order() {
		return CMP_ORDER_PORT;
	}
	/**
	 * @return
	 */
	public EString getName() {
		return EString.fromString(toString());
	}
	
	@Override
	int compare_same(EObject rhs) {
		// TODO: make faster
		return toString().compareTo(rhs.toString());
	}

	@Override
	public int hashCode() {
		return (10000019 * node.hashCode() +
				20000003 * id +
				30000001 * creation);
	}

	@Override
	public EPort testPort() {
		return this;
	}
	

	public static EAtom am_fd = EAtom.intern("fd");
	public static EAtom am_packet = EAtom.intern("packet");
	public static EAtom am_stream = EAtom.intern("stream");
	public static EAtom am_line = EAtom.intern("line");
	public static EAtom am_hide = EAtom.intern("hide");
	public static EAtom am_cd = EAtom.intern("cd");
	public static EAtom am_env = EAtom.intern("env");
	public static EAtom am_args = EAtom.intern("args");
	public static EAtom am_arg0 = EAtom.intern("arg0");
	public static EAtom am_exit_status = EAtom.intern("exit_status");
	public static EAtom am_use_stdio = EAtom.intern("use_stdio");
	public static EAtom am_nouse_stdio = EAtom.intern("nouse_stdio");
	public static EAtom am_stderr_to_stdout = EAtom.intern("stderr_to_stdout");
	public static EAtom am_in = EAtom.intern("in");
	public static EAtom am_out = EAtom.intern("out");
	public static EAtom am_binary = EAtom.intern("binary");
	public static EAtom am_eof = EAtom.intern("eof");
	public static EAtom am_close = EAtom.intern("close");
	public static EAtom am_command = EAtom.intern("command");
	public static EAtom am_connect = EAtom.intern("connect");
	public static EAtom am_connected = EAtom.intern("connected");
	/**
	 * @return
	 */
	public abstract boolean isOpen();
	
	public static EPort read(EInputStream ei) {
		throw new NotImplemented();
	}
	/**
	 * @param node
	 * @param id
	 * @param creation
	 * @return
	 */
	public static EPort make(EAtom node, int id, int creation) {
		throw new NotImplemented();
	}
	
	@Override
	public String toString() {
		return "#Port<" + id + "." + creation + ">"; //TODO: include node?
	}
	
	@Override
	public void encode(EOutputStream eos) {
		eos.write_port(node.node().getName(), id, creation);
	}

	public abstract EObject port_info(EAtom spec);

	public abstract void set_data(EObject data);
	public abstract EObject get_data();

	public abstract void close() throws Pausable;
}
