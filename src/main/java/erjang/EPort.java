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

public abstract class EPort extends EHandle {

	/* (non-Javadoc)
	 * @see erjang.EObject#cmpOrder()
	 */
	@Override
	int cmp_order() {
		return 4;
	}
	/**
	 * @return
	 */
	public EString getName() {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	int compare_same(EObject rhs) {
		
		// TODO: make faster
		return toString().compareTo(rhs.toString());
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

}
