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

package erjang.m.erlang;

import java.util.regex.Pattern;

import erjang.BIF;
import erjang.EAtom;
import erjang.EFun;
import erjang.EInternalPID;
import erjang.EInternalPort;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.ESmall;
import erjang.ErlangError;
import erjang.Import;
import erjang.NotImplemented;

/**
 * BIFs supporting distribution
 */
public class ErlDist {

	private static DistEntry this_dist_entry = null;
	private static final EAtom am_net_kernel = EAtom.intern("net_kernel");
	private static final EAtom am_Noname = EAtom.intern("Noname");

	// initialization from node_tables
	static {
		
		this_dist_entry = new DistEntry(am_Noname, (EInternalPort) null);
		
	}
	
	
	/** distribution traps */
	
	@Import(module="erlang", fun="dsend", arity=2)
	static EFun dsend2_trap;
	
	@Import(module="erlang", fun="dsend", arity=3)
	static EFun dsend3_trap;
	
	@Import(module="erlang", fun="dlink", arity=1)
	static EFun dlink1_trap;
	
	@Import(module="erlang", fun="dunlink", arity=1)
	static EFun dunlink1_trap;
	
	@Import(module="erlang", fun="dmonitor_node", arity=3)
	static EFun dmonitor_node3_trap;
	
	@Import(module="erlang", fun="dgroup_leader", arity=2)
	static EFun dgroup_leader2_trap;
	
	@Import(module="erlang", fun="dexit", arity=2)
	static EFun dexit2_trap;

	@Import(module="erlang", fun="dmonitor_p", arity=2)
	static EFun dmonitor_p2_trap;

	static EAtom erts_is_alive = ERT.FALSE;

	@BIF
	public static final EObject is_alive() {
		return erts_is_alive;
	}
	
	@BIF
	public static EObject nodes(EObject node) {
		throw new NotImplemented();
	}

	@BIF
	public static EObject monitor_node(EObject a1, EObject a2, EObject a3) {
		throw new NotImplemented();
	}

	@BIF
	public static EObject dist_exit(EObject a1, EObject a2, EObject a3) {
		throw new NotImplemented();
	}
	
	@BIF
	public static EObject setnode(EObject arg_node, EObject arg_creation)
	{
		int creation;
		ESmall cr = arg_creation.testSmall();
		EAtom node = arg_node.testAtom();
		
		if (cr == null || node == null || cr.value > 3 || !is_node_name_atom(node)) {
			System.err.println("cr="+cr+"; node="+node+"; is_name="+is_node_name_atom(node));
			throw ERT.badarg(arg_node, arg_creation);
		}

		EObject net_kernel = ERT.whereis(am_net_kernel);		
		EInternalPID nk = net_kernel.testInternalPID();
		if (nk == null) {
			throw new ErlangError(EAtom.intern("no_net_kernel"));
		}
		
		nk.set_dist_entry(this_dist_entry);
		// nk.add_flag( F_DISTRIBUTION );
		
		set_this_node(node, cr.value);
		erts_is_alive = ERT.TRUE;
		
		return ERT.TRUE;
	}

	private static void set_this_node(EAtom node, int value) {
		ERT.getLocalNode().set(node, value);
	}

	private static Pattern node_name_regex = Pattern.compile("([a-zA-Z0-9_]|-)+@[^@]+");
	private static boolean is_node_name_atom(EAtom node) {
		if (node_name_regex.matcher(node.getName()).matches()) {
			return true;
		} else {
			return false;
		}
	}

}
