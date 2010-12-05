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

import kilim.Pausable;
import erjang.BIF;
import erjang.EAbstractNode;
import erjang.EAtom;
import erjang.EFun;
import erjang.EHandle;
import erjang.EInternalPID;
import erjang.EInternalPort;
import erjang.ENode;
import erjang.EObject;
import erjang.EPeer;
import erjang.EProc;
import erjang.ERT;
import erjang.ERef;
import erjang.ESeq;
import erjang.ESmall;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ErlangError;
import erjang.Import;
import erjang.NotImplemented;
import erjang.BIF.Type;
import erjang.driver.EDriverTask;

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
	

	@Import(module="net_kernel", fun="connect", arity=1)
	public static EFun net_kernel__connect__1;
	
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
	public static EObject monitor_node(EProc proc, EObject node, EObject flag, EObject opts) throws Pausable {
		
		EAtom aname = node.testAtom();
		EAtom aflag = flag.testAtom();
		ESeq sopts = opts.testSeq();
		
		if (aname == null || aflag == null || sopts == null) {
			throw ERT.badarg(node, flag);
		}
		
		EAbstractNode n = EPeer.get(aname);
		if (n == null) {
			return dmonitor_node3_trap.invoke(proc, new EObject[]{aname, aflag, opts});
		} else {			
			n.monitor_node(proc.self_handle(), aflag==ERT.TRUE);
			return ERT.TRUE;
		}
		
	}

	@BIF
	public static EObject monitor_node(EProc proc, EObject node, EObject flag) throws Pausable {
		return monitor_node(proc, node, flag, ERT.NIL);
	}

	@BIF
	public static EObject dist_exit(EObject a1, EObject a2, EObject a3) {
		throw new NotImplemented();
	}
	
	@BIF
	static public EAtom node() {
		EAtom val = ERT.getLocalNode().node();
		return val;
	}

	@BIF
	static public EAtom node(EObject name) {
		
		if (!ERT.getLocalNode().isALive()) {
			return ENode.am_nonode_at_nohost;
		}
		
		ERef ref;
		if ((ref=name.testReference()) != null) 
			return ref.node();
		
		EHandle handle;
		if ((handle=name.testHandle()) != null) 
			return handle.node();
		
		throw ERT.badarg(name);
	}

	@BIF(type = Type.GUARD, name = "node")
	static public EAtom node$p(EObject name) {
		
		if (!ERT.getLocalNode().isALive()) {
			return ENode.am_nonode_at_nohost;
		}
		
		EHandle handle;
		if ((handle=name.testHandle()) != null) {
			return handle.node();
		}
		
		ERef ref;
		if ((ref=name.testReference()) != null) {
			return ref.node();
		}
		
		return null;
	}

	@BIF
	public static EObject setnode(EObject arg_node, EObject arg_creation)
		throws Pausable
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

		EAbstractNode n = EPeer.get(node);
		n.node_up(null/*??*/, ERT.NIL);

		return ERT.TRUE;
	}

	private static void set_this_node(EAtom node, int value) {
		ERT.getLocalNode().set(node, value);
	}

	private static Pattern node_name_regex = Pattern.compile("([a-zA-Z0-9_]|-)+@[^@]+");
	public static boolean is_node_name_atom(EAtom node) {
		if (node_name_regex.matcher(node.getName()).matches()) {
			return true;
		} else {
			return false;
		}
	}

	/**********************************************************************
	 ** Allocate a dist entry, set node name install the connection handler
	 ** setnode_3({name@host, Creation}, Cid, {Type, Version, Initial, IC, OC})
	 ** Type = flag field, where the flags are specified in dist.h
	 ** Version = distribution version, >= 1
	 ** IC = in_cookie (ignored)
	 ** OC = out_cookie (ignored)
	 **
	 ** Note that in distribution protocols above 1, the Initial parameter
	 ** is always NIL and the cookies are always the atom '', cookies are not
	 ** sent in the distribution messages but are only used in 
	 ** the handshake.
	 **
	 ***********************************************************************/

	@BIF
	public static EObject setnode(EObject node_arg, EObject cid_arg, EObject type_arg)
		throws Pausable
	{
		//System.err.println("SETNODE("+node_arg+", "+cid_arg+", "+type_arg+")");
		
		EAtom node;
		int creation = 0;
		
		ETuple2 tup;
		if ((tup=ETuple2.cast(node_arg)) != null) {
			node = tup.elem1.testAtom();
			ESmall sm = tup.elem2.testSmall();
			
			if (node == null || sm==null || !is_node_name_atom(node)) 
				throw ERT.badarg(node_arg, cid_arg, type_arg);
			
			creation = sm.value;
		} else if ((node=node_arg.testAtom()) != null && is_node_name_atom(node)) {
			// ok
		} else {
			throw ERT.badarg(node_arg, cid_arg, type_arg);
		}
		
		/** first arg is ok */
		
		EInternalPort port = cid_arg.testInternalPort();
		if (port == null) {
			throw ERT.badarg(node_arg, cid_arg, type_arg);
		}

		ETuple t = type_arg.testTuple();
		if (t.arity() != 4) {
			throw ERT.badarg(node_arg, cid_arg, type_arg);
		}
		
		ESmall flags = t.elm(1).testSmall();
		ESmall version = t.elm(2).testSmall();
		if (flags == null || version == null) {
			throw ERT.badarg(node_arg, cid_arg, type_arg);
		}

		EPeer n = EPeer.get_or_create(node, creation, port, flags.value, version.value);
		
		EDriverTask task = port.task();
		if (task != null) { 
			task.node(n);

			/*TODO:
    send_nodes_mon_msgs(BIF_P,
                        am_nodeup,
                        BIF_ARG_1,
                        flags & DFLAG_PUBLISHED ? am_visible : am_hidden,
                        NIL);
			*/

			n.node_up(null/*??*/, ERT.NIL);

			return ERT.TRUE;
		} else {
			return ERT.FALSE;
		}
	}

}
