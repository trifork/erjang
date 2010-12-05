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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import erjang.m.erlang.ErlDist;

import kilim.Pausable;


/**
 * 
 */
public abstract class EAbstractNode {

    /**
	 * 
	 */
	public static final EAtom am_nonode_at_nohost = EAtom.intern("nonode@nohost");
	public static final EAtom am_nodedown = EAtom.intern("nodedown");
	public static final EAtom am_nodeup   = EAtom.intern("nodeup");
	public static final EAtom am_nodedown_reason = EAtom.intern("nodedown_reason");
	public static final EAtom am_node_type = EAtom.intern("node_type");
	public static final EAtom am_visible   = EAtom.intern("visible");
	public static final EAtom am_hidden    = EAtom.intern("hidden");
	public static final EAtom am_all       = EAtom.intern("all");

	static String localHost = null;
    EAtom node = am_nonode_at_nohost;
    String host;
    String alive;
    EAtom cookie;
    static EAtom defaultCookie = null;

    // Node types
    static final int NTYPE_R6 = 110; // 'n' post-r5, all nodes
    static final int NTYPE_R4_ERLANG = 109; // 'm' Only for source compatibility
    static final int NTYPE_R4_HIDDEN = 104; // 'h' Only for source compatibility


	public static final int ERTS_NODES_MON_OPT_TYPE_VISIBLE = 1<<0;
	public static final int ERTS_NODES_MON_OPT_TYPE_HIDDEN = 1<<1;
	public static final int ERTS_NODES_MON_OPT_DOWN_REASON = 1<<2;

	public static final int ERTS_NODES_MON_OPT_TYPES =
			ERTS_NODES_MON_OPT_TYPE_VISIBLE | ERTS_NODES_MON_OPT_TYPE_HIDDEN;

    // Node capability flags
    static final int dFlagPublished = 1;
    static final int dFlagAtomCache = 2;
    static final int dFlagExtendedReferences = 4;
    static final int dFlagDistMonitor = 8;
    static final int dFlagFunTags = 0x10;
    static final int dFlagDistMonitorName = 0x20; // NOT USED
    static final int dFlagHiddenAtomCache = 0x40; // NOT SUPPORTED
    static final int dflagNewFunTags = 0x80;
    static final int dFlagExtendedPidsPorts = 0x100;
    static final int dFlagExportPtrTag = 0x200; // NOT SUPPORTED
    static final int dFlagBitBinaries = 0x400;
    static final int dFlagNewFloats = 0x800;

    static final int dFUnicodeIO = 0x1000;
    static final int dFDistHdrAtomCache = 0x2000;
    static final int dFlagSmallAtoms = 0x4000;

    int ntype = NTYPE_R6;
    int proto = 0; // tcp/ip
    int distHigh = 5; // Cannot talk to nodes before R6
    int distLow = 5; // Cannot talk to nodes before R6
    int creation = 0;
    int flags = dFlagExtendedReferences | dFlagExtendedPidsPorts
            | dFlagBitBinaries | dFlagNewFloats | dFlagFunTags
            | dflagNewFunTags;

    
    /* initialize hostname and default cookie */
    static {
        try {
            localHost = InetAddress.getLocalHost().getHostName();
            /*
             * Make sure it's a short name, i.e. strip of everything after first
             * '.'
             */
            final int dot = localHost.indexOf(".");
            if (dot != -1) {
                localHost = localHost.substring(0, dot);
            }
        } catch (final UnknownHostException e) {
            localHost = "localhost";
        }

        final String dotCookieFilename = System.getProperty("user.home")
                + File.separator + ".erlang.cookie";
        BufferedReader br = null;

        try {
            final File dotCookieFile = new File(dotCookieFilename);

            br = new BufferedReader(new FileReader(dotCookieFile));
            defaultCookie = EAtom.intern( br.readLine().trim() );
        } catch (final IOException e) {
            defaultCookie = EAtom.intern("");
        } finally {
            try {
                if (br != null) {
                    br.close();
                }
            } catch (final IOException e) {
            }
        }
    }

	
	/**
	 * @param node
	 */
	public EAbstractNode(EAtom node) {
		this(node, defaultCookie);
	}

	/**
	 * 
	 */
	public EAbstractNode() {
		this(am_nonode_at_nohost);
	}

	/**
	 * @param node
	 * @param cookie
	 */
	public EAbstractNode(EAtom node, EAtom cookie) {

        this.cookie = cookie;
        set(node, 0);
	
	}
	
	public void set(EAtom node, int cr) {
	

        String name = node.getName();

        final int i = name.indexOf('@', 0);
        if (i < 0) {
            alive = name;
            host = localHost;
        } else {
            alive = name.substring(0, i);
            host = name.substring(i + 1, name.length());
        }

        if (alive.length() > 0xff) {
            alive = alive.substring(0, 0xff);
        }

        this.node = EAtom.intern( alive + "@" + host );

        this.creation = cr;
		
	}


	
    /**
     * Get the name of this node.
     * 
     * @return the name of the node represented by this object.
     */
    public EAtom node() {
        return node;
    }

    /**
     * Get the hostname part of the nodename. Nodenames are composed of two
     * parts, an alivename and a hostname, separated by '@'. This method returns
     * the part of the nodename following the '@'.
     * 
     * @return the hostname component of the nodename.
     */
    public String host() {
        return host;
    }

    /**
     * Get the alivename part of the hostname. Nodenames are composed of two
     * parts, an alivename and a hostname, separated by '@'. This method returns
     * the part of the nodename preceding the '@'.
     * 
     * @return the alivename component of the nodename.
     */
    public String alive() {
        return alive;
    }

    /**
     * Get the authorization cookie used by this node.
     * 
     * @return the authorization cookie used by this node.
     */
    public EAtom cookie() {
        return cookie;
    }

    // package scope
    int type() {
        return ntype;
    }

    // package scope
    int distHigh() {
        return distHigh;
    }

    // package scope
    int distLow() {
        return distLow;
    }

    // package scope: useless information?
    int proto() {
        return proto;
    }

    // package scope
    int creation() {
        return creation;
    }

    /**
     * Set the authorization cookie used by this node.
     * 
     * @return the previous authorization cookie used by this node.
     */
    public EAtom setCookie(final EAtom cookie) {
        final EAtom prev = this.cookie;
        this.cookie = cookie;
        return prev;
    }


	/*==================== Monitoring of a specific node ====================*/
	ConcurrentHashMap<EHandle,AtomicInteger> node_monitors = new ConcurrentHashMap<EHandle,AtomicInteger>();

	public void monitor_node(EHandle caller, boolean on) {
		node_monitors.putIfAbsent(caller, new AtomicInteger());
		AtomicInteger ami = node_monitors.get(caller);
		if (on) {
			ami.incrementAndGet();
		} else {
			ami.decrementAndGet();
		}
	}

	/*==================== Monitoring of node changes in general ==========*/

	static ConcurrentHashMap<EHandle,ConcurrentHashMap<Integer,AtomicInteger> > nodes_monitors = new ConcurrentHashMap();
	

	/** @return whether monitoring was already active for the given opts_list,
	 *          or null if the opts_list was invalid.
	 */
	public static Boolean monitor_nodes(EHandle caller, boolean on, ESeq opts_list) {
		boolean all = false, visible = false, hidden = false;
		int opts = 0;
		
		for (; !opts_list.isNil(); opts_list = opts_list.tail()) {
			EObject opt = opts_list.head();

			ETuple2 tp;
			if (opt == am_nodedown_reason) {
				opts |= ERTS_NODES_MON_OPT_DOWN_REASON;
			} else if ((tp = ETuple2.cast(opt)) != null) {
				if (tp.elem1 == am_node_type) {
		
					if (tp.elem2 == am_visible) {
		
						if (hidden || all) return null;
						opts |= ERTS_NODES_MON_OPT_TYPE_VISIBLE;
						visible = true;
						
					} else if (tp.elem2 == am_hidden) {
						if (visible || all) return null;
						opts |= ERTS_NODES_MON_OPT_TYPE_HIDDEN;
						hidden = true;
						
					} else if (tp.elem2 == am_all) {
						if (visible || hidden) return null;
						opts |= ERTS_NODES_MON_OPT_TYPES;
					} else {
						return null;
					}
					
				} else {
					return null;
				}
				
			} else {
				return null;
			}
			
		}

		return Boolean.valueOf(monitor_nodes(caller, on, opts));
	}

	public static boolean monitor_nodes(EHandle caller, boolean on, int opts) {
		if (on) nodes_monitors.putIfAbsent(caller, new ConcurrentHashMap());
		ConcurrentHashMap<Integer,AtomicInteger> forHandle = nodes_monitors.get(caller);
		if (forHandle==null) return false; // We're disabling but entry wasn't there.

		if (on) {
			forHandle.putIfAbsent(opts, new AtomicInteger(0));
			AtomicInteger ami = forHandle.get(opts);
			if (ami==null) return false; // We're disabling but entry wasn't there.
 			int old = ami.getAndIncrement();
			return old > 0;
		} else {
			AtomicInteger old = forHandle.remove(opts);
			return old != null && old.get() > 0;
		}
	}

	/** network driver exited! 
	 * @throws Pausable */
	public void node_going_down(EHandle sender, EObject reason) throws Pausable {
		
		for (Map.Entry<EHandle,AtomicInteger> ent : node_monitors.entrySet()) {
			EHandle handle = ent.getKey();
			AtomicInteger howmany = ent.getValue();
			
			ETuple nd = ETuple.make(am_nodedown, this.node());
			while (howmany.decrementAndGet() >= 0) {			
				handle.send(sender, nd);
			}
		}
		
		node_monitors.clear();
		
	}

	public void node_up(EHandle sender, EObject reason) throws Pausable {
		// TODO: Send only to those which subscribe to the kind of node in question (visible/hidden).
		for (Map.Entry<EHandle,ConcurrentHashMap<Integer,AtomicInteger> > ent : nodes_monitors.entrySet()) {
			EHandle handle = ent.getKey();
			ConcurrentHashMap<Integer,AtomicInteger> submap = ent.getValue();
			// TODO: Add exit hook under the right circumstances.

			for (Map.Entry<Integer,AtomicInteger> subent : submap.entrySet()) {
				int opts = subent.getKey().intValue();
				int howmany = subent.getValue().get();

				final ETuple msg;
				if (opts == 0) {
					msg = ETuple.make(am_nodeup, this.node());
				} else {
					/* TODO: Respect opts.
					 * (1) Don't send if opts and type don't agree
					 * (2) Add to the property list in the third element:
					 * optionally with {'node_type', Type},
					 * optionally with {'nodedown_reason', Reason}.
					 */
					ECons info = ERT.NIL;
					// TODO: construct correct info value
					msg = ETuple.make(am_nodeup, this.node(), info);
				}
				for (int i=0; i < howmany; i++) {
					handle.send(sender, msg);
				}
			}
		}
	}

	public abstract EObject dsig_reg_send(EInternalPID caller, EAtom name,
			EObject msg) throws Pausable;

	public abstract void dsig_demonitor(EHandle sender, ERef ref,
			EObject to_pid_or_name) throws Pausable;

	
	
	public static EAbstractNode get_or_connect(ETask proc, EAtom n) throws Pausable {
		EAbstractNode res = EPeer.get(n);
		if (res == null && (proc instanceof EProc)) {			
			if (ErlDist.net_kernel__connect__1.invoke((EProc) proc, new EObject[] { n }) == ERT.TRUE) {
				return EPeer.get(n);
			} 
		}
		return res;
	}
}
