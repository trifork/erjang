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

import erjang.driver.EDriverTask;
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
	
	/** network driver exited! 
	 * @throws Pausable */
	public void node_going_down(EHandle sender, EObject reason)  {
		
		for (Map.Entry<EHandle,AtomicInteger> ent : node_monitors.entrySet()) {
			EHandle handle = ent.getKey();
			AtomicInteger howmany = ent.getValue();
			
			ETuple nd = ETuple.make(am_nodedown, this.node());
			while (howmany.decrementAndGet() >= 0) {			
				handle.sendb(sender, nd);
			}
		}
		
		node_monitors.clear();
		
	}

	public abstract EObject dsig_reg_send(EInternalPID caller, EAtom name,
			EObject msg) ;

	public abstract void dsig_demonitor(EHandle sender, ERef ref,
			EObject to_pid_or_name);

	
	
	public static EAbstractNode get_or_connect(EProc proc, EAtom n) throws Pausable {
		EAbstractNode res = EPeer.get(n);
		if (res == null && (proc instanceof EProc)) {			
			if (ErlDist.net_kernel__connect__1.invoke((EProc) proc, new EObject[] { n }) == ERT.TRUE) {
				return EPeer.get(n);
			} 
		}
		return res;
	}

	public static EAbstractNode get_or_connect(EDriverTask proc, EAtom n) {
		EAbstractNode res = EPeer.get(n);
		return res;
	}
}
