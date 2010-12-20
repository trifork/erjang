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

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Map;
import java.util.LinkedList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import erjang.driver.EDriverTask;

import kilim.Pausable;

/**
 * This corresponds to a DistEntry in BEAM
 */
public class EPeer extends EAbstractNode {

	protected static final byte passThrough = (byte) 0x70;
	protected static final byte distHeader = (byte) 131;
	protected static final byte version = (byte) 0x83;

	// Erlang message header tags
	protected static final int LINK = 1;
	protected static final int SEND = 2;
	protected static final int EXIT = 3;
	protected static final int UNLINK = 4;
	protected static final int NODE_LINK = 5;
	protected static final int REG_SEND = 6;
	protected static final int GROUP_LEADER = 7;
	protected static final int EXIT2 = 8;

	protected static final int SEND_TT = 12;
	protected static final int EXIT_TT = 13;
	protected static final int REG_SEND_TT = 16;
	protected static final int EXIT2_TT = 18;

	protected static final int MONITOR_P = 19;
	protected static final int DEMONITOR_P = 20;
	protected static final int MONITOR_P_EXIT = 21;
	private static final EAtom am_ = EAtom.intern("");

	static ConcurrentHashMap<EAtom, EPeer> peers = new ConcurrentHashMap<EAtom, EPeer>();
	/** Invariant: (port != null) xor (port_queue != null). */
	private EInternalPort port;
	private LinkedList<ByteBuffer[]> port_queue;

	public EPeer(EAtom node, int creation, EInternalPort port, int flags,
			int version) {
		super(node);
		this.flags = flags;
		this.creation = creation;
		this.port = port;
		this.ntype = version == 5 ? NTYPE_R6 : 0;
		this.port_queue = (this.port == null)
			? new LinkedList<ByteBuffer[]>() : null;
	}

	private void setPort(EInternalPort port) throws Pausable {
		assert(port != null);
		assert(this.port == null);
		assert(this.port_queue != null);
		//System.err.println("EPeer: Patching port for "+node+" to "+port);
		EDriverTask task = port.task();
		assert(task != null);
		/* Note: The following could be simpler if
		 * EDriverTask.outputv() wasn't Pausable - which it
		 * shouldn't be, but is as long as we're using
		 * bounded-size Mailboxes. -- eriksoe */

		try {
			// Empty port_queue, then set port:
			while (true) {
				ByteBuffer[] msg;
				synchronized (this) {
					if (port_queue.isEmpty()) { // Go from queueing mode to direct-port mode:
						this.port = port;
						this.port_queue = null;
						break;
					}
					msg = port_queue.removeFirst();
					assert(msg != null);
				}
				task.outputv(null, msg); // Pausable so called outside lock
			}
		} catch (IOException e) {
			e.printStackTrace();
			close_and_finish(port);
		}
	}

	private void send_to_port(ByteBuffer[] ev, EHandle sender) throws Pausable {
		// Either send to port, or enqueue...:
		EInternalPort port;
		synchronized (this) { // get port; enqueue if necessary
			port = this.port;
			if (port == null) {	// In queueing mode
				assert(port_queue != null);
				port_queue.addLast(ev);
				return;
			}
		}
		assert (port != null);	// In direct-to-port mode

		EDriverTask task = port.task();
		if (task != null) {
			try {
				task.outputv(null, ev);
			} catch (IOException e) {
				e.printStackTrace();
				close_and_finish(port);
			}
		} else {
			System.err.println("sending cast to dead task (port="+port+", this="+this+", sender="+sender+")");
		}
	}


	// TODO: who closes/deletes these peers?

	/** get_or_create - "no port" case. */
	public static EPeer get_or_create(EAtom node, int creation, int flags, int version) {
		EPeer peer = peers.get(node);
		if (peer != null) {
			// check version, etc?
			return peer;
		}

		peer = new EPeer(node, creation, null, flags, version);
		peers.put(node, peer);

		return peer;
	}

	/** get_or_create - "port given" case. */
	public static EPeer get_or_create(EAtom node, int creation,
			EInternalPort port, int flags, int version) throws Pausable {

		EPeer peer = peers.get(node);
		if (peer != null) {
			// check version, etc?

			boolean do_set_port;
			synchronized (peer) {
				do_set_port = (peer.port == null && port != null);
			}
			if (do_set_port) {
				peer.setPort(port);
			} else if (port != null) {
				System.err.println("Port already set for node (to "+peer.port+"); refraining from setting it again (to "+port+")");
			}

			return peer;
		}

		peer = new EPeer(node, creation, port, flags, version);
		peers.put(node, peer);

		return peer;
	}

	public void net_message(EInternalPort port, ByteBuffer hdr, ByteBuffer buf) throws Pausable {
		try {
			net_message2(port, hdr, buf);
		} catch (IOException e) {
			//e.printStackTrace();
			// TODO: this close and finish doesn't work right!
			close_and_finish(port);
		}
	}

	EAtom[][] atom_cache;

	{
		atom_cache = new EAtom[8][];
		for (int i = 0; i < 8; i++) {
			atom_cache[i] = new EAtom[256];
		}
	}

	public void net_message2(EInternalPort port, ByteBuffer hdr, ByteBuffer buf)
			throws IOException, Pausable {

		if (buf.remaining() == 0) {
			if (ERT.DEBUG_DIST) {
				System.err.println("received tick from " + this.node);
			}
			return;
		}

		if (hdr != null && hdr.remaining() != 0) {
			close_and_finish(port);
			return;
		}

		EInputStream ibuf = new EInputStream(buf.array(), buf.arrayOffset()
				+ buf.position(), buf.remaining(), flags);

		int start = ibuf.getPos();

		receive_loop: do {

			int first = ibuf.read1();
			EAtom[] atom_cache_refs;

			switch (first) {
			case 131:
				if (!process_distribution_header(port, ibuf))
					return;

			case passThrough:
				break;

			default:
				close_and_finish(port);
				return;
			}

			// got a real message (really)
			EObject reason = null;
			EAtom cookie = null;
			EObject tmp = null;
			ETuple head = null;
			EAtom toName;
			EPID to;
			EPID from;
			int tag;

			// decode the header
			tmp = ibuf.read_any();
			if ((head = tmp.testTuple()) == null) {
				break receive_loop;
			}

			if (ERT.DEBUG_DIST) {
				System.err.println("received net_message " + tmp);
			}
			
			ESmall tag_sm;
			if ((tag_sm = head.elm(1).testSmall()) == null) {
				break receive_loop;
			}

			// lets see what kind of message this is
			tag = tag_sm.value;

			switch (tag) {
			
			case LINK: { // {1, observer, observed }
				
				EPID from_pid = head.elm(2).testPID();
				EInternalPID to_pid = head.elm(3).testInternalPID();
				
				if (!to_pid.link_oneway(from_pid)) {
					
					dsig_exit(to_pid, from_pid, ERT.am_noproc);
					
				}
				return;
			}
			
			case UNLINK: { // {1, observer, observed }
				
				EPID from_pid = head.elm(2).testPID();
				EInternalPID to_pid = head.elm(3).testInternalPID();
				
				if (to_pid != null && from_pid != null) {
					to_pid.unlink_oneway(from_pid);
				}
				return;
			}
			
			case SEND: { // {2, Cookie, ToPid}

				EObject msg = ibuf.read_any();
				if (ERT.DEBUG_DIST)
					System.err.println("           payload: " + msg);
				EHandle dst = head.elm(3).testHandle();
				if (dst == null)
					throw new IOException("protocol error");
				if (dst != null) {
					dst.send(null, msg);
				}
				return;
			}

			case REG_SEND: // { REG_SEND, FromPid, Cookie, ToName }
			case REG_SEND_TT: // { REG_SEND, FromPid, Cookie, ToName, TraceToken
								// }
			{
				EObject msg = ibuf.read_any();
				if (ERT.DEBUG_DIST)
					System.err.println("           payload: " + msg);
				EAtom toname = head.elm(4).testAtom();
				if (toname == null)
					throw new IOException("protocol error");
				EHandle dst = ERT.whereis(toname).testHandle();
				if (dst != null) {
					dst.sendb(msg);
				}
				return;
			}
			
			case EXIT:
			case EXIT2:
			{
				EPID from_pid = head.elm(2).testPID();
				EPID to_proc = head.elm(3).testPID();
				reason = head.elm(4);

				to_proc.exit_signal(from_pid, reason, false);
				return;
				
			}
			

			case MONITOR_P: { // {19, FromPid, ToProc, Ref}
				// FromPid = monitoring process
				// ToProc = monitored process pid or name (atom)

				EPID from_pid = head.elm(2).testPID();
				EPID to_proc = head.elm(3).testPID();
				ERef ref = head.elm(4).testReference();

				if (to_proc == null) {
					EAtom am = head.elm(3).testAtom();
					if (am == null) {
						throw new IOException("protocol error: " + head);
					}
					to_proc = ERT.whereis(am).testPID();
				}
				if (to_proc == null) {
					throw new IOException("protocol error: " + head);
				}

				if (!to_proc.add_monitor(from_pid, ref)) {
					dsig_send_monitor_exit(to_proc, from_pid, ref, ERT.am_noproc);
				}
				return;
			}

			case DEMONITOR_P: {
				// {20, FromPid, ToProc, Ref}

				EPID from_pid = head.elm(2).testPID();
				EPID to_proc = head.elm(3).testPID();
				ERef ref = head.elm(4).testReference();

				if (to_proc == null) {
					EAtom am = head.elm(3).testAtom();
					if (am == null) {
						throw new IOException("protocol error: " + head);
					}
					to_proc = ERT.whereis(am).testPID();
				}
				if (to_proc == null) {
					throw new IOException("protocol error: " + head);
				}

				to_proc.remove_monitor(from_pid, ref, false);
				return;
			}
			
			case MONITOR_P_EXIT:
			{ // {21, FromProc, ToPid, Ref, Reason}
				
				EPID from_pid = head.elm(2).testPID();
				EPID to_proc = head.elm(3).testPID();
				ERef ref = head.elm(4).testReference();
				reason = head.elm(5);

				if (to_proc == null) {
					EAtom am = head.elm(3).testAtom();
					if (am == null) {
						throw new IOException("protocol error: " + head);
					}
					to_proc = ERT.whereis(am).testPID();
				}
				if (to_proc == null) {
					throw new IOException("protocol error: " + head);
				}

				to_proc.send_monitor_exit(from_pid, ref, reason);
				return;
			}

			default:
				throw new NotImplemented("dmesg: " + head);
			}

		} while (false); // end receive_loop
	}

	private boolean process_distribution_header(EInternalPort port,
			EInputStream ibuf) throws IOException {
		EAtom[] atom_cache_refs;

		if (ERT.DEBUG_DIST)
			System.err.println("parsing distribuionHeader....");
		
		int datum = ibuf.read1();

		if (datum != 68) {
			close_and_finish(port);
			return false;
		}
		
		int numberOfAtomCacheRefs = ibuf.read1() & 0xff;
		atom_cache_refs = new EAtom[numberOfAtomCacheRefs];

		if (numberOfAtomCacheRefs == 0) {
			// do nothing //
		} else {

			int nflag_bytes = numberOfAtomCacheRefs / 2 + 1;
			byte[] flags = new byte[nflag_bytes * 2];
			int pos = 0;
			for (int i = 0; i < nflag_bytes; i++) {
				int twoflags = ibuf.read1() & 0xff;
				// System.err.println("flag["+i+"]="+Integer.toBinaryString(twoflags));
				flags[pos++] = (byte) (twoflags & 0xf);
				flags[pos++] = (byte) (twoflags >>> 4);
				// System.err.println("flags["+(pos-2)+"]="+Integer.toBinaryString(flags[pos-2]));
				// System.err.println("flags["+(pos-1)+"]="+Integer.toBinaryString(flags[pos-1]));
			}

			boolean longAtoms = (flags[numberOfAtomCacheRefs] & 0x01) == 1;

			if (longAtoms) {
				System.err.println("LONGATOMS!");
			}

			for (int i = 0; i < numberOfAtomCacheRefs; i++) {

				int segment_index = flags[i] & 7;
				int index = ibuf.read1();

				if (ERT.DEBUG_DIST)
				System.err.print("cache[" + i + "] -> ref["
						+ segment_index + "][" + index + "]");

				if ((flags[i] & 8) == 8) {
					// it's new!

					int len = ibuf.read1() & 0xff;
					if (longAtoms) {
						len <<= 8;
						len |= (ibuf.read1() & 0xff);
					}
					byte[] atom_text = new byte[len];
					ibuf.read(atom_text);

					atom_cache[segment_index][index] = EAtom
							.intern(atom_text);

				} else {
					// it's old
				}

				atom_cache_refs[i] = atom_cache[segment_index][index];

				if (ERT.DEBUG_DIST)
					System.err.println(" => " + atom_cache_refs[i]);
			}

			ibuf.setAtomCacheRefs(atom_cache_refs);
		}
		
		return true;
	}

	private void close_and_finish(EInternalPort port) {
		port.task().exit(ERT.am_killed);
	}

	public static EAbstractNode get(EAtom node) {
		if (node == ERT.getLocalNode().node())
			return ERT.getLocalNode();
		return peers.get(node);
	}

	void dsig_cast(EHandle sender, ETuple hdr) throws Pausable {

		ByteBuffer disthdr = ByteBuffer.allocate(3);
		disthdr.put((byte) 131);
		disthdr.put((byte) 68);
		disthdr.put((byte) 0);
		disthdr.flip();

		EOutputStream eos = new EOutputStream(1024,flags);
		hdr.encode(eos);

		ByteBuffer barr = eos.toByteBuffer();

		ByteBuffer[] ev = new ByteBuffer[] { disthdr, barr };

		send_to_port(ev, sender);
	}

	int dsig_cast(EHandle sender, ETuple hdr, EObject payload) throws Pausable {

		ByteBuffer disthdr = ByteBuffer.allocate(3);
		disthdr.put((byte) 131);
		disthdr.put((byte) 68);
		disthdr.put((byte) 0);
		disthdr.flip();

		EOutputStream eos = new EOutputStream(1024,flags);
		hdr.encode(eos);
		payload.encode(eos);

		ByteBuffer barr = eos.toByteBuffer();

		ByteBuffer[] ev = new ByteBuffer[] { disthdr, barr };

		send_to_port(ev, sender);

		return eos.size();

	}

	public int dsig_send(EHandle sender, EExternalPID pid, EObject msg) throws Pausable {
		ETuple hdr = ETuple.make(ERT.box(SEND), (EAtom) am_, pid);
		return dsig_cast(sender, hdr, msg);
	}

	public void dsig_send_monitor_exit(EHandle sender, EPID to_pid,
			ERef ref, EObject reason) throws Pausable {

		ETuple hdr = ETuple.make(ERT.box(MONITOR_P_EXIT), sender, to_pid,
								 ref, reason);
		dsig_cast(sender, hdr);
	}

	public void dsig_monitor(EHandle sender, EObject to_pid, ERef ref) throws Pausable {

		ETuple hdr = ETuple.make(ERT.box(MONITOR_P), sender, to_pid, ref);
		dsig_cast(sender, hdr);

	}

	public void dsig_exit(EHandle sender, EPID to_pid,
			EObject reason) throws Pausable {
		ETuple hdr = ETuple.make(ERT.box(EXIT), sender, to_pid, reason);
		dsig_cast(sender, hdr);
		
	}

	public void dsig_link(EHandle sender, EExternalPID to_pid) throws Pausable {
		ETuple hdr = ETuple.make(ERT.box(LINK), sender, to_pid);
		dsig_cast(sender, hdr);
	}

	public void dsig_unlink(EHandle sender, EExternalPID to_pid) throws Pausable {
		ETuple hdr = ETuple.make(ERT.box(UNLINK), sender, to_pid);
		dsig_cast(sender, hdr);
	}

	public void dsig_demonitor(EHandle sender, ERef ref,
			EObject to_pid_or_name) throws Pausable {
		ETuple hdr = ETuple.make(ERT.box(DEMONITOR_P), sender, to_pid_or_name, ref);
		dsig_cast(sender, hdr);
	}

	public EObject dsig_reg_send(EInternalPID sender, EAtom to_name, EObject msg) throws Pausable {
		ETuple hdr = ETuple.make(ERT.box(REG_SEND), sender, am_, to_name);
		dsig_cast(sender, hdr, msg);
		return msg;
	}

	public static ESeq getRemoteNodes() {
		EAtom[] nodes = peers.keySet().toArray(new EAtom[0]);

		ESeq reply = ERT.NIL;
		for (int i = 0; i < nodes.length; i++) {
			reply = reply.cons(nodes[i]);
		}
		return reply;
	}

	

}
