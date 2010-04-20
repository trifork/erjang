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
import java.util.concurrent.ConcurrentHashMap;

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
	private EInternalPort port;

	public EPeer(EAtom node, int creation, EInternalPort port, int flags,
			int version) {

		super(node);
		this.flags = flags;
		this.creation = creation;
		this.port = port;
		this.ntype = version == 5 ? NTYPE_R6 : 0;
	}

	// TODO: who closes/deletes these peers?

	public static EPeer get_or_create(EAtom node, int creation,
			EInternalPort port, int flags, int version) {

		EPeer peer = peers.get(node);
		if (peer != null) {
			// check version, etc?
			return peer;
		}

		peer = new EPeer(node, creation, port, flags, version);
		peers.put(node, peer);

		return peer;

	}

	public void net_message(EInternalPort port, ByteBuffer hdr, ByteBuffer buf) {
		try {
			net_message2(port, hdr, buf);
		} catch (IOException e) {
			e.printStackTrace();
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
			throws IOException {

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
				if (ERT.DEBUG_DIST) 
					System.err.println("parsing distribuionHeader....");
				int datum = ibuf.read1();

				if (datum != 68) {
					close_and_finish(port);
					return;
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

						/*
						System.err.print("cache[" + i + "] -> ref["
								+ segment_index + "][" + index + "]");
						 */
						
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

						/*
						System.err.println(" => " + atom_cache_refs[i]);
						*/
					}

					ibuf.setAtomCacheRefs(atom_cache_refs);
				}

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
			
			case SEND: { // {2, Cookie, ToPid}

				EObject msg = ibuf.read_any();
				if (ERT.DEBUG_DIST)
					System.err.println("           payload: " + msg);
				EHandle dst = head.elm(3).testHandle();
				if (dst == null)
					throw new IOException("protocol error");
				if (dst != null) {
					dst.sendb(msg);
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

				to_proc.exit_signal(from_pid, reason);
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

			default:
				throw new NotImplemented("dmesg: " + head);
			}

		} while (false); // end receive_loop
	}

	private void close_and_finish(EInternalPort port) {
		port.task().exit(ERT.am_killed);
	}

	public static EAbstractNode get(EAtom node, int creation) {
		return peers.get(node);
	}

	void dsig_cast(EHandle sender, ETuple hdr) {

		ByteBuffer disthdr = ByteBuffer.allocate(3);
		disthdr.put((byte) 131);
		disthdr.put((byte) 68);
		disthdr.put((byte) 0);
		disthdr.flip();

		EOutputStream eos = new EOutputStream();
		hdr.encode(eos);

		ByteBuffer barr = eos.toByteBuffer();

		ByteBuffer[] ev = new ByteBuffer[] { disthdr, barr };

		try {
			this.port.task().outputv(sender, ev);
		} catch (IOException e) {
			e.printStackTrace();
			close_and_finish(port);
		}

	}

	void dsig_cast(EHandle sender, ETuple hdr, EObject payload) {

		ByteBuffer disthdr = ByteBuffer.allocate(3);
		disthdr.put((byte) 131);
		disthdr.put((byte) 68);
		disthdr.put((byte) 0);
		disthdr.flip();

		EOutputStream eos = new EOutputStream();
		hdr.encode(eos);
		payload.encode(eos);

		ByteBuffer barr = eos.toByteBuffer();

		ByteBuffer[] ev = new ByteBuffer[] { disthdr, barr };

		try {
			this.port.task().outputv(sender, ev);
		} catch (IOException e) {
			e.printStackTrace();
			close_and_finish(port);
		}

	}

	public void dsig_send(EHandle sender, EExternalPID pid, EObject msg) {
		ETuple hdr = ETuple.make(ERT.box(SEND), (EAtom) am_, pid);
		dsig_cast(sender, hdr, msg);
	}

	public void dsig_send_monitor_exit(EHandle sender, EPID to_pid,
			ERef ref, EObject reason) {

		ETuple hdr = ETuple.make(ERT.box(MONITOR_P_EXIT), sender, to_pid,
				reason);
		dsig_cast(sender, hdr);
	}

	public void dsig_monitor(EHandle sender, EExternalPID to_pid, ERef ref) {

		ETuple hdr = ETuple.make(ERT.box(MONITOR_P), sender, to_pid, ref);
		dsig_cast(sender, hdr);

	}

	public void dsig_exit(EHandle sender, EPID to_pid,
			EObject reason) {
		ETuple hdr = ETuple.make(ERT.box(EXIT), sender, to_pid, reason);
		dsig_cast(sender, hdr);
		
	}

	public void dsig_link(EHandle sender, EExternalPID to_pid) {
		ETuple hdr = ETuple.make(ERT.box(LINK), sender, to_pid);
		dsig_cast(sender, hdr);
	}

	public void dsig_demonitor(EHandle sender, ERef ref,
			EExternalPID to_pid) {
		ETuple hdr = ETuple.make(ERT.box(DEMONITOR_P), sender, to_pid, ref);
		dsig_cast(sender, hdr);
	}

}
