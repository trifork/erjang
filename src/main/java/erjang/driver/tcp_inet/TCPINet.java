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

/**
 * TODO
 * 
 * - add locks
 * - sock_select
 * 
 */

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.AsynchronousCloseException;
import java.nio.channels.CancelledKeyException;
import java.nio.channels.ClosedByInterruptException;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.GatheringByteChannel;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.SocketChannel;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.concurrent.atomic.AtomicInteger;

import kilim.Pausable;
import kilim.RingQueue;
import erjang.EAtom;
import erjang.EBinList;
import erjang.EBinary;
import erjang.EHandle;
import erjang.EInternalPID;
import erjang.EInternalPort;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.ERT;
import erjang.ERef;
import erjang.EString;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ErlangError;
import erjang.NotImplemented;
import erjang.driver.EAsync;
import erjang.driver.EDriverInstance;
import erjang.driver.EDriverTask;
import erjang.driver.IO;
import erjang.driver.NIOSelector;
import erjang.driver.SelectMode;
import erjang.driver.efile.Posix;
import erjang.net.InetSocket;
import erjang.net.Protocol;
import erjang.net.ProtocolFamily;
import erjang.net.ProtocolType;

public class TCPINet extends EDriverInstance implements java.lang.Cloneable {

	public class AsyncMultiOp {

		public AsyncOp op;
		public MultiTimerData timeout;
		public AsyncMultiOp next;

	}

	static class MultiTimerData implements Comparable<MultiTimerData>{

		public long when;
		public EPID caller;

		@Override
		public int compareTo(MultiTimerData o) {
			if ( this.when < o.when )
				return -1;
			if ( this.when > o.when )
				return 1;
			return 0;
		}

	}

	static class AsyncOp {

		public AsyncOp(short id, EPID caller, int req, int timeout,
				ERef monitor) {
			this.id = id;
			this.caller = caller;
			this.req = req;
			this.monitor = monitor;
			this.timeout = timeout;
		}

		/** id used to identify reply */
		final short id;

		/** recipient of async reply */
		final EPID caller;

		/* Request id (CONNECT/ACCEPT/RECV) */
		final int req;

		final ERef monitor;

		final public int timeout;
	}

	static enum SockType {
		SOCK_STREAM, SOCK_DGRAM, SOCK_SEQPACKET;
	}

	static enum ActiveType {
		PASSIVE(0), ACTIVE(1), ACTIVE_ONCE(2);
		final int value;

		ActiveType(int val) {
			this.value = val;
		}

		public static ActiveType valueOf(int ival) {
			switch (ival) {
			case 0:
				return PASSIVE;
			case 1:
				return ACTIVE;
			case 2:
				return ACTIVE_ONCE;
			}

			throw new erjang.NotImplemented();
		}
	}

	/* general address encode/decode tag */
	public static final byte INET_AF_INET = 1;
	public static final byte INET_AF_INET6 = 2;
	public static final byte INET_AF_ANY = 3; /* INADDR_ANY or IN6ADDR_ANY_INIT */
	public static final byte INET_AF_LOOPBACK = 4; /*
													 * INADDR_LOOPBACK or
													 * IN6ADDR_LOOPBACK_INIT
													 */

	/* INET_REQ_GETTYPE enumeration */
	public static final int INET_TYPE_STREAM = 1;
	public static final int INET_TYPE_DGRAM = 2;
	public static final int INET_TYPE_SEQPACKET = 3;

	/* INET_LOPT_MODE options */
	public static final int INET_MODE_LIST = 0;
	public static final int INET_MODE_BINARY = 1;

	/* INET_LOPT_DELIVER options */
	public static final int INET_DELIVER_PORT = 0;
	public static final int INET_DELIVER_TERM = 1;

	/* INET_LOPT_ACTIVE options */
	public static final int INET_PASSIVE = 0; /* false */
	public static final int INET_ACTIVE = 1; /* true */
	public static final int INET_ONCE = 2; /* true; active once then passive */

	/* INET_REQ_GETSTATUS enumeration */
	public static final int INET_F_OPEN = 0x0001;
	public static final int INET_F_BOUND = 0x0002;
	public static final int INET_F_ACTIVE = 0x0004;
	public static final int INET_F_LISTEN = 0x0008;
	public static final int INET_F_CON = 0x0010;
	public static final int INET_F_ACC = 0x0020;
	public static final int INET_F_LST = 0x0040;
	public static final int INET_F_BUSY = 0x0080;
	public static final int INET_F_MULTI_CLIENT = 0x0100; /*
														 * Multiple clients for
														 * one descriptor, i.e.
														 * multi-accept
														 */

	/*
	 * One numberspace for *_REC_* so if an e.g UDP request is issued* for a TCP
	 * socket, the driver can protest.
	 */
	public static final int INET_REQ_OPEN = 1;
	public static final int INET_REQ_CLOSE = 2;
	public static final int INET_REQ_CONNECT = 3;
	public static final int INET_REQ_PEER = 4;
	public static final int INET_REQ_NAME = 5;
	public static final int INET_REQ_BIND = 6;
	public static final int INET_REQ_SETOPTS = 7;
	public static final int INET_REQ_GETOPTS = 8;
	/* public static final int INET_REQ_GETIX 9 NOT USED ANY MORE */
	/* public static final int INET_REQ_GETIF 10 REPLACE BY NEW STUFF */
	public static final int INET_REQ_GETSTAT = 11;
	public static final int INET_REQ_GETHOSTNAME = 12;
	public static final int INET_REQ_FDOPEN = 13;
	public static final int INET_REQ_GETFD = 14;
	public static final int INET_REQ_GETTYPE = 15;
	public static final int INET_REQ_GETSTATUS = 16;
	public static final int INET_REQ_GETSERVBYNAME = 17;
	public static final int INET_REQ_GETSERVBYPORT = 18;
	public static final int INET_REQ_SETNAME = 19;
	public static final int INET_REQ_SETPEER = 20;
	public static final int INET_REQ_GETIFLIST = 21;
	public static final int INET_REQ_IFGET = 22;
	public static final int INET_REQ_IFSET = 23;
	public static final int INET_REQ_SUBSCRIBE = 24;
	/* TCP requests */
	public static final int TCP_REQ_ACCEPT = 40;
	public static final int TCP_REQ_LISTEN = 41;
	public static final int TCP_REQ_RECV = 42;
	public static final int TCP_REQ_UNRECV = 43;
	public static final int TCP_REQ_SHUTDOWN = 44;
	public static final int TCP_REQ_MULTI_OP = 45;
	/* UDP and SCTP requests */
	public static final int PACKET_REQ_RECV = 60; /* Common for UDP and SCTP */
	public static final int SCTP_REQ_LISTEN = 61; /*
												 * Different from TCP; not for
												 * UDP
												 */
	public static final int SCTP_REQ_BINDX = 62; /* Multi-home SCTP bind */

	/* INET_REQ_SUBSCRIBE sub-requests */
	public static final byte INET_SUBS_EMPTY_OUT_Q = 1;

	/* TCP additional flags */
	public static final byte TCP_ADDF_DELAY_SEND = 1;
	public static final byte TCP_ADDF_CLOSE_SENT = 2; /*
													 * Close sent (active mode
													 * only)
													 */
	public static final byte TCP_ADDF_DELAYED_CLOSE_RECV = 4; /*
															 * If receive fails,
															 * report
															 * {error,closed}
															 * (passive mode)
															 */
	public static final byte TCP_ADDF_DELAYED_CLOSE_SEND = 8; /*
															 * If send fails,
															 * report
															 * {error,closed}
															 * (passive mode)
															 */

	/*                     *_REQ_* replies */
	public static final byte INET_REP_ERROR = 0;
	public static final byte INET_REP_OK = 1;
	public static final byte INET_REP_SCTP = 2;

	public static final int INET_STATE_CLOSED = 0;
	public static final int INET_STATE_OPEN = (INET_F_OPEN);
	public static final int INET_STATE_BOUND = (INET_STATE_OPEN | INET_F_BOUND);
	public static final int INET_STATE_CONNECTED = (INET_STATE_BOUND | INET_F_ACTIVE);

	public static final int TCP_STATE_CLOSED = INET_STATE_CLOSED;
	public static final int TCP_STATE_OPEN = (INET_F_OPEN);
	public static final int TCP_STATE_BOUND = (TCP_STATE_OPEN | INET_F_BOUND);
	public static final int TCP_STATE_CONNECTED = (TCP_STATE_BOUND | INET_F_ACTIVE);
	public static final int TCP_STATE_LISTEN = (TCP_STATE_BOUND | INET_F_LISTEN);
	public static final int TCP_STATE_CONNECTING = (TCP_STATE_BOUND | INET_F_CON);
	public static final int TCP_STATE_ACCEPTING = (TCP_STATE_LISTEN | INET_F_ACC);
	public static final int TCP_STATE_MULTI_ACCEPTING = (TCP_STATE_ACCEPTING | INET_F_MULTI_CLIENT);

	// options
	public static final byte INET_OPT_REUSEADDR = 0;
	public static final byte INET_OPT_KEEPALIVE = 1;
	public static final byte INET_OPT_DONTROUTE = 2;
	public static final byte INET_OPT_LINGER = 3;
	public static final byte INET_OPT_BROADCAST = 4;
	public static final byte INET_OPT_OOBINLINE = 5;
	public static final byte INET_OPT_SNDBUF = 6;
	public static final byte INET_OPT_RCVBUF = 7;
	public static final byte INET_OPT_PRIORITY = 8;
	public static final byte INET_OPT_TOS = 9;
	public static final byte TCP_OPT_NODELAY = 10;
	public static final byte UDP_OPT_MULTICAST_IF = 11;
	public static final byte UDP_OPT_MULTICAST_TTL = 12;
	public static final byte UDP_OPT_MULTICAST_LOOP = 13;
	public static final byte UDP_OPT_ADD_MEMBERSHIP = 14;
	public static final byte UDP_OPT_DROP_MEMBERSHIP = 15;
	// "Local" options: codes start from 20:
	public static final byte INET_LOPT_BUFFER = 20;
	public static final byte INET_LOPT_HEADER = 21;
	public static final byte INET_LOPT_ACTIVE = 22;
	public static final byte INET_LOPT_PACKET = 23;
	public static final byte INET_LOPT_MODE = 24;
	public static final byte INET_LOPT_DELIVER = 25;
	public static final byte INET_LOPT_EXITONCLOSE = 26;
	public static final byte INET_LOPT_TCP_HIWTRMRK = 27;
	public static final byte INET_LOPT_TCP_LOWTRMRK = 28;
	public static final byte INET_LOPT_BIT8 = 29;
	public static final byte INET_LOPT_TCP_SEND_TIMEOUT = 30;
	public static final byte INET_LOPT_TCP_DELAY_SEND = 31;
	public static final byte INET_LOPT_PACKET_SIZE = 32;
	public static final byte INET_LOPT_READ_PACKETS = 33;
	public static final byte INET_OPT_RAW = 34;
	public static final byte INET_LOPT_TCP_SEND_TIMEOUT_CLOSE = 35;
	// Specific SCTP options: separate range:
	public static final byte SCTP_OPT_RTOINFO = 100;
	public static final byte SCTP_OPT_ASSOCINFO = 101;
	public static final byte SCTP_OPT_INITMSG = 102;
	public static final byte SCTP_OPT_AUTOCLOSE = 103;
	public static final byte SCTP_OPT_NODELAY = 104;
	public static final byte SCTP_OPT_DISABLE_FRAGMENTS = 105;
	public static final byte SCTP_OPT_I_WANT_MAPPED_V4_ADDR = 106;
	public static final byte SCTP_OPT_MAXSEG = 107;
	public static final byte SCTP_OPT_SET_PEER_PRIMARY_ADDR = 108;
	public static final byte SCTP_OPT_PRIMARY_ADDR = 109;
	public static final byte SCTP_OPT_ADAPTATION_LAYER = 110;
	public static final byte SCTP_OPT_PEER_ADDR_PARAMS = 111;
	public static final byte SCTP_OPT_DEFAULT_SEND_PARAM = 112;
	public static final byte SCTP_OPT_EVENTS = 113;
	public static final byte SCTP_OPT_DELAYED_ACK_TIME = 114;
	public static final byte SCTP_OPT_STATUS = 115;
	public static final byte SCTP_OPT_GET_PEER_ADDR_INFO = 116;

	/* INET_REQ_IFGET and INET_REQ_IFSET options */
	public static final int INET_IFOPT_ADDR = 1;
	public static final int INET_IFOPT_BROADADDR = 2;
	public static final int INET_IFOPT_DSTADDR = 3;
	public static final int INET_IFOPT_MTU = 4;
	public static final int INET_IFOPT_NETMASK = 5;
	public static final int INET_IFOPT_FLAGS = 6;
	public static final int INET_IFOPT_HWADDR = 7;

	/* INET_LOPT_BIT8 options */
	public static final int INET_BIT8_CLEAR = 0;
	public static final int INET_BIT8_SET = 1;
	public static final int INET_BIT8_ON = 2;
	public static final int INET_BIT8_OFF = 3;

	/* INET_REQ_GETSTAT enumeration */
	public static final int INET_STAT_RECV_CNT = 1;
	public static final int INET_STAT_RECV_MAX = 2;
	public static final int INET_STAT_RECV_AVG = 3;
	public static final int INET_STAT_RECV_DVI = 4;
	public static final int INET_STAT_SEND_CNT = 5;
	public static final int INET_STAT_SEND_MAX = 6;
	public static final int INET_STAT_SEND_AVG = 7;
	public static final int INET_STAT_SEND_PND = 8;
	public static final int INET_STAT_RECV_OCT = 9; /* received octets */
	public static final int INET_STAT_SEND_OCT = 10; /* sent octets */

	/* INET_IFOPT_FLAGS enumeration */
	public static final int INET_IFF_UP = 0x0001;
	public static final int INET_IFF_BROADCAST = 0x0002;
	public static final int INET_IFF_LOOPBACK = 0x0004;
	public static final int INET_IFF_POINTTOPOINT = 0x0008;
	public static final int INET_IFF_RUNNING = 0x0010;
	public static final int INET_IFF_MULTICAST = 0x0020;
	/* Complement flags for turning them off */
	public static final int INET_IFF_DOWN = 0x0100;
	public static final int INET_IFF_NBROADCAST = 0x0200;
	/* public static final int INET_IFF_NLOOPBACK =0x0400; */
	public static final int INET_IFF_NPOINTTOPOINT = 0x0800;
	/* public static final int INET_IFF_NRUNNING =0x1000; */
	/* public static final int INET_IFF_NMULTICAST =0x2000; */

	/*
	 * Flags for "sctp_sndrcvinfo". Used in a bitmask -- must be powers of 2:*
	 * INET_REQ_SETOPTS:SCTP_OPT_DEFAULT_SEND_PARAM
	 */
	public static final int SCTP_FLAG_UNORDERED = (1 /* am_unordered */);
	public static final int SCTP_FLAG_ADDR_OVER = (2 /* am_addr_over */);
	public static final int SCTP_FLAG_ABORT = (4 /* am_abort */);
	public static final int SCTP_FLAG_EOF = (8 /* am_eof */);
	public static final int SCTP_FLAG_SNDALL = (16 /*
													 * am_sndall, NOT YET
													 * IMPLEMENTED
													 */);

	/*
	 * Flags for "sctp_set_opts" (actually for SCTP_OPT_PEER_ADDR_PARAMS).*
	 * These flags are also used in a bitmask, so they must be powers of 2:
	 */
	public static final int SCTP_FLAG_HB_ENABLE = (1 /* am_hb_enable */);
	public static final int SCTP_FLAG_HB_DISABLE = (2 /* am_hb_disable */);
	public static final int SCTP_FLAG_HB_DEMAND = (4 /* am_hb_demand */);
	public static final int SCTP_FLAG_PMTUD_ENABLE = (8 /* am_pmtud_enable */);
	public static final int SCTP_FLAG_PMTUD_DISABLE = (16 /* am_pmtud_disable */);
	public static final int SCTP_FLAG_SACDELAY_ENABLE = (32 /* am_sackdelay_enable */);
	public static final int SCTP_FLAG_SACDELAY_DISABLE = (64 /* am_sackdelay_disable */);

	public static final int INET_DEF_BUFFER = 1460; /* default buffer size */
	public static final int INET_MIN_BUFFER = 1; /* internal min buffer */
	public static final int INET_MAX_BUFFER = (1024 * 64); /*
															 * internal max
															 * buffer
															 */

	public static final byte[] EXBADPORT = "exbadport".getBytes(Charset
			.forName("ASCII"));
	public static final byte[] EXBADSEQ = "exbadseq".getBytes(Charset
			.forName("ASCII"));

	public static final int INET_HIGH_WATERMARK =(1024*8); /* 8k pending high => busy  */
	/* Note: INET_LOW_WATERMARK MUST be less than INET_MAX_BUFFER and
	** less than INET_HIGH_WATERMARK
	*/
	public static final int INET_LOW_WATERMARK  =(1024*4); /* 4k pending => allow more */

	public static final int INET_INFINITY = 0xffffffff;
	private static final EAtom am_inet_async = EAtom.intern("inet_async");
	private static final EAtom am_inet_reply = EAtom.intern("inet_reply");
	private static final int INVALID_EVENT = 0xffff0000;
	private static final int SOL_SOCKET = 0xffff;
	private static final EAtom am_closed = EAtom.intern("closed");
	private static final EAtom am_empty_out_q = EAtom.intern("empty_out_q");
	private static final EAtom am_tcp = EAtom.intern("tcp");
	private static final EAtom am_tcp_closed = EAtom.intern("tcp_closed");
	private static final EAtom am_timeout = EAtom.intern("timeout");
	private static final EAtom am_tcp_error = EAtom.intern("tcp_error");
	private static final byte[] NOPROC = new byte[] { 'n', 'o', 'p', 'r', 'o', 'c'} ;
	private static final PacketCallbacks<TCPINet> INET_CALLBACKS = new TCPINetCallbacks();

	private int state = INET_STATE_CLOSED;
	private InetSocket fd;
	private List<EHandle> empty_out_q_subs = new ArrayList<EHandle>(1);
	private InetSocketAddress remote;
	ActiveType active = ActiveType.PASSIVE;
	private RingQueue<AsyncOp> opt = new RingQueue<AsyncOp>(1);
	private boolean busy_on_send;
	private EHandle caller;
	private EHandle busy_caller;
	private int i_remain;
	private boolean send_timeout_close;

	private AsyncMultiOp multi_last;
	private AsyncMultiOp multi_first;
	private PriorityQueue<MultiTimerData> mtd_queue;
	private boolean prebound;
	private int event_mask;
	PacketParseType htype = PacketParseType.TCP_PB_RAW;
	private int hsz;
	private int mode = INET_MODE_LIST;
	private int deliver = INET_DELIVER_TERM;
	private int bufsz = 1200;
	private boolean exitf = true;
	private int psize;
	private boolean bit8f = false;
	private boolean bit8 = false;
	private int low = INET_LOW_WATERMARK;
	private int high = INET_HIGH_WATERMARK;
	private int send_timeout = INET_INFINITY;
	private int tcp_add_flags;
	private int read_packets;

	private Protocol protocol;
	private ProtocolType stype;
	private ProtocolFamily sfamily;
	private ByteBuffer i_buf;
	private int i_ptr_start;
	private IntCell http_state = new IntCell();
	private PacketCallbacks<TCPINet> packet_callbacks = INET_CALLBACKS;

	private int recv_cnt;
	private int recv_max;
	private double recv_avg;
	private double recv_dvi;
	private int send_cnt;
	private int send_max;
	private double send_avg;
	private long recv_oct;
	private long send_oct;

	// private int i_ptr;
	// private int i_bufsz;

	public TCPINet(Protocol protocol, Driver driver) {
		super(driver);
		this.protocol = protocol;
		this.bufsz = INET_DEF_BUFFER;
	}

	public TCPINet copy(EPID caller, InetSocket sock) {

		TCPINet copy;
		try {
			copy = (TCPINet) this.clone();
		} catch (CloneNotSupportedException e) {
			throw new InternalError("cannot clone");
		}

		copy.fd = sock;
		copy.caller = caller;
		copy.opt = new RingQueue<AsyncOp>(2);
		copy.empty_out_q_subs = new ArrayList<EHandle>();
		copy.active = ActiveType.PASSIVE;

		final EDriverTask this_task = port().task();
		EDriverTask driver = new EDriverTask(caller, copy) {
			public EObject getName() {
				return this_task.getName();
			}
		};

		// two-way link to new owner
		EInternalPID caller_pid = caller.testInternalPID();
		if (caller_pid != null) {
			caller_pid.task().link_oneway(driver.self_handle());
			driver.link_oneway(caller_pid);
		}
		ERT.run(driver);

		return copy;
	}

	@Override
	protected void flush() throws Pausable {
		throw new erjang.NotImplemented();

	}

	/*
	** command:
	**   output on a socket only !
	**   a reply code will be sent to connected (caller later)
	**   {inet_reply, S, Status}
	** NOTE! normal sockets use the the tcp_inet_commandv
	** but distribution still uses the tcp_inet_command!!
	*/

	@Override
	protected void output(EHandle caller, ByteBuffer buf) throws IOException, Pausable {
		
		this.caller = caller;
		if (!is_connected()) {
			inet_reply_error(Posix.ENOTCONN);
		} else if (tcp_sendv(new ByteBuffer[]{buf}) == 0) {
			inet_reply_ok(caller);
		}
		
		//System.err.println("OUTPUT!!");
		throw new erjang.NotImplemented();

	}

	@Override
	protected void outputv(EHandle caller, ByteBuffer[] ev)
			throws IOException, Pausable {

		this.caller = caller;

		if (ERT.DEBUG_INET) {
			System.err.println("TCPIP::outputv");
			dump_write(ev);
		}

		if (!is_connected()) {
			if ((tcp_add_flags & TCP_ADDF_DELAYED_CLOSE_SEND) != 0) {
				tcp_add_flags &= ~TCP_ADDF_DELAYED_CLOSE_SEND;
				inet_reply_error(am_closed);
			} else {
				inet_reply_error(Posix.ENOTCONN);
			}
		} else if (tcp_sendv(ev) == 0) {
			inet_reply_ok(caller);
		} else {
			// System.err.println("bad output");
		}

	}

	private int tcp_sendv(ByteBuffer[] ev) throws Pausable {
		int sz;
		EPort ix = port();
		long len = remaining(ev);
		ByteBuffer hbuf = null;

		switch (htype) {
		case TCP_PB_1:
			hbuf = ByteBuffer.allocate(1);
			hbuf.put(0, (byte) (len & 0xff));
			break;
		case TCP_PB_2:
			hbuf = ByteBuffer.allocate(2);
			hbuf.putShort(0, (short) (len & 0xffff));
			break;
		case TCP_PB_4:
			hbuf = ByteBuffer.allocate(4);
			hbuf.putInt(0, (int) len);
			break;
		default:
			if (len == 0) {
				return 0;
			}
		}

		inet_output_count(len+ (hbuf==null ? 0 : hbuf.limit()));
		
		if (hbuf != null) {
			len += hbuf.remaining();
			// insert hbuf ahead of ev
			if (ev.length > 0 && ev[0].remaining() == 0) {
				ev[0] = hbuf;
			} else {
				ByteBuffer[] ev2 = new ByteBuffer[ev.length + 1];
				ev2[0] = hbuf;
				System.arraycopy(ev, 0, ev2, 1, ev.length);
				ev = ev2;
			}
		}

		if ((sz = driver_sizeq()) > 0) {
			
			driver_enqv(ev);
			sock_select(ERL_DRV_WRITE, SelectMode.SET);

			//System.err.println("enqued output [1]!");
			//dump_write(ev);
			//System.err.println("queue is now");
			//dump_write(driver_peekq());



			if (sz + len >= high) {

				// TODO: we somehow fail in this case.
				// The data is put on the queue, but never written?
				
				state |= INET_F_BUSY; /* mark for low-watermark */
				busy_caller = caller;
				set_busy_port(port(), true);
				if (this.send_timeout != INET_INFINITY) {
					busy_on_send = true;
					driver_set_timer(send_timeout);
				}
				return 1;
			}

		} else {
			int vsize = ev.length;
			long n;

			if ((tcp_add_flags & TCP_ADDF_DELAY_SEND) != 0) {
				if (ERT.DEBUG_INET)
					System.err.println("tcp_delay_send!");
				n = 0;
			} else {
				GatheringByteChannel gbc = (GatheringByteChannel) fd.channel();

				try {

					// dump_write(ev);

					n = gbc.write(ev);

					// System.err.println("sent " + n + " bytes to " + gbc);

				} catch (IOException e) {
					// System.err.println("failed to write to " + gbc);

					int sock_errno = IO.exception_to_posix_code(e);
					if ((sock_errno != Posix.EAGAIN)
							&& (sock_errno != Posix.EINTR)) {
						tcp_send_error(sock_errno);
						return sock_errno;
					}
					// TODO: handle partial writes, async
					n = 0;
				}

			}

			if (n == len) {
				// we sent everything!
				assert empty_out_q_subs.isEmpty() : "no subscribers";
				return 0;
			}

			
			//System.err.println("enqueing output [2]! n="+n);
			//dump_write(ev);

			driver_enqv(ev);
			sock_select(ERL_DRV_WRITE | 0, SelectMode.SET);

		}
		return 0;

	}

	private void inet_output_count(long len) {
		this.send_oct += len;
		this.send_cnt += 1;
		if (len > send_max)
			send_max = (int) len;
		
		// TODO: Implement avg and dvi 
	}

	static String hex2(int i) {
		if (i < 0x10) return "0" + Integer.toHexString(i);
		return Integer.toHexString(i);
	}
	
	static String hex4(int i) {
		if (i < 0x10) return "000" + Integer.toHexString(i);
		if (i < 0x100) return "00" + Integer.toHexString(i);
		if (i < 0x1000) return "0" + Integer.toHexString(i);
		return Integer.toHexString(i);
	}
	
	public static void dump_write(ByteBuffer[] ev) {

		if (!ERT.DEBUG_INET) return;
		
		System.err.println(" vec[" + ev.length + "]:: ");

		for (int i = 0; i < ev.length; i++) {

			ByteBuffer evp = ev[i];
			int off = 0;
			boolean did_print = false;
			for (int p = evp.position(); p < evp.limit(); p++) {

				if ((off % 0x10) == 0 && off != 0)
					System.err.println("");

				if ((off % 0x10) == 0)
					System.err.print("["+i+"] 0x" + hex4(off) + " :");

				did_print = true;
				System.err.print(" ");
				byte ch = evp.get(p);
				System.err.print(hex2(ch&0xff));

				off += 1;

			}

			if (i < ev.length-1 && did_print) System.out.println();
		}

		System.err.println("---");

		for (int i = 0; i < ev.length; i++) {

			ByteBuffer evp = ev[i];
			int off = 0;
			boolean did_print = false;
			for (int p = evp.position(); p < evp.limit(); p++) {

				if ((off % 0x10) == 0 && off != 0)
					System.err.println("");

				if ((off % 0x10) == 0)
					System.err.print("["+i+"] 0x" + hex4(off) + " : ");

				did_print = true;
				byte ch = evp.get(p);
				if (ch >= 32 && ch <= 127) {
					System.err.print((char)ch);
				} else {
					System.err.print('.');
				}

				off += 1;

			}

			if (i < ev.length-1 && did_print) System.out.println();
		}


	}

	private long remaining(ByteBuffer[] ev) {
		long res = 0;
		for (int i = 0; i < ev.length; i++) {
			if (ev[i] != null) {
				res += ev[i].remaining();
			}
		}
		return res;
	}

	@Override
	public void processExit(ERef monitor) throws Pausable {

		EHandle who = driver_get_monitored_process(monitor);
		int state = this.state;

		if ((state & TCP_STATE_MULTI_ACCEPTING) == TCP_STATE_MULTI_ACCEPTING) {
			AsyncMultiOp op;
			if ((op = remove_multi_op(who))  == null){
				return;
			}
			if (op.timeout != null) {
				remove_multi_timer(op.timeout);
			}
			if (this.multi_first == null) {
				sock_select(ERL_DRV_ACCEPT, SelectMode.CLEAR);
				this.state = TCP_STATE_LISTEN;
			}

		} else if ((state & TCP_STATE_ACCEPTING) == TCP_STATE_ACCEPTING) {
			deq_async();
			driver_cancel_timer();
			sock_select(ERL_DRV_ACCEPT, SelectMode.CLEAR);
			this.state = TCP_STATE_LISTEN;
		}

	}

	private void sock_select(int ops, SelectMode onOff) {
		if (onOff == SelectMode.SET) {
			this.event_mask |= ops;
		} else {
			this.event_mask &= ~ops;
		}

		if (ERT.DEBUG_INET)
		System.err.println("sock_select " + this + " ops="
				+ Integer.toBinaryString(ops) + "; mode=" + onOff);

		/*
		 * if (ops == 1 && onOff == SelectMode.CLEAR) { new
		 * Throwable().printStackTrace(System.err); }
		 */

		super.select(this.fd.channel(), ops, onOff);
	}

	private void remove_multi_timer(MultiTimerData p) {
		boolean is_first = mtd_queue.peek() == p;
		
		mtd_queue.remove(p);
		
		if (is_first) {
			driver_cancel_timer();
			
			MultiTimerData first = mtd_queue.peek();
			if (first != null) {
				long timeout = relative_timeout( first.when );
				driver_set_timer(timeout);
			}			
		}
		
	}

	private long relative_timeout(long abs_time) {
		long now = System.currentTimeMillis();
		if (abs_time < now)
			return 0;
		else
			return abs_time-now;
	}

	@Override
	protected void readyAsync(EAsync data) throws Pausable {
		throw new erjang.NotImplemented();

	}

	@Override
	protected void readyInput(SelectableChannel ch) throws Pausable {

		if (fd == null || fd.channel() == null)
			return;

		if (!is_open())
			return;

		if (ch.isOpen())
			select(ch, ERL_DRV_READ, SelectMode.SET);

		if (ERT.DEBUG_INET)
		System.err.println("readyInput " + this + " @ " + ch);

		if (is_connected()) {
			int rcv = tcp_recv(0);
			if (ERT.DEBUG_INET)
			System.err.println("tcp_recv[async] =>" + rcv);

		} else {
			if (ERT.DEBUG_INET)
			System.err.println("received input while not connected?");
		}

	}

	private int tcp_recv(int request_len) throws Pausable {
		if (ERT.DEBUG_INET)
		System.err.println("tcp_recv len="+request_len);
		
		int n, len, nread;

		if (i_buf == null) {
			if (ERT.DEBUG_INET)
				System.err.println("tcp_recv no ibuf");
			/* allocate a read buffer */
			int sz = (request_len > 0) ? request_len : this.bufsz;

			try {
				i_buf = ByteBuffer.allocate(sz);
			} catch (OutOfMemoryError e) {
				return -1;
			}

			i_ptr_start = 0;
			nread = sz;
			if (request_len > 0) {
				i_remain = request_len;
			} else {
				i_remain = 0;
			}

		} else if (request_len > 0) {
			n = i_buf.position() - i_ptr_start;
			if (ERT.DEBUG_INET)
				System.err.println("tcp_recv has "+n+" bytes");
			if (n >= request_len) {
				return tcp_deliver(request_len);
			} else if (tcp_expand_buffer(request_len) < 0) {
				return tcp_recv_error(Posix.ENOMEM);
			} else {
				i_remain = nread = request_len - n;
			}

		} else if (i_remain == 0) {
			if (ERT.DEBUG_INET)
				System.err.println("tcp_recv i_remain == 0");
			int[] lenp = new int[1];
			if ((nread = tcp_remain(lenp)) < 0) {
				return tcp_recv_error(Posix.EMSGSIZE);
			} else if (nread == 0) {
				return tcp_deliver(lenp[0]);
			} else if (lenp[0] > 0) {
				i_remain = lenp[0];
			}

		} else {
			if (ERT.DEBUG_INET)
				System.err.println("tcp_recv i_remain == "+i_remain);
			nread = i_remain;
		}

		try {
			ReadableByteChannel rbc = (ReadableByteChannel) fd.channel();
			
			if (rbc instanceof SocketChannel) {
				SocketChannel sc = (SocketChannel) rbc;
				if (sc.isBlocking()) {
					System.err.println("SOCKET IS BLOCKING! "+sc);
				}
			} else {
				System.err.println("NOT A SOCKET! "+rbc);
			}

			// only read up to nread bytes
			i_buf.limit( Math.min (i_buf.position() + nread, i_buf.capacity()) );
			
			n = rbc.read(i_buf);
			if (ERT.DEBUG_INET)
				System.err.println("did read " + n + " bytes (ibuf.size="+i_buf.remaining()+")");
			if (n == 0)
				return 0;
			
		} catch (ClosedChannelException e) {
			return tcp_recv_closed();
		} catch (IOException e) {
			int code = IO.exception_to_posix_code(e);
			if (code == Posix.ENOTCONN || !fd.isOpen())
				return tcp_recv_closed();
			else
				return tcp_recv_error(IO.exception_to_posix_code(e));
		}

		if (n < 0) {
			try {
				fd.channel().close();
			} catch (IOException e) {
				// ignore
			}
			return tcp_recv_closed();
		}

		if (i_remain > 0) {
			i_remain -= n;
			if (i_remain == 0) {
				return tcp_deliver(i_buf.position() - i_ptr_start);
			}
		} else {
			int[] lenp = new int[1];
			nread = tcp_remain(lenp);

			if ((nread) < 0) {
				return tcp_recv_error(Posix.EMSGSIZE);
			} else if (nread == 0) {
				return tcp_deliver(lenp[0]);
			} else if (lenp[0] > 0) {
				i_remain = lenp[0];
			}
		}

		return 0;
	}

	private int tcp_recv_closed() throws Pausable {
		if (is_busy()) {
			caller = busy_caller;
			tcp_clear_output();
			if (busy_on_send) {
				driver_cancel_timer();
				busy_on_send = false;
			}
			state &= ~INET_F_BUSY;
			set_busy_port(false);
			inet_reply_error(am_closed);
		}

		if (active == ActiveType.PASSIVE) {
			driver_cancel_timer();
			tcp_clear_input();
			if (exitf) {
				tcp_clear_output();
				desc_close();
			} else {
				desc_close_read();
			}
			async_error_all(am_closed);
		} else {
			tcp_clear_input();
			tcp_closed_message();
			if (exitf) {
				driver_exit(0);
			} else {
				desc_close_read();
			}
		}
		return -1;
	}

	private void tcp_clear_output() throws Pausable {
		int qsz = driver_sizeq();

		// clear queue
		ByteBuffer[] q = driver_peekq();
		if (q != null) {
			for (int i = 0; i < q.length; i++) {
				if (q[i] != null) {
					q[i].position(q[i].limit());
				}
			}
		}

		driver_deq(qsz);
		send_empty_out_q_msgs();

	}

	private int tcp_recv_error(int err) throws Pausable {

		if (err == Posix.EAGAIN) {
			return 0;
		}

		if (is_busy()) {
			/* A send is blocked */
			caller = busy_caller;
			tcp_clear_output();
			if (busy_on_send) {
				driver_cancel_timer();
				busy_on_send = false;
			}
			state &= ~INET_F_BUSY;
			set_busy_port(false);
			inet_reply_error(am_closed);
		}
		if (active == ActiveType.PASSIVE) {
			/* We must cancel any timer here ! */
			driver_cancel_timer();
			tcp_clear_input();
			if (exitf) {
				desc_close();
			} else {
				desc_close_read();
			}
			async_error_all(EAtom.intern(Posix.errno_id(err)));
		} else {
			tcp_clear_input();
			tcp_error_message(err); /* first error */
			tcp_closed_message(); /* then closed */
			if (exitf)
				driver_exit(err);
			else
				desc_close();
		}
		return -1;

	}

	private void tcp_error_message(int err) throws Pausable {
		ETuple spec = ETuple.make(am_tcp_error, port(), EAtom.intern(Posix
				.errno_id(err)));
		driver_output_term(spec);
	}

	@Override
	protected void readyOutput(SelectableChannel evt)  throws Pausable {


		EInternalPort ix = port();

		if (is_connected()) {
			ByteBuffer[] iov;
			if ((iov = driver_peekq()) == null) {
				select(evt, ERL_DRV_WRITE, SelectMode.CLEAR);
				send_empty_out_q_msgs();
				return;
			}

			GatheringByteChannel gbc = (GatheringByteChannel) fd.channel();
			long n;
			try {
				// dump_write(iov);
				n = gbc.write(iov);
				
				//System.err.println("delayed write!");
				//dump_write(iov);


			} catch (IOException e) {
				tcp_send_error(IO.exception_to_posix_code(e));
				return;
			}
			
			driver_deq(n);
			
			int dsq = driver_sizeq();
			if (dsq != 0) {
				select(evt, ERL_DRV_WRITE, SelectMode.SET);
			}
			
			if (dsq <= low) {
				if (is_busy()) {
					this.caller = busy_caller;
					state &= ~INET_F_BUSY;
					set_busy_port(port(), false);
					if (busy_on_send) {
						driver_cancel_timer();
						busy_on_send = false;
					}
					inet_reply_ok(caller);
				}
			}
		}

	}

	private void set_busy_port(EInternalPort port, boolean on) {
		if (on) {
			task.status |= EDriverTask.ERTS_PORT_SFLG_PORT_BUSY;
		} else {
			task.status &= ~EDriverTask.ERTS_PORT_SFLG_PORT_BUSY;
		}
	}

	private void inet_reply_ok(EHandle caller2) throws Pausable {
		ETuple msg = ETuple.make(am_inet_reply, port(), ERT.am_ok);
		EHandle caller = this.caller;
		this.caller = null;

		if (ERT.DEBUG_INET && caller != null) {
			System.err.println("sending to " + caller + " ! " + msg);
//			if (caller == null) {
//				new Throwable("caller is null, caller2="+caller2).printStackTrace(System.err);
//			}
		}

		driver_send_term(caller, msg);
	}

	private void tcp_send_error(int err) throws Pausable {

		if (is_busy()) {
			caller = busy_caller;
			tcp_clear_output();
			if (busy_on_send) {
				driver_cancel_timer();
				busy_on_send = false;
			}
			state &= ~INET_F_BUSY;
			set_busy_port(false);
		}

		if (active != ActiveType.PASSIVE) {
			tcp_closed_message();
			inet_reply_error(am_closed);
			if (exitf) {
				driver_exit(0);
			} else {
				try {
					fd.close();
				} catch (IOException e) {
					// ignore //
				}
			}
		} else {
			tcp_clear_output();
			tcp_clear_input();
			tcp_close_check();
			erl_inet_close();

			if (caller != null) {
				inet_reply_error(am_closed);
			} else {
				/*
				 * No blocking send op to reply to right now. If next op is a
				 * send, make sure it returns {error,closed} rather than
				 * {error,enotconn}.
				 */
				tcp_add_flags |= TCP_ADDF_DELAYED_CLOSE_SEND;
			}

			tcp_add_flags |= TCP_ADDF_DELAYED_CLOSE_RECV;
		}

	}

	private void tcp_close_check() throws Pausable {
		if (state == TCP_STATE_ACCEPTING) {
			AsyncOp op = opt.peek();
			sock_select(ERL_DRV_ACCEPT, SelectMode.CLEAR);
			state = TCP_STATE_LISTEN;
			if (op != null) {
				driver_demonitor_process(op.monitor);
			}
			async_error(am_closed);

		} else if ((state&TCP_STATE_MULTI_ACCEPTING) == TCP_STATE_MULTI_ACCEPTING) {
			throw new NotImplemented();

		} else if ((state&TCP_STATE_CONNECTING) == TCP_STATE_CONNECTING) {
			async_error(am_closed);

		} else if ((state&TCP_STATE_CONNECTED) == TCP_STATE_CONNECTED) {
			async_error_all(am_closed);
		}
	}

	private void async_error_all(EAtom reason) throws Pausable {

		for (AsyncOp op = deq_async(); op != null; op = deq_async()) {
			send_async_error(op.id, op.caller, reason);
		}

	}

	private void send_empty_out_q_msgs() throws Pausable {

		if (empty_out_q_subs.isEmpty())
			return;

		ETuple2 msg = new ETuple2(am_empty_out_q, port());
		for (EHandle h : empty_out_q_subs) {
			h.send(port(), msg);
		}
	}

	@Override
	public void readyAccept(SelectableChannel ch) throws Pausable {
		if (ERT.DEBUG_INET)
			System.err.println("readyAccept " + this);

		if (state == TCP_STATE_ACCEPTING) {
			InetSocket sock;
			try {
				sock = fd.accept();
			} catch (IOException e) {
				sock = null;
				// return //
			}

			// we got a connection
			sock_select(ERL_DRV_ACCEPT, SelectMode.CLEAR);
			state = TCP_STATE_LISTEN;

			AsyncOp peek = opt.peek();
			if (peek != null) {
				driver_demonitor_process(peek.monitor);
			}

			driver_cancel_timer();

			if (sock == null) {
				// return //
			} else {

				if (peek == null) {
					sock_close(fd.channel());
					async_error(Posix.EINVAL);
					return;
				}

				TCPINet accept_desc = this.copy(peek.caller, sock);
				accept_desc.state = TCP_STATE_CONNECTED;

				try {
					sock.setNonBlocking();
				} catch (IOException e) {
					e.printStackTrace();
				}
				// accept_desc.select(sock.channel(), ERL_DRV_READ,
				// SelectMode.SET);

				async_ok_port(peek.caller, accept_desc.port());
			}
		} else if (state == TCP_STATE_MULTI_ACCEPTING) {
			
			while (state == TCP_STATE_MULTI_ACCEPTING) {
				int err = 0;
				InetSocket sock;
				try {
					sock = fd.accept();
					if (sock == null) {
						sock_select(ERL_DRV_ACCEPT, SelectMode.SET);
						return;
					}
				} catch (IOException e) {
					sock = null;
					err = IO.exception_to_posix_code(e);
				}
	
				AsyncMultiOp multi = deq_multi_op();
				if (multi == null) {
					return; /* -1; close? */
				}

				if (this.multi_first == null) {
					// we got a connection, and there are no more multi's waiting
					sock_select(ERL_DRV_ACCEPT, SelectMode.CLEAR);
					state = TCP_STATE_LISTEN;
				}
				
				if (multi.timeout != null) {
					remove_multi_timer(multi.timeout);
				}
				
				driver_demonitor_process(multi.op.monitor);

				if (sock == null) {
					send_async_error(multi.op.id, multi.op.caller, 
							EAtom.intern(Posix.errno_id(err).toLowerCase()));
					// return //
				} else {
	
					TCPINet accept_desc = this.copy(multi.op.caller, sock);
					accept_desc.state = TCP_STATE_CONNECTED;
	
					try {
						sock.setNonBlocking();
					} catch (IOException e) {
						e.printStackTrace();
					}
					// accept_desc.select(sock.channel(), ERL_DRV_READ,
					// SelectMode.SET);
	
					send_async_ok_port(multi.op.id, multi.op.caller, accept_desc.port());
				}
			}
		}
	}

	@Override
	public void readyConnect(SelectableChannel evt) throws Pausable {

		if (ERT.DEBUG_INET)
			System.err.println("readyConnect " + this);

		if (state == TCP_STATE_CONNECTING) {
			// clear select state
			sock_select(ERL_DRV_CONNECT, SelectMode.CLEAR);

			// cancel the timer
			driver_cancel_timer();

			try {
				if (!fd.finishConnect()) {
					async_error(Posix.EIO);
					return;
				} else {
					state = TCP_STATE_CONNECTED;

				}
			} catch (IOException e) {
				// e.printStackTrace();
				async_error(IO.exception_to_posix_code(e));
				return;
			}

			if (active != ActiveType.PASSIVE) {
				sock_select(ERL_DRV_READ, SelectMode.SET);
			}

			async_ok();

		} else {
			sock_select(ERL_DRV_CONNECT, SelectMode.CLEAR);

		}

	}

	private boolean async_error(int eio) throws Pausable {

		return async_error(EAtom.intern(Posix.errno_id(eio).toLowerCase()));
	}

	@Override
	/* Handling of timeout in driver */
	protected void timeout() throws Pausable {

		if (ERT.DEBUG_INET) {
			System.err.println("timeout "+this);
		}
		
		if ((state & INET_F_MULTI_CLIENT) != 0) {
			fire_multi_timers();

		} else if ((state & TCP_STATE_CONNECTED) == TCP_STATE_CONNECTED) {

			if (busy_on_send) {
				assert is_busy();

				caller = busy_caller;
				state &= ~INET_F_BUSY;
				busy_on_send = false;
				set_busy_port(false);
				inet_reply_error(ERT.am_timeout);
				if (send_timeout_close) {
					erl_inet_close();
				}
			} else {
				/* assume recv timeout */
				assert active != ActiveType.PASSIVE;
				sock_select(ERL_DRV_READ | ERL_DRV_WRITE, SelectMode.CLEAR);
				i_remain = 0;
				async_error(ERT.am_timeout);
			}

		} else if ((state & TCP_STATE_CONNECTING) == TCP_STATE_CONNECTING) {

			erl_inet_close();
			async_error(ERT.am_timeout);

		} else if ((state & TCP_STATE_ACCEPTING) == TCP_STATE_ACCEPTING) {
			AsyncOp this_op = opt.peek();

			/* timer is set on accept */
			sock_select(ERL_DRV_ACCEPT, SelectMode.CLEAR);
			if (this_op != null) {
				driver_demonitor_process(this_op.monitor);
			}
			state = TCP_STATE_LISTEN;
			async_error(ERT.am_timeout);

		}
	}

	private boolean is_busy() {
		return (state & INET_F_BUSY) == INET_F_BUSY;
	}

	private void fire_multi_timers() throws Pausable {
		long next_timeout = 0;
		if (this.mtd_queue == null || this.mtd_queue.isEmpty()) {
			throw new InternalError();
		}
		
		do {
			MultiTimerData save = mtd_queue.remove();
			tcp_inet_multi_timeout(save.caller);
			
			if (mtd_queue.isEmpty())
				return;
			
			next_timeout = mtd_queue.peek().when;
			
		} while (next_timeout == 0);
		driver_set_timer(next_timeout);
	}

	private void tcp_inet_multi_timeout(EPID caller) throws Pausable {

		AsyncMultiOp multi;
		if ((multi = remove_multi_op(caller)) == null) {
			return;
		}
		
		driver_demonitor_process(multi.op.monitor);
		
		if (multi_first == null) {
			sock_select(ERL_DRV_ACCEPT, SelectMode.CLEAR);
			this.state = TCP_STATE_LISTEN;
		}
		
		send_async_error(multi.op.id, caller, am_timeout);
	}

	private boolean async_error(EAtom reason) throws Pausable {
		AsyncOp op = deq_async();
		if (op == null) {
			return false;
		}
		return send_async_error(op.id, op.caller, reason);
	}

	private void erl_inet_close() {

		free_subscribers(empty_out_q_subs);
		if (!prebound && ((state & INET_F_OPEN) == INET_F_OPEN)) {
			desc_close();
			state = INET_STATE_CLOSED;
		} else if (prebound && (fd != null)) {
			sock_select(ERL_DRV_READ | ERL_DRV_WRITE, SelectMode.CLEAR);
			event_mask = 0;
		}
	}

	private void desc_close() {

		sock_select(ERL_DRV_USE, SelectMode.CLEAR);
		fd = null;
		event_mask = 0;

		// when no more users of socket, the callback
		// stopSelect is called
	}

	@Override
	protected void stopSelect(SelectableChannel ch) throws Pausable {
		sock_close(ch);
	}
	
	@Override
	protected void stop(EObject reason) throws Pausable {
		super.stop(reason);
		tcp_close_check();
		InetSocket ff = fd;
		if (ff != null) {
			SelectableChannel ch = ff.channel();
			if (ch != null) {
			sock_close(ch);
			}
		}
	}

	private void sock_close(SelectableChannel ch) {
		try {
			ch.close();
		} catch (IOException e) {
			if (ERT.DEBUG_PORT)
				e.printStackTrace();
		}
	}

	private void free_subscribers(List<EHandle> emptyOutQSubs) {
		emptyOutQSubs.clear();
	}

	@Override
	protected ByteBuffer control(EPID caller, int command,
			ByteBuffer buf) throws Pausable {

		switch (command) {
		case INET_REQ_OPEN:
			return inet_open(buf);

		case INET_REQ_PEER:
			return inet_peer(buf);

		case INET_REQ_SUBSCRIBE:
			return inet_subscribe(caller, buf);

		case INET_REQ_BIND:
			return inet_bind(buf);

		case INET_REQ_NAME:
			return inet_name(buf);

		case TCP_REQ_LISTEN:
			return tcp_listen(buf);

		case TCP_REQ_ACCEPT:
			return tcp_accept(caller, buf);

		case TCP_REQ_RECV:
			return tcp_recv(caller, buf);

		case INET_REQ_CONNECT:
			return inet_connect(caller, buf);

		case INET_REQ_GETOPTS:
			return inet_get_opts(buf);

		case INET_REQ_SETOPTS:
			switch (inet_set_opts(buf)) {
			case -1:
				return ctl_error(Posix.EINVAL);
			case 0:
				return ctl_reply(INET_REP_OK, new byte[0]);
			default: /* active/passive change!! */
				/*
				 * Let's hope that the descriptor really is a tcp_descriptor
				 * here.
				 */
				tcp_deliver(0);
				return ctl_reply(INET_REP_OK, new byte[0]);
			}

		case INET_REQ_GETSTAT:
			return inet_getstat(buf);

		}

		throw new erjang.NotImplemented("tcp_inet control(cmd=" + command + ")");

	}

	private ByteBuffer inet_getstat(ByteBuffer buf) {
		int outsize = 1 + (buf.remaining() + 2) * 5;
		ByteBuffer dst = ByteBuffer.allocate(outsize);

		dst.put(INET_REP_OK);

		while (buf.hasRemaining()) {
			byte op = buf.get();
			dst.put(op);
			int val;

			switch (op) {
			case INET_STAT_RECV_CNT:
				val = this.recv_cnt;
				break;
			case INET_STAT_RECV_MAX:
				val = this.recv_max;
				break;
			case INET_STAT_RECV_AVG:
				val = (int) this.recv_avg;
				break;
			case INET_STAT_RECV_DVI:
				val = (int) Math.abs(this.recv_dvi);
				break;
			case INET_STAT_SEND_CNT:
				val = this.send_cnt;
				break;
			case INET_STAT_SEND_MAX:
				val = this.send_max;
				break;
			case INET_STAT_SEND_AVG:
				val = (int) this.send_avg;
				break;
			case INET_STAT_SEND_PND:
				val = driver_sizeq();
				break;
			case INET_STAT_RECV_OCT:
				dst.putLong(this.recv_oct);
				continue;
			case INET_STAT_SEND_OCT:
				dst.putLong(this.send_oct);
				continue;
			default:
				return ctl_error(Posix.EINVAL);
			}

			dst.putInt(val); /* write 32bit value */

		}

		return dst;

	}

	private ByteBuffer inet_peer(ByteBuffer buf) {

		if ((state & INET_F_ACTIVE) == 0) {
			//System.err.println("peer -> not connected");
			
			return ctl_error(Posix.ENOTCONN);
		}

		InetSocketAddress addr = fd.getRemoteAddress();
		InetAddress a = addr.getAddress();

		//System.err.println("peer -> "+addr);

		byte[] bytes = a.getAddress();
		int port = addr.getPort();

		byte[] data = new byte[1 + 2 + bytes.length];
		data[0] = (byte) sfamily.code;
		data[1] = (byte) ((port >> 8) & 0xff);
		data[2] = (byte) (port & 0xff);
		System.arraycopy(bytes, 0, data, 3, bytes.length);

		return ctl_reply(INET_REP_OK, data);

	}

	private ByteBuffer tcp_recv(EPID caller2, ByteBuffer buf) throws Pausable {
		/* INPUT: Timeout(4), Length(4) */

		if (!is_connected()) {
			if ((tcp_add_flags & TCP_ADDF_DELAYED_CLOSE_RECV) != 0) {
				tcp_add_flags &= ~(TCP_ADDF_DELAYED_CLOSE_RECV | TCP_ADDF_DELAYED_CLOSE_SEND);
				return ctl_reply(INET_REP_ERROR, "closed");
			} else {
				return ctl_error(Posix.ENOTCONN);
			}
		}

		if (active != ActiveType.PASSIVE || buf.remaining() != 8) {
			return ctl_error(Posix.EINVAL);
		}

		int timeout = buf.getInt();
		int n = buf.getInt();

		if (htype != PacketParseType.TCP_PB_RAW && n != 0)
			return ctl_error(Posix.EINVAL);

		if (n > 0x4000000)
			return ctl_error(Posix.ENOMEM);

		ByteBuffer tbuf = ByteBuffer.allocate(2);

		if (enq_async(caller2, tbuf, TCP_REQ_RECV) < 0)
			return ctl_error(Posix.EALREADY);

		if (ERT.DEBUG_INET)
			System.err.println("enq " + caller2 + "::" + tbuf.getShort(0) +"; timeout="+timeout);

		int rep;
		if ((rep = tcp_recv(n)) == 0) {
			if (timeout == 0) {
				async_error(am_timeout);
			} else {
				if (timeout != INET_INFINITY) {
					driver_set_timer(timeout);
				}
				sock_select(ERL_DRV_READ, SelectMode.SET);
			}
		} else {
			if (ERT.DEBUG_INET)
				System.err.println("tcp_recv[sync] => " + rep);
		}

		return ctl_reply(INET_REP_OK, tbuf.array());
	}

	private ByteBuffer inet_get_opts(ByteBuffer buf) {
		ByteArrayOutputStream barr = new ByteArrayOutputStream();
		DataOutputStream ptr = new DataOutputStream(barr);

		try {
			barr.write(INET_REP_OK);

			while (buf.hasRemaining()) {
				byte opt = buf.get();

				switch (opt) {
				case INET_LOPT_BUFFER:
					ptr.write(opt);
					ptr.writeInt(bufsz);
					continue;
				case INET_LOPT_HEADER:
					ptr.write(opt);
					ptr.writeInt(hsz);
					continue;
				case INET_LOPT_MODE:
					ptr.write(opt);
					ptr.writeInt(mode);
					continue;
				case INET_LOPT_DELIVER:
					ptr.write(opt);
					ptr.writeInt(deliver);
					continue;
				case INET_LOPT_ACTIVE:
					ptr.write(opt);
					ptr.writeInt(active == ActiveType.PASSIVE ? 0
							: active == ActiveType.ACTIVE ? 1 : 2);
					continue;
				case INET_LOPT_PACKET:
					ptr.write(opt);
					ptr.writeInt(htype.code);
					continue;
				case INET_LOPT_PACKET_SIZE:
					ptr.write(opt);
					ptr.writeInt(psize);
					continue;
				case INET_LOPT_EXITONCLOSE:
					ptr.write(opt);
					ptr.writeInt(exitf ? 1 : 0);
					continue;

				case INET_LOPT_BIT8:
					ptr.write(opt);
					ptr.writeInt(bit8f ? (bit8 ? 1 : 0) : INET_BIT8_OFF);
					continue;

				case INET_LOPT_TCP_HIWTRMRK:
					if (stype == ProtocolType.STREAM) {
						ptr.write(opt);
						ptr.writeInt(high);
					}
					continue;

				case INET_LOPT_TCP_LOWTRMRK:
					if (stype == ProtocolType.STREAM) {
						ptr.write(opt);
						ptr.writeInt(low);
					}
					continue;

				case INET_LOPT_TCP_SEND_TIMEOUT:
					if (stype == ProtocolType.STREAM) {
						ptr.write(opt);
						ptr.writeInt(send_timeout);
					}
					continue;

				case INET_LOPT_TCP_SEND_TIMEOUT_CLOSE:
					if (stype == ProtocolType.STREAM) {
						ptr.write(opt);
						ptr.writeInt(send_timeout_close ? 1 : 0);
					}
					continue;

				case INET_LOPT_TCP_DELAY_SEND:
					if (stype == ProtocolType.STREAM) {
						ptr.write(opt);
						ptr
								.writeInt(((tcp_add_flags & TCP_ADDF_DELAY_SEND) != 0) ? 1
										: 0);
					}
					continue;

				case INET_LOPT_READ_PACKETS:
					if (stype == ProtocolType.STREAM) {
						ptr.write(opt);
						ptr.writeInt(read_packets);
					}
					continue;

				case INET_OPT_RAW:
					/* ignore (in Java7 we can do some...) */
					continue;

				case INET_OPT_PRIORITY:
					ptr.write(opt);
					ptr.writeInt(0);
					continue;

				case INET_OPT_TOS:
					ptr.write(opt);
					ptr.writeInt(0);
					continue;

				case INET_OPT_REUSEADDR:
					try {
						boolean val = fd.getReuseAddress();
						ptr.write(opt);
						ptr.writeInt(val ? 1 : 0);
						continue;
					} catch (IOException e) {
						// ignore //
						continue;
					}

				case INET_OPT_KEEPALIVE:
					try {
						boolean val = fd.getKeepAlive();
						ptr.write(opt);
						ptr.writeInt(val ? 1 : 0);
						continue;
					} catch (IOException e) {
						// ignore //
						continue;
					}

				case INET_OPT_SNDBUF:
					try {
						int val = fd.getSendBufferSize();
						ptr.write(opt);
						ptr.writeInt(val);
						continue;
					} catch (IOException e) {
						// ignore //
						continue;
					}
				case INET_OPT_RCVBUF:
					try {
						int val = fd.getReceiveBufferSize();
						ptr.write(opt);
						ptr.writeInt(val);
						continue;
					} catch (IOException e) {
						// ignore //
						continue;
					}
				case TCP_OPT_NODELAY:
					try {
						boolean val = fd.getNoDelay();
						ptr.write(opt);
						ptr.writeInt(val ? 1 : 0);
						continue;
					} catch (IOException e) {
						// ignore //
						continue;
					}
				default:
					throw new NotImplemented("getopt " + ((int) opt));
				}
			}

			ptr.close();
			byte[] b = barr.toByteArray();
			ByteBuffer res = ByteBuffer.wrap(b);
			res.position(b.length);
			return res;

		} catch (IOException e) {
			e.printStackTrace();
			return ctl_error(IO.exception_to_posix_code(e));
		}

	}

	private ByteBuffer tcp_accept(EPID caller, ByteBuffer buf) throws Pausable {

		if ((state != TCP_STATE_LISTEN && state != TCP_STATE_ACCEPTING && state != TCP_STATE_MULTI_ACCEPTING)
				|| buf.remaining() != 4) {
			return ctl_error(Posix.EINVAL);
		}

		int timeout = buf.getInt();

		switch (state) {
		case TCP_STATE_ACCEPTING: {
			long time_left;
		    MultiTimerData mtd = null, omtd = null;
		    ERef monitor;

		    if ((monitor = driver_monitor_process(caller)) == null) {
		    	return ctl_xerror(NOPROC);
		    }
		    
		    AsyncOp op = deq_async_w_tmo();
		    if (op.timeout != INET_INFINITY) {
		    	time_left = driver_read_timer();
		    	driver_cancel_timer();
		    	
		    	if (time_left <= 0) {
		    		time_left = 1;
		    	}
		    	omtd = add_multi_timer(op.caller, time_left); 
		    }
		    enq_old_multi_op(op, omtd);
		    if (timeout != INET_INFINITY) {
		    	mtd = add_multi_timer(caller, timeout);
		    }
		    short id = enq_multi_op(TCP_REQ_ACCEPT, caller, mtd, monitor);
		    byte[] data = new byte[2];
		    data[0] = (byte) (id >>> 8);
		    data[1] = (byte) (id & 0xff);
		    state = TCP_STATE_MULTI_ACCEPTING;
		    return ctl_reply(INET_REP_OK, data);
		}
			// break;

		case TCP_STATE_MULTI_ACCEPTING:
		{
		    MultiTimerData mtd = null, omtd = null;
		    ERef monitor;

		    if ((monitor = driver_monitor_process(caller)) == null) {
		    	return ctl_xerror(NOPROC);
		    }
		    
		    if (timeout != INET_INFINITY) {
		    	mtd = add_multi_timer(caller, timeout);
		    }
		    
		    short id = enq_multi_op(TCP_REQ_ACCEPT, caller, mtd, monitor);
		    byte[] data = new byte[2];
		    data[0] = (byte) (id >>> 8);
		    data[1] = (byte) (id & 0xff);
		    return ctl_reply(INET_REP_OK, data);

		}
			// break;

		case TCP_STATE_LISTEN:
			ByteBuffer reply = ByteBuffer.allocate(2);
			InetSocket sock;
			try {
				sock = fd.accept();
			} catch (IOException e) {
				return ctl_error(IO.exception_to_posix_code(e));
			}
			if (sock == null) {
				// async ...
				ERef monitor = driver_monitor_process(caller);
				if (monitor == null) {
					return ctl_xerror(NOPROC);
				}
				enq_async_w_tmo(caller, reply, TCP_REQ_ACCEPT, timeout, monitor);
				state = TCP_STATE_ACCEPTING;
				sock_select(ERL_DRV_ACCEPT, SelectMode.SET);
				if (timeout != INET_INFINITY) {
					driver_set_timer(timeout);
				}
			} else {
				// we got a connection

				try {
					sock.setNonBlocking();
				} catch (IOException e) {
					return ctl_error(IO.exception_to_posix_code(e));
				}
				TCPINet accept_desc = this.copy(caller, sock);
				accept_desc.state = TCP_STATE_CONNECTED;
				enq_async(caller, reply, TCP_REQ_ACCEPT);
				async_ok_port(caller, accept_desc.port());
			}

			return ctl_reply(INET_REP_OK, reply.array());

		default:
			throw new InternalError();
		}
	}
	
	private MultiTimerData add_multi_timer(EPID caller, long timeout) {

		MultiTimerData mtd = new MultiTimerData();
		mtd.when = System.currentTimeMillis() + timeout;
		mtd.caller = caller;

		if (this.mtd_queue == null) {
			this.mtd_queue = new PriorityQueue<MultiTimerData>();
		}
		
		this.mtd_queue.add(mtd);
		
		if (this.mtd_queue.peek() == mtd) {			
			// are we in front?
			
			if (this.mtd_queue.size() > 1) {
				// we're not alone, so there must already be a timer set
				driver_cancel_timer();
			}
			
			// then set our timer.
			driver_set_timer(timeout);			
		}
		
		return mtd;
	}


	private AsyncMultiOp remove_multi_op(EHandle caller) {
		
		AsyncMultiOp opp, slap = null;
		
		for (opp = multi_first; 
			 opp != null && opp.op.caller != caller;
			 slap = opp, opp = opp.next
		) {
			/* skip */
		}
		
		if (opp == null) {
			return null;
		}
		
		if (slap == null) {
			multi_first = opp.next;
		} else {
			slap.next = opp.next;
		}
		
		if (multi_last == opp) {
			multi_last = slap;
		}
		
		opp.next = null;
		return opp;
	}

	private short enq_multi_op(int req, EPID caller,
			MultiTimerData timeout, ERef monitor) {

		short id = (short) (aid.incrementAndGet() & 0xffff);
		
		AsyncOp op = new AsyncOp(id, caller, req, -1, monitor);
		enq_old_multi_op(op, timeout);
		
		return id;
	}
	
	AsyncMultiOp deq_multi_op() {
		AsyncMultiOp first = multi_first;
		if (first == null) {
			return null;
		}
		
		multi_first = first.next;
		if (multi_first == null) {
			multi_last = null;
		} 
		first.next = null;
		return first;
	}

	private void enq_old_multi_op(AsyncOp op, MultiTimerData timeout) {
		AsyncMultiOp opp = new AsyncMultiOp();
		opp.op = op;
		opp.timeout = timeout;
		
		if (this.multi_first == null) {
			multi_first = opp;
		} else {
			multi_last.next = opp;
		}
		multi_last = opp;
	}

	private boolean async_ok_port(EPID caller, EInternalPort port2) throws Pausable {
		AsyncOp op = deq_async();
		if (op == null) {
			return false;
		}
		return send_async_ok_port(op.id, caller, port2);
	}

	private ByteBuffer inet_name(ByteBuffer buf) {

		if (!is_bound()) {
			return ctl_error(Posix.EINVAL);
		}

		InetSocketAddress addr = (InetSocketAddress) fd.getLocalSocketAddress();
		int port = addr.getPort();
		byte[] ip = addr.getAddress().getAddress();

		ByteBuffer out = ByteBuffer.allocate(4 + ip.length);

		out.put(INET_REP_OK);

		out.put(encode_proto_family(this.sfamily));
		out.putShort((short) port);
		out.put(ip);

		return out;
	}

	private ByteBuffer tcp_listen(ByteBuffer buf) {
		if (state == TCP_STATE_CLOSED || !is_open()) {
			return ctl_xerror(EXBADPORT);
		} else if (!is_bound()) {
			return ctl_xerror(EXBADSEQ);
		} else if (buf.remaining() != 2) {
			return ctl_error(Posix.EINVAL);
		}

		int backlog = buf.getShort() & 0xffff;

		try {
			this.fd.listen(backlog);
		} catch (IOException e) {
			return ctl_error(IO.exception_to_posix_code(e));
		}

		state = TCP_STATE_LISTEN;
		return ctl_reply(INET_REP_OK);
	}

	/*
	 * set socket options:* return -1 on error* 0 if ok* 1 if ok force deliver
	 * of queued data
	 */

	private int inet_set_opts(ByteBuffer buf) throws Pausable {

		PacketParseType old_htype = this.htype;
		ActiveType old_active = this.active;
		boolean propagate = false;/*
								 * Set to true if failure to set this option
								 * should be propagated to erlang (not all
								 * errors can be propagated for BC reasons)
								 */

		int res = 0;

		while (buf.remaining() >= 5) {
			byte opt = buf.get();
			int ival = buf.getInt();

			switch (opt) {
			case INET_LOPT_HEADER:
				this.hsz = ival;
				continue;

			case INET_LOPT_MODE:
				this.mode = ival;
				continue;

			case INET_LOPT_DELIVER:
				this.deliver = ival;
				continue;

			case INET_LOPT_BUFFER:
				if (ival > INET_MAX_BUFFER)
					ival = INET_MAX_BUFFER;
				else if (ival < INET_MIN_BUFFER)
					ival = INET_MIN_BUFFER;
				this.bufsz = ival;
				continue;

			case INET_LOPT_ACTIVE:
				this.active = ActiveType.valueOf(ival);
				if ((stype == ProtocolType.STREAM)
						&& (active != ActiveType.PASSIVE)
						&& (state == INET_STATE_CLOSED)) {
					tcp_closed_message();
					if (this.exitf) {
						driver_exit(0);
						return 0; /* Give up on this socket, descriptor lost */
					} else {
						desc_close_read();
					}
				}
				res = 1;
				continue;

			case INET_LOPT_PACKET:
				this.htype = PacketParseType.valueOf(ival);
				continue;

			case INET_LOPT_PACKET_SIZE:
				this.psize = (int) ival;
				continue;

			case INET_LOPT_EXITONCLOSE:
				System.err.println("setting exitf="+(ival != 0)+" on "+this);
				this.exitf = (ival == 0) ? false : true;
				continue;

			case INET_LOPT_BIT8:
				switch (ival) {
				case INET_BIT8_ON:
					this.bit8f = true;
					this.bit8 = false;
					break;
				case INET_BIT8_OFF:
					this.bit8f = false;
					this.bit8 = false;
					break;
				case INET_BIT8_CLEAR:
					this.bit8f = true;
					this.bit8 = false;
					break;
				case INET_BIT8_SET:
					this.bit8f = true;
					this.bit8 = true;
					break;
				}
				continue;

			case INET_LOPT_TCP_HIWTRMRK:
				if (this.stype == ProtocolType.STREAM) {
					if (ival < 0)
						ival = 0;
					else if (ival > INET_MAX_BUFFER * 2)
						ival = INET_MAX_BUFFER * 2;
					if (this.low > ival)
						this.low = ival;
					this.high = ival;
				}
				continue;

			case INET_LOPT_TCP_LOWTRMRK:
				if (this.stype == ProtocolType.STREAM) {
					if (ival < 0)
						ival = 0;
					else if (ival > INET_MAX_BUFFER)
						ival = INET_MAX_BUFFER;
					if (this.high < ival)
						this.high = ival;
					this.high = ival;
				}
				continue;

			case INET_LOPT_TCP_SEND_TIMEOUT:
				if (this.stype == ProtocolType.STREAM) {
					this.send_timeout = ival;
				}
				continue;

			case INET_LOPT_TCP_SEND_TIMEOUT_CLOSE:
				if (this.stype == ProtocolType.STREAM) {
					this.send_timeout_close = ival == 0 ? false : true;
				}
				continue;

			case INET_LOPT_TCP_DELAY_SEND:
				if (this.stype == ProtocolType.STREAM) {
					if (ival != 0)
						this.tcp_add_flags |= TCP_ADDF_DELAY_SEND;
					else
						this.tcp_add_flags &= ~TCP_ADDF_DELAY_SEND;
				}
				continue;

			case INET_LOPT_READ_PACKETS:
				if (this.stype == ProtocolType.STREAM) {
					if (ival <= 0)
						return -1;
					this.read_packets = ival;
				}
				continue;

			case INET_OPT_REUSEADDR:
				try {
					fd.setReuseAddress(ival != 0);
				} catch (IOException e) {
					if (propagate)
						return -1;
				}
				continue;

			case INET_OPT_KEEPALIVE:
				try {
					fd.setKeepAlive(ival != 0);
				} catch (IOException e) {
					if (propagate)
						return -1;
				}
				break;

			case INET_OPT_DONTROUTE:
				// TODO: WARN?
				continue;

			case INET_OPT_BROADCAST:
				try {
					fd.setBroadcast(ival != 0);
				} catch (IOException e) {
					if (propagate)
						return -1;
				}
				continue;

			case INET_OPT_OOBINLINE:
				try {
					fd.setOOBInline(ival != 0);
				} catch (IOException e) {
					if (propagate)
						return -1;
				}
				continue;

			case INET_OPT_SNDBUF:
				try {
					fd.setSendBufferSize(ival);
				} catch (IOException e) {
					if (propagate)
						return -1;
				}
				continue;

			case INET_OPT_RCVBUF:
				try {
					fd.setReceiveBufferSize(ival);
					if (ival > this.bufsz) {
						this.bufsz = ival;
					}
				} catch (IOException e) {
					if (propagate)
						return -1;
				}
				continue;

			case INET_OPT_LINGER:
				if (buf.remaining() < 4)
					return -1;
				int linger = buf.getInt();
				try {
					fd.setLinger(ival != 0, linger);
				} catch (IOException e) {
					if (propagate)
						return -1;
				}
				continue;

			case INET_OPT_PRIORITY:
				// TODO: WARN?
				continue;

			case INET_OPT_TOS:
				try {
					fd.setTrafficClass(ival);
				} catch (IOException e) {
					if (propagate)
						return -1;
				}
				continue;

			case TCP_OPT_NODELAY:
				try {
					fd.setTcpNoDelay(ival != 0);
				} catch (IOException e) {
					if (propagate)
						return -1;
				}
				continue;

				/** NO MULTICAST SUPPORT **/
				/**
				 * case UDP_OPT_MULTICAST_TTL: try { fd.setTimeToLive(ival); }
				 * catch (IOException e) { if (propagate) return -1; } continue;
				 * 
				 * case UDP_OPT_MULTICAST_LOOP: try { fd.setLoopbackMode(ival ==
				 * 0); } catch (SocketException e) { if (propagate) return -1; }
				 * continue;
				 * 
				 * case UDP_OPT_MULTICAST_IF: try { byte[] ip = int_to_ip(ival);
				 * InetAddress addr = InetAddress.getByAddress(ip);
				 * fd.setInterface(addr); } catch (IOException e) { if
				 * (propagate) return -1; } continue;
				 * 
				 * case UDP_OPT_ADD_MEMBERSHIP: try { // TODO: figure out which
				 * port to use? // in the erlang call sequence, we have not yet
				 * bound // the socket; so how can we know which port int port =
				 * 0; if (fd.isBound()) { port = fd.getLocalPort(); } byte[] ip
				 * = int_to_ip(ival); InetAddress mcast =
				 * InetAddress.getByAddress(ip); SocketAddress mcastaddr = new
				 * InetSocketAddress(mcast, port); InetAddress iaddr =
				 * InetAddress.getByAddress(int_to_ip(buf .getInt()));
				 * NetworkInterface netIf = NetworkInterface
				 * .getByInetAddress(iaddr); fd.joinGroup(mcastaddr, netIf); }
				 * catch (IOException e) { if (propagate) return -1; } continue;
				 * 
				 * case UDP_OPT_DROP_MEMBERSHIP: try { // TODO: figure out which
				 * port to use? // in the erlang call sequence, we have not yet
				 * bound // the socket; so how can we know which port int port =
				 * 0; if (fd.isBound()) { port = fd.getLocalPort(); } byte[] ip
				 * = int_to_ip(ival); InetAddress mcast =
				 * InetAddress.getByAddress(ip); SocketAddress mcastaddr = new
				 * InetSocketAddress(mcast, port); InetAddress iaddr =
				 * InetAddress.getByAddress(int_to_ip(buf .getInt()));
				 * NetworkInterface netIf = NetworkInterface
				 * .getByInetAddress(iaddr); fd.leaveGroup(mcastaddr, netIf); }
				 * catch (IOException e) { if (propagate) return -1; } continue;
				 **/
			default:
				return -1;

			}

		}

		if (((stype == ProtocolType.STREAM) && is_connected())
				|| ((stype == ProtocolType.DGRAM) && is_open())) {

			if (active != old_active)
				sock_select(ERL_DRV_READ,
						active == ActiveType.PASSIVE ? SelectMode.CLEAR
								: SelectMode.SET);

			if ((stype == ProtocolType.STREAM) && active != ActiveType.PASSIVE) {
				if (old_active == ActiveType.PASSIVE || (htype != old_htype)) {
					/*
					 * passive => active change OR header type change in active
					 * mode
					 */
					return 1;
				}
				return 0;
			}
		}
		return 0;
	}

	/** decode 0x12345678 into byte[]{ 0x12, 0x34, 0x56, 0x78 } */
	private byte[] int_to_ip(int ival) {
		return new byte[] { (byte) ((ival >>> 24) & 0xff),
				(byte) ((ival >>> 16) & 0xff), (byte) ((ival >>> 8) & 0xff),
				(byte) ((ival) & 0xff), };
	}

	private void desc_close_read() {
		sock_select(ERL_DRV_READ|ERL_DRV_USE, SelectMode.CLEAR);
	}

	private void tcp_closed_message() throws Pausable {
		if ((tcp_add_flags & TCP_ADDF_CLOSE_SENT) == 0) {
			tcp_add_flags |= TCP_ADDF_CLOSE_SENT;
			driver_output_term(new ETuple2(am_tcp_closed, port()));
		}
	}

	/**
	 * deliver len bytes (from i_ptr_start...)
	 * 
	 * @return number of packets delivered
	 * @throws Pausable 
	 */
	private int tcp_deliver(int len) throws Pausable {

		if (ERT.DEBUG_INET)
			System.err.println("tcp_deliver " + i_ptr_start + ":" + len);

		int count = 0;
		int n;
		int[] lenp = new int[] { len };

		if (len == 0) {

			if (i_buf == null || i_remain > 0) {
				return count;
			}

			n = tcp_remain(lenp);
			len = lenp[0];
			if (n != 0) {
				if (n < 0) {

					if (ERT.DEBUG_INET)
						System.err.println("tcp_deliver::packet_error " + n);

					
					/* packet error */
					return n;
				}
				if (len > 0) /* more data pending */
					i_remain = len;
				return count;
			}
		}

		while (len > 0) {
			if (ERT.DEBUG_INET)
				System.err.println("deliver.2");
			int code = 0;

			inet_input_count(len);
			
			if (len * 4 >= i_buf.capacity() * 3) { /* >=75% */
				if (ERT.DEBUG_INET)
						System.err.println("deliver.2.1");
				/* something after? */
				if (i_ptr_start + len == i_buf.position()) { /* no */
					code = tcp_reply_data(i_buf.array(), i_buf
							.arrayOffset()
							+ i_ptr_start, len);
					if (code >= 0) tcp_clear_input();
				} else { /* move trail to beginning of a new buffer */
					ByteBuffer bin = ByteBuffer.allocate(i_buf.capacity());
					bin.put(i_buf.array(), i_buf.arrayOffset() + i_ptr_start
							+ len, i_buf.position() - len - i_ptr_start);

					code = tcp_reply_data(i_buf.array(), i_buf
							.arrayOffset()
							+ i_ptr_start, len);
					if (code >= 0) {
					i_buf = bin;
					i_ptr_start = 0;
					i_remain = 0;
					}
				}
			} else {
				if (ERT.DEBUG_INET)
					System.err.println("deliver.2.2");
				// are we sending all we've got?
				boolean share_ok = i_ptr_start + len == i_buf.position();
				if (share_ok) {
					code = tcp_reply_data(i_buf.array(), i_buf.arrayOffset()
							+ i_ptr_start, len);
					if (code >= 0)
					tcp_clear_input();
				} else {
					byte[] data = new byte[len];
					System.arraycopy(i_buf.array(), i_buf.arrayOffset()
							+ i_ptr_start, data, 0, len);
					code = tcp_reply_data(data, 0, len);
					if (code >= 0) {
					i_ptr_start += len;
					i_remain = 0;
					}
				}
			}

			if (code < 0) {
				if (ERT.DEBUG_INET)
					System.err.println("tcp_deliver::error(code) " + code);

			return code;
			}
			
			count++;
			len = 0;

			if (active == ActiveType.PASSIVE) {
				driver_cancel_timer();
				sock_select(ERL_DRV_READ | 0, SelectMode.CLEAR);
				if (i_buf != null) {
					tcp_restart_input();
				}
			} else if (i_buf != null) {
				lenp[0] = len;
				n = tcp_remain(lenp);
				len = lenp[0];
				if (n != 0) {
					if (n < 0) /* packet error */
						return n;
					tcp_restart_input();
					if (len > 0)
						i_remain = len;
					len = 0;
				}
			}
		}
		return count;
	}

	/*
	 * * active=TRUE:* (NOTE! distribution MUST use active=TRUE, deliver=PORT)*
	 * deliver=PORT {S, {data, [H1,..Hsz | Data]}}* deliver=TERM {tcp, S,
	 * [H1..Hsz | Data]}** active=FALSE:* {async, S, Ref, {ok,[H1,...Hsz |
	 * Data]}}
	 */

	private void inet_input_count(int len) {
		recv_cnt += 1;
		recv_oct += len;
		
		double avg = recv_avg;
		double dvi = recv_dvi;
		
		recv_avg = avg + (len - avg) / recv_cnt;
		if (len > recv_max)
			recv_max = len;
	}

	private void tcp_restart_input() {
		if (i_ptr_start != 0) {
			int n = i_buf.position() - i_ptr_start;
			System.arraycopy(i_buf.array(), i_buf.arrayOffset() + i_ptr_start,
					i_buf.array(), i_buf.arrayOffset(), n);
			i_ptr_start = 0;
			i_buf.position(n);
		}
	}

	private int tcp_reply_data(byte[] ib, int start, int len) throws Pausable {

		if (ERT.DEBUG_INET)
			System.err.println("tcp_reply_data len="+len);
		
		ByteBuffer out = ByteBuffer.wrap(ib, start, len);
		Packet.get_body(htype, out);
		start = out.position();
		int bodylen = out.remaining();

		scanbit8(out);

		out = out.slice();
		out.position(out.limit());

		int code;
		if (deliver == INET_DELIVER_PORT) {
			code = inet_port_data(out);
		} else if ((code = Packet.parse(htype, ib, start, len, http_state,
				packet_callbacks, this)) == 0) {
			if (active == ActiveType.PASSIVE) {
				return inet_async_data(0, out);
			} else {
				code = tcp_message(out);
			}
		}

		if (code < 0)
			return code;

		if (active == ActiveType.ACTIVE_ONCE) {
			active = ActiveType.PASSIVE;
		}

		return code;
	}

	/** data is at 0 .. out.position() 
	 * @throws Pausable */
	private int tcp_message(ByteBuffer out) throws Pausable {
		int hsz = this.hsz;

		EObject msg;
		EObject data;

		if (mode == INET_MODE_LIST) {
			out.flip();
			data = EString.make(out);
		} else {
			out.position(hsz);
			ByteBuffer tail = out.slice();

			out.flip();
			ByteBuffer header = out;

			data = new EBinList(header, EBinary.make(tail));
		}

		msg = ETuple.make(am_tcp, port(), data);

		// System.out.println("sending "+msg);

		driver_output_term(msg);

		return 0;
	}

	/*
	 * * passive mode reply:* {inet_async, S, Ref, {ok,[H1,...Hsz | Data]}}* NB:
	 * this is for TCP only;* UDP and SCTP use inet_async_binary_data .
	 */
	private int inet_async_data(int i, ByteBuffer out) {

		AsyncOp op = deq_async();
		if (op == null)
			return -1;

		if (ERT.DEBUG_INET)
			System.err.println("deq " + op.caller + "::" + op.id);

		EObject data;
		if (mode == INET_MODE_LIST) {
			out.flip();
			data = EString.make(out);
		} else {
			// TODO: FIGURE OUT IF THIS IS RIGHT
			out.position(hsz);
			ByteBuffer tail = out.slice();
			out.flip();
			data = EBinary.make(tail);
		}

		ETuple res = ETuple.make(am_inet_async, port(), ERT.box(op.id),
				new ETuple2(ERT.am_ok, data));

		if (ERT.DEBUG_INET) {
			System.out.println("sending to " + op.caller + " ! " + res);
		}

		op.caller.sendb(res);

		return 0;
	}

	/** out has data from 0..out.position() 
	 * @throws Pausable */
	private int inet_port_data(ByteBuffer out) throws Pausable {

		int hsz = this.hsz;
		if (mode == INET_MODE_LIST || (hsz > out.remaining())) {
			driver_output(out);
			return 0;
		} else if (hsz > 0) {
			out.limit(out.position());
			out.position(hsz);
			ByteBuffer tail = out.slice();
			tail.position(tail.limit());
			out.limit(hsz);

			driver_output2(out, tail);
			return 0;
		} else {
			driver_output(out);
			return 0;
		}

	}

	private int tcp_reply_binary_data(byte[] ib, int start, int len) throws Pausable {

		ByteBuffer out = ByteBuffer.wrap(ib, start, len);
		Packet.get_body(htype, out);
		start = out.position();
		int bodylen = out.remaining();

		scanbit8(out);

		int code;
		if (deliver == INET_DELIVER_PORT) {
			code = inet_port_binary_data(out);
		} else if ((code = Packet.parse(htype, ib, start, len, http_state,
				packet_callbacks, this)) == 0) {
			if (active == ActiveType.PASSIVE) {
				return inet_async_binary_data(0, out);
			} else {
				code = tcp_binary_message(out);
			}
		}

		if (code < 0)
			return code;

		if (active == ActiveType.ACTIVE_ONCE) {
			active = ActiveType.PASSIVE;
		}

		return code;
	}

	private int tcp_binary_message(ByteBuffer out) {
		throw new erjang.NotImplemented();

	}

	private int inet_async_binary_data(int i, ByteBuffer out) {
		throw new erjang.NotImplemented();

	}

	private int inet_port_binary_data(ByteBuffer out) {
		throw new erjang.NotImplemented();

	}

	private void scanbit8(ByteBuffer out) {
		if (!bit8f || bit8)
			return;

		char c = 0;
		int rem = out.remaining();
		for (int i = 0; i < rem; i++) {
			c |= out.get(i);
		}
		bit8 = ((c & 0x80) != 0);

	}

	private void tcp_clear_input() {
		i_buf = null;
		i_ptr_start = 0;
		i_remain = 0;
	}

	private int tcp_expand_buffer(int len) {

		int used = i_ptr_start;
		int ulen = used + len;

		if (i_buf.limit() >= ulen) {
			/* packet will fit (within limit) */
			return 0;
		} else if (i_buf.capacity() >= ulen) {
			/* capacity is large enough to grow limit */
			i_buf.limit(ulen);
			return 0;
		}

		try {
			// allocate a new buffer of size "ulen"
			ByteBuffer bin = ByteBuffer.allocate(ulen);

			// copy the old buffer into this new buffer
			i_buf.flip();
			bin.put(i_buf);

			// and make the new buffer be the real thing
			i_buf = bin;
			return 0;

		} catch (OutOfMemoryError e) {
			return -1;
		}
	}

	/**
	 * i_buf is a ByteBuffer, which has position() at the end of read input. The
	 * variable i_ptr_start is an index into 0..position() which marks the
	 * position of the first byte that has not been delivered to the client.
	 * 
	 * i_ptr === i_buf.position() i_bufsz === i_buf.limit(). ibuf->orig_size ===
	 * i_buf.capacity().
	 * */

	private final int i_ptr() {
		return i_buf.position();
	}

	private final int i_bufsz() {
		return i_buf.limit();
	}

	private final void i_bufsz(int pos) {
		i_buf.limit(pos);
	}

	private final int i_buf_origsz() {
		return i_buf.capacity();
	}

	/** push data in front of the input buffer (unget) */
	private int tcp_push_buffer(byte[] data, int off, int len) {
		if (i_buf == null) {
			i_buf = ByteBuffer.allocate(len);
			i_buf.put(data, off, len);
			i_ptr_start = 0;
		} else {
			int sz_before = i_ptr_start;
			int sz_filled = i_buf.position() - i_ptr_start;

			if (len <= sz_before) {
				i_buf.position(sz_before - len);
				i_buf.put(data, off, len);
				i_ptr_start -= len;
			} else {
				ByteBuffer bin = ByteBuffer.allocate(len + i_buf.limit());
				bin.put(data, off, len);

				// semi-flip
				i_buf.position(i_ptr_start);
				i_buf.limit(sz_filled);
				bin.put(i_buf.array(), i_buf.arrayOffset() + i_ptr_start,
						sz_filled);
				i_buf = bin;
				i_ptr_start = 0;
			}
		}

		i_remain = 0;
		return 0;

	}

	private int tcp_remain(int[] lenp) {

		int nfill = i_buf.position();
		int nsz = i_buf.remaining();
		int n = nfill - i_ptr_start;

		int tlen;

		tlen = Packet.get_length(htype, i_buf.array(), i_buf.arrayOffset()
				+ i_ptr_start, n, this.psize, i_bufsz(), http_state);

		if (tlen > 0) {
			if (tlen <= n) {
				// got a packet
				lenp[0] = tlen;
				return 0;
			} else {
				if (tcp_expand_buffer(tlen) < 0) {
					return -1;
				}
				lenp[0] = (tlen - n);
				return lenp[0];
			}
		} else if (tlen == 0) {
			lenp[0] = 0;
			if (nsz == 0) {
				if (nfill == n) {
					return -1;
				} else {
					return nfill - n;
				}
			} else {
				return nsz;
			}
		}

		return -1;
	}

	private ByteBuffer inet_open(ByteBuffer cmd) {
		if (cmd.remaining() == 1) {
			byte family = cmd.get();

			if (family == INET_AF_INET || family == INET_AF_INET6) {
				return inet_ctl_open(decode_proto_family(family),
						ProtocolType.STREAM);
			}
		}

		return ctl_error(Posix.EINVAL);
	}

	private ByteBuffer inet_connect(EPID caller, ByteBuffer cmd) throws Pausable {
		/* INPUT: Timeout(4), Port(2), Address(N) */

		if (!is_open()) {
			return ctl_xerror(EXBADPORT);
		} else if (is_connected()) {
			return ctl_error(Posix.EISCONN);
		} else if (!is_bound()) {
			return ctl_xerror(EXBADSEQ);
		} else if (is_connecting()) {
			return ctl_error(Posix.EINVAL);
		}

		int timeout = cmd.getInt();

		remote = inet_set_address(sfamily, cmd);
		if (remote == null) {
			return ctl_error(Posix.EINVAL);
		}

		try {
			short aid;
			ByteBuffer reply = ByteBuffer.allocate(3);
			reply.put(INET_REP_OK);

			if (this.fd.connect(remote)) {
				// established

				state = TCP_STATE_CONNECTED;
				if (active != ActiveType.PASSIVE)
					sock_select(ERL_DRV_READ, SelectMode.SET);
				aid = enq_async(caller, reply, INET_REQ_CONNECT);
				async_ok();

			} else {
				// async

				state = TCP_STATE_CONNECTING;
				if (timeout != INET_INFINITY)
					driver_set_timer(timeout);

				sock_select(ERL_DRV_CONNECT, SelectMode.SET);

				aid = enq_async(caller, reply, INET_REQ_CONNECT);
			}

			return reply;
		} catch (IOException e) {
			e.printStackTrace();
			return ctl_error(IO.exception_to_posix_code(e));
		}
	}

	private ByteBuffer inet_bind(ByteBuffer cmd) {
		if (cmd.remaining() < 2)
			return ctl_error(Posix.EINVAL);

		if (state != INET_STATE_OPEN)
			return ctl_xerror(EXBADSEQ);

		InetSocketAddress addr = inet_set_address(sfamily, cmd);
		if (addr == null)
			return ctl_error(Posix.EINVAL);

		try {
			fd.bind(addr);
		} catch (IOException e1) {
			e1.printStackTrace();
			return ctl_error(IO.exception_to_posix_code(e1));
		}

		state = INET_STATE_BOUND;

		int port = addr.getPort();
		if (port == 0) {
			port = fd.getLocalPort();
		}

		ByteBuffer reply = ByteBuffer.allocate(3);
		reply.order(ByteOrder.nativeOrder());
		reply.put(INET_REP_OK);
		reply.putShort((short) (port & 0xFFFF));

		return reply;
	}

	private boolean async_ok() throws Pausable {
		AsyncOp op = deq_async();
		if (op == null) {
			return false;
		}
		return send_async_ok(op.id, op.caller);
	}

	AsyncOp deq_async() {
		return deq_async_w_tmo();
	}

	private AsyncOp deq_async_w_tmo() {
		if (opt.size() == 0)
			return null;
		return opt.get();
	}

	private boolean inet_reply_error(EObject reason) throws Pausable {
		/*
		 * send message:* {inet_reply, Port, Ref, {error,Reason}}
		 */
		EHandle caller = this.caller;
		this.caller = null;
		ETuple msg = ETuple.make(am_inet_reply, port(), new ETuple2(
				ERT.am_error, reason));
		if (ERT.DEBUG_PORT) {
			System.out.println("sending to " + caller + " ! " + msg);
		}
		
		driver_send_term(caller, msg);
		return true;
	}

	private boolean inet_reply_error(int reason) throws Pausable {
		return inet_reply_error(EAtom.intern(Posix.errno_id(reason)));
	}

	private boolean send_async_error(short id, EPID caller, EObject reason) throws Pausable {
		/*
		 * send message:* {inet_async, Port, Ref, {error,Reason}}
		 */

		ETuple msg = ETuple.make(am_inet_async, port(), ERT.box(id),
				new ETuple2(ERT.am_error, reason));
		if (ERT.DEBUG_PORT) {
			System.out.println("sending to " + caller + " ! " + msg);
		}
		caller.send(port(), msg);
		return  true;
	}

	private boolean send_async_ok(int id, EPID caller) throws Pausable {
		ETuple msg = ETuple.make(am_inet_async, port(), ERT.box(id), ERT.am_ok);
		if (ERT.DEBUG_PORT) {
			System.out.println("sending to " + caller + " ! " + msg);
		}
		caller.send(port(), msg);
		return true;
	}

	private boolean send_async_ok_port(int id, EPID caller, EPort port2) throws Pausable {
		ETuple msg = ETuple.make(am_inet_async, port(), ERT.box(id),
				new ETuple2(ERT.am_ok, port2));
		if (ERT.DEBUG_PORT) {
			System.out.println("sending to " + caller + " ! " + msg);
		}
		caller.send(port(), msg);
		return true;
	}

	private short enq_async(EPID caller, ByteBuffer reply, int req) {
		return enq_async_w_tmo(caller, reply, req, INET_INFINITY, (ERef) null);
	}

	static AtomicInteger aid = new AtomicInteger(0);

	private short enq_async_w_tmo(EPID caller, ByteBuffer reply, int req,
			int timeout, ERef monitor) {

		short id = (short) (aid.incrementAndGet() & 0x7fff);
		AsyncOp op = new AsyncOp(id, caller, req, timeout, monitor);

		if (reply != null) {
			reply.putShort(op.id);
		}

		opt.put(op);

		return op.id;
	}

	private InetSocketAddress inet_set_address(ProtocolFamily sfamily2,
			ByteBuffer cmd) {

		int port = cmd.getShort() & 0xffff;

		byte[] bytes;
		if (sfamily2 == ProtocolFamily.INET) {
			bytes = new byte[4];
		} else if (sfamily2 == ProtocolFamily.INET6) {
			bytes = new byte[16];
		} else {
			return null;
		}
		InetAddress addr;
		try {
			if (cmd.remaining() == 0) {
				addr = InetAddress.getLocalHost();
			} else {

				cmd.get(bytes);

				addr = InetAddress.getByAddress(bytes);
			}
		} catch (UnknownHostException e) {
			return null;
		}

		InetSocketAddress res = new InetSocketAddress(addr, port);

		return res;
	}

	private boolean is_connecting() {
		return (state & INET_F_CON) == INET_F_CON;
	}

	private boolean is_bound() {
		return (state & INET_F_BOUND) == INET_F_BOUND;
	}

	private boolean is_connected() {
		return (state & INET_STATE_CONNECTED) == INET_STATE_CONNECTED;
	}

	private boolean is_open() {
		return (state & INET_F_OPEN) == INET_F_OPEN;
	}

	private ByteBuffer inet_subscribe(EPID caller, ByteBuffer cmd) {
		ByteBuffer reply = ByteBuffer.allocate(1 + cmd.remaining() * 5);
		reply.put(INET_REP_OK);

		while (cmd.hasRemaining()) {
			byte op = cmd.get();
			if (op == INET_SUBS_EMPTY_OUT_Q) {

				reply.put(INET_SUBS_EMPTY_OUT_Q);

				int val = driver_sizeq();
				if (val > 0) {
					save_subscriber(empty_out_q_subs, caller);
				}

				reply.putInt(val);

			} else {
				throw new ErlangError(EAtom.intern("einval"));
			}
		}

		return reply;
	}

	private boolean save_subscriber(List<EHandle> subs, EPID pid) {

		subs.add(pid);

		return true;
	}

	private ByteBuffer inet_ctl_open(ProtocolFamily domain, ProtocolType type) {
		if (state != INET_STATE_CLOSED) {
			return ctl_xerror(EXBADSEQ);
		}

		// since we don't know if we will be connecting or listening we
		// need to delay that decision; for now, create a (client) socket.

		try {
			this.fd = InetSocket.open(domain, type, protocol);
			fd.configureBlocking(false);

		} catch (IOException e) {
			int code = IO.exception_to_posix_code(e);
			return ctl_error(code);
		}

		this.state = INET_STATE_OPEN;
		this.stype = type;
		this.sfamily = domain;

		return ctl_reply(INET_REP_OK);
	}

	private ProtocolFamily decode_proto_family(byte domain) {
		switch (domain) {
		case INET_AF_INET:
			return ProtocolFamily.INET;
		case INET_AF_INET6:
			return ProtocolFamily.INET6;
		case INET_AF_ANY:
			return ProtocolFamily.ANY;
		case INET_AF_LOOPBACK:
			return ProtocolFamily.LOOPBACK;
		default:
			throw new NotImplemented();
		}
	}

	private byte encode_proto_family(ProtocolFamily domain) {
		switch (domain) {
		case INET:
			return INET_AF_INET;
		case INET6:
			return INET_AF_INET6;
		case ANY:
			return INET_AF_ANY;
		case LOOPBACK:
			return INET_AF_LOOPBACK;
		default:
			throw new NotImplemented();
		}
	}

	private ByteBuffer ctl_reply(int code) {
		ByteBuffer buf = ByteBuffer.allocate(1);
		buf.put((byte) code);
		return buf;
	}

	private ByteBuffer ctl_xerror(byte[] exbadseq2) {
		return ctl_reply(INET_REP_ERROR, exbadseq2);
	}

	private ByteBuffer ctl_reply(int code, byte[] data) {
		ByteBuffer buf = ByteBuffer.allocate(1 + data.length);
		buf.put((byte) code);
		buf.put(data);
		return buf;
	}

	private ByteBuffer ctl_reply(int code, String data) {
		ByteBuffer buf = ByteBuffer.allocate(1 + data.length());
		buf.put((byte) code);
		for (int i = 0; i < data.length(); i++) {
			buf.put((byte) data.charAt(i));
		}
		return buf;
	}

	private ByteBuffer ctl_error(int err) {
		String id = Posix.errno_id(err);
		return ctl_reply(INET_REP_ERROR, id);
	}

	void state(StringBuffer sb) {
		if ((state & TCP_STATE_ACCEPTING) != 0) sb.append("Ac");
		if ((state & TCP_STATE_BOUND) != 0) sb.append("Bo");
		if ((state & TCP_STATE_CLOSED) != 0) sb.append("Clo");
		if ((state & TCP_STATE_CONNECTED) != 0) sb.append("Con");
		if ((state & TCP_STATE_CONNECTING) != 0) sb.append("Cog");
		if ((state & TCP_STATE_LISTEN) != 0) sb.append("Li");
		if ((state & TCP_STATE_MULTI_ACCEPTING) != 0) sb.append("Mu");
		if ((state & TCP_STATE_OPEN) != 0) sb.append("Op");
	}
	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer("TCPIP@");
		sb.append(System.identityHashCode(this));
		sb.append('[');

		sb.append("sock=").append(fd);

		sb.append("; state="); state(sb);
		
		sb.append("; active=").append(active);

		sb.append("; deliver=").append(deliver);

		sb.append("; select=").append(Integer.toBinaryString(event_mask));

		if (fd != null && fd.channel() != null) {
			SelectionKey sk = NIOSelector.interest(fd.channel());
			if (sk == null) {
				sb.append("; nointrest");
			} else if (!sk.isValid()) {

				sb.append("; cancelled");
			} else {

				try {
					sb.append("; ready="
							+ Integer.toBinaryString(sk.readyOps()));
					sb.append("; interest="
							+ Integer.toBinaryString(sk.interestOps()));
				} catch (CancelledKeyException e) {
					sb.append("; cancelled");
				}
			}
		}

		sb.append(']');

		return sb.toString();

	}

}
