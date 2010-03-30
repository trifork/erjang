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

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SelectableChannel;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import kilim.RingQueue;

import erjang.EAtom;
import erjang.EInternalPort;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.EProc;
import erjang.ERT;
import erjang.ERef;
import erjang.EString;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ErlangError;
import erjang.driver.EAsync;
import erjang.driver.EDriverInstance;
import erjang.driver.IO;
import erjang.driver.SelectMode;
import erjang.driver.efile.Posix;

public class TCPINet extends EDriverInstance {

	static class AsyncOp {

		/** id used to identify reply */
		short id;
		
		/** recipient of async reply */
		EPID caller;
		
		/* Request id (CONNECT/ACCEPT/RECV) */
		int req;
		
		ERef monitor;

		public int timeout;
	}
	
	static enum SockType {
		SOCK_STREAM, SOCK_DGRAM, SOCK_SEQPACKET;
	}
	
	static enum ActiveType {
		PASSIVE(0), ACTIVE(1), ACTIVE_ONCE(2);
		final int value;
		ActiveType(int val) { this.value = val; }
	}
	
	/* general address encode/decode tag */
	public static final int INET_AF_INET        =1;
	public static final int INET_AF_INET6       =2;
	public static final int INET_AF_ANY         =3; /* INADDR_ANY or IN6ADDR_ANY_INIT */
	public static final int INET_AF_LOOPBACK    =4; /* INADDR_LOOPBACK or IN6ADDR_LOOPBACK_INIT */

	/* INET_REQ_GETTYPE enumeration */
	public static final int INET_TYPE_STREAM    =1;
	public static final int INET_TYPE_DGRAM     =2;
	public static final int INET_TYPE_SEQPACKET =3;

	/* INET_LOPT_MODE options */
	public static final int INET_MODE_LIST      =0;
	public static final int INET_MODE_BINARY    =1;

	/* INET_LOPT_DELIVER options */
	public static final int INET_DELIVER_PORT   =0;
	public static final int INET_DELIVER_TERM   =1;

	/* INET_LOPT_ACTIVE options */
	public static final int INET_PASSIVE        =0;  /* false */
	public static final int INET_ACTIVE         =1;  /* true */
	public static final int INET_ONCE           =2;  /* true; active once then passive */

	/* INET_REQ_GETSTATUS enumeration */
	public static final int INET_F_OPEN         =0x0001;
	public static final int INET_F_BOUND        =0x0002;
	public static final int INET_F_ACTIVE       =0x0004;
	public static final int INET_F_LISTEN       =0x0008;
	public static final int INET_F_CON          =0x0010;
	public static final int INET_F_ACC          =0x0020;
	public static final int INET_F_LST          =0x0040;
	public static final int INET_F_BUSY         =0x0080;
	public static final int INET_F_MULTI_CLIENT =0x0100; /* Multiple clients for one descriptor, i.e. multi-accept */

	/* One numberspace for *_REC_* so if an e.g UDP request is issued
	** for a TCP socket, the driver can protest.
	*/
	public static final int INET_REQ_OPEN          =1;
	public static final int INET_REQ_CLOSE         =2;
	public static final int INET_REQ_CONNECT       =3;
	public static final int INET_REQ_PEER          =4;
	public static final int INET_REQ_NAME          =5;
	public static final int INET_REQ_BIND          =6;
	public static final int INET_REQ_SETOPTS       =7;
	public static final int INET_REQ_GETOPTS       =8;
	/* public static final int INET_REQ_GETIX         9  NOT USED ANY MORE */
	/* public static final int INET_REQ_GETIF         10 REPLACE BY NEW STUFF */
	public static final int INET_REQ_GETSTAT       =11;
	public static final int INET_REQ_GETHOSTNAME   =12;
	public static final int INET_REQ_FDOPEN        =13;
	public static final int INET_REQ_GETFD         =14;
	public static final int INET_REQ_GETTYPE       =15;
	public static final int INET_REQ_GETSTATUS     =16;
	public static final int INET_REQ_GETSERVBYNAME =17;
	public static final int INET_REQ_GETSERVBYPORT =18;
	public static final int INET_REQ_SETNAME       =19;
	public static final int INET_REQ_SETPEER       =20;
	public static final int INET_REQ_GETIFLIST     =21;
	public static final int INET_REQ_IFGET         =22;
	public static final int INET_REQ_IFSET         =23;
	public static final int INET_REQ_SUBSCRIBE     =24;
	/* TCP requests */
	public static final int TCP_REQ_ACCEPT         =40;
	public static final int TCP_REQ_LISTEN         =41;
	public static final int TCP_REQ_RECV           =42;
	public static final int TCP_REQ_UNRECV         =43;
	public static final int TCP_REQ_SHUTDOWN       =44;
	public static final int TCP_REQ_MULTI_OP       =45;
	/* UDP and SCTP requests */
	public static final int PACKET_REQ_RECV        =60; /* Common for UDP and SCTP         */
	public static final int SCTP_REQ_LISTEN	       =61; /* Different from TCP; not for UDP */
	public static final int SCTP_REQ_BINDX	       =62; /* Multi-home SCTP bind            */

	/* INET_REQ_SUBSCRIBE sub-requests */
	public static final byte INET_SUBS_EMPTY_OUT_Q  =1;


	/* *_REQ_* replies */
	public static final byte INET_REP_ERROR       =0;
	public static final byte INET_REP_OK          =1;
	public static final byte INET_REP_SCTP        =2;

	public static final int INET_STATE_CLOSED    =0;
	public static final int INET_STATE_OPEN      =(INET_F_OPEN);
	public static final int INET_STATE_BOUND     =(INET_STATE_OPEN | INET_F_BOUND);
	public static final int INET_STATE_CONNECTED =(INET_STATE_BOUND | INET_F_ACTIVE);

	public static final int TCP_STATE_CLOSED     =INET_STATE_CLOSED;
	public static final int TCP_STATE_OPEN       =(INET_F_OPEN);
	public static final int TCP_STATE_BOUND      =(TCP_STATE_OPEN | INET_F_BOUND);
	public static final int TCP_STATE_CONNECTED  =(TCP_STATE_BOUND | INET_F_ACTIVE);
	public static final int TCP_STATE_LISTEN     =(TCP_STATE_BOUND | INET_F_LISTEN);
	public static final int TCP_STATE_CONNECTING =(TCP_STATE_BOUND | INET_F_CON);
	public static final int TCP_STATE_ACCEPTING  =(TCP_STATE_LISTEN | INET_F_ACC);
	public static final int TCP_STATE_MULTI_ACCEPTING =(TCP_STATE_ACCEPTING | INET_F_MULTI_CLIENT);

	public static final byte[] EXBADPORT = "exbadport".getBytes(Charset.forName("ASCII"));
	public static final byte[] EXBADSEQ = "exbadseq".getBytes(Charset.forName("ASCII"));

	public static final int INET_INFINITY  =   0xffffffff;
	private static final EAtom am_inet_async = EAtom.intern("inet_async");
	
	private final EString command;
	private int state = INET_STATE_CLOSED;
	private SockType type;
	private byte family;
	private SocketChannel fd;
	private List<EObject> empty_out_q_subs = new ArrayList<EObject>(1);
	private InetSocketAddress remote;
	private ActiveType active = ActiveType.PASSIVE;
	private RingQueue<AsyncOp> opt = new RingQueue<AsyncOp>(2);
	
	public TCPINet(EString command) {
		this.command = command;
	}

	@Override
	protected void flush() {
		throw new erjang.NotImplemented();

	}

	@Override
	protected void output(ByteBuffer data) throws IOException {
		throw new erjang.NotImplemented();

	}

	@Override
	protected void outputv(ByteBuffer[] ev) throws IOException {
		throw new erjang.NotImplemented();
		
	}
	
	@Override
	protected void processExit(ERef monitor) {
		throw new erjang.NotImplemented();

	}

	@Override
	protected void readyAsync(EAsync data) {
		throw new erjang.NotImplemented();

	}

	@Override
	protected void readyInput(SelectableChannel ch) {
		throw new erjang.NotImplemented();

	}

	@Override
	protected void readyOutput(SelectableChannel evt) {
		throw new erjang.NotImplemented();

	}
	
	@Override
	public void readyConnect(SelectableChannel evt) {
		
		if (state == TCP_STATE_CONNECTING) {
			// clear select state
		    select(fd, ERL_DRV_CONNECT, SelectMode.CLEAR);
		    
		    // cancel the timer
			driver_cancel_timer(port());
			
			try {
				if (!fd.finishConnect()) {
					async_error(Posix.EIO);
					return;
				}
			} catch (IOException e) {
				async_error(IO.exception_to_posix_code(e));
				return;
			}
			
			if (active != ActiveType.PASSIVE) {
				select(fd, ERL_DRV_READ|ERL_DRV_WRITE, SelectMode.SET);
			}
			
			async_ok();
			
		}

		
	}
	
	private boolean async_error(int eio) {

		return async_error(EAtom.intern(Posix.errno_id(eio).toLowerCase()));
	}

	@Override
	/* Handling of timeout in driver */
	protected void timeout() {

		if ((state & INET_F_MULTI_CLIENT) != 0) {
			
		} else if ((state & TCP_STATE_CONNECTED) == TCP_STATE_CONNECTED) {
			
		} else if ((state & TCP_STATE_CONNECTING) == TCP_STATE_CONNECTING) {

			erl_inet_close();
			async_error(ERT.am_timeout);
			
		} else if ((state & TCP_STATE_ACCEPTING) == TCP_STATE_ACCEPTING) {
			
		}
	}
	
	private boolean async_error(EAtom reason) {
		AsyncOp op = deq_async();
		if (op == null) {
			return false;
		}
		return send_async_error(op.id, op.caller, reason);
	}

	private void erl_inet_close() {

		try {
			fd.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

	@Override
	protected ByteBuffer control(EPID caller, int command, ByteBuffer cmd) {
		
		switch (command) {
		case INET_REQ_OPEN:
			if (cmd.remaining()==1) {
				byte family = cmd.get();
				
				if (family == INET_AF_INET || family == INET_AF_INET6) {
					return inet_ctl_open(family, SockType.SOCK_STREAM);
				}
			}
			
			return ctl_error(Posix.EINVAL);

		case INET_REQ_SUBSCRIBE:
			return inet_subscribe(caller, cmd);
			
			
		case INET_REQ_BIND:
			return inet_bind(cmd);
			
		case INET_REQ_CONNECT:
			/* INPUT: Timeout(4), Port(2), Address(N) */

			
			if (!is_open()) {
				return ctl_xerror(EXBADPORT);
			} else if (is_conected()) {
				return ctl_error(Posix.EISCONN);
			} else if (!is_bound()) {
				return ctl_xerror(EXBADSEQ);
			} else if (is_connecting()) {
				return ctl_error(Posix.EINVAL);
			}
			
			int timeout = cmd.getInt();
			
			remote = inet_set_address(family, cmd);
			
			try {
				short aid;
				ByteBuffer reply = ByteBuffer.allocate(3);
				reply.put(INET_REP_OK);
				
				if (this.fd.connect(remote)) {
					// established
					
				    state = TCP_STATE_CONNECTED;
				    if (active != ActiveType.PASSIVE)
				    	select(fd, ERL_DRV_READ|ERL_DRV_WRITE, SelectMode.SET);
				    aid = enq_async(caller, reply, INET_REQ_CONNECT);
				    async_ok();

				} else {
					// async
					
				    state = TCP_STATE_CONNECTING;
				    if (timeout != INET_INFINITY)
					   driver_set_timer(timeout);
				    
				    select(fd, ERL_DRV_CONNECT, SelectMode.SET);
				    
				    aid = enq_async(caller, reply, INET_REQ_CONNECT);
				}
				
				return reply;
			} catch (IOException e) {
				e.printStackTrace();
				return ctl_error(IO.exception_to_posix_code(e));
			}

			
			
		}
		
		throw new erjang.NotImplemented("tcp_inet control(cmd="+command+")");
		
	}

	private ByteBuffer inet_bind(ByteBuffer cmd) {
		if (cmd.remaining() < 2) 
			return ctl_error(Posix.EINVAL);

		if (state != INET_STATE_OPEN) 
			return ctl_xerror(EXBADSEQ);
		
		InetSocketAddress addr = inet_set_address(family, cmd);
		if (addr == null) 
			return ctl_error(Posix.EINVAL);
		
		try {
			fd.socket().bind(addr);
		} catch (IOException e1) {
			e1.printStackTrace();
			return ctl_error(IO.exception_to_posix_code(e1));
		}
		
		state = INET_STATE_BOUND;
		
		int port = addr.getPort();
		if (port == 0) {
			port = fd.socket().getLocalPort();
		}
		
		ByteBuffer reply = ByteBuffer.allocate(3);
		reply.order(ByteOrder.nativeOrder());
		reply.put(INET_REP_OK);
		reply.putShort((short) (port & 0xFFFF));
		
		return reply;
	}

	private boolean async_ok() {
		AsyncOp op = deq_async();
		if (op == null) {
			return false;
		}
		return send_async_ok(op.id, op.caller);
	}

	private AsyncOp deq_async() {
		return deq_async_w_tmo();
	}

	private AsyncOp deq_async_w_tmo() {
		if (opt.size() == 0)
			return null;
		return opt.get();
	}

	private boolean send_async_error(short id, EPID caller, EObject reason) {
		/* send message:
		**      {inet_async, Port, Ref, {error,Reason}}
		*/
		
		ETuple msg = ETuple.make(am_inet_async, port(), ERT.box(id), 
				new ETuple2(ERT.am_error, reason));
		if (ERT.DEBUG_PORT) {
			System.out.println("sending to "+caller+" ! "+msg);
		}
		return caller.sendnb(msg);

	}


	private boolean send_async_ok(int id, EPID caller) {
		ETuple msg = ETuple.make(am_inet_async, port(), ERT.box(id), ERT.am_ok);
		if (ERT.DEBUG_PORT) {
			System.out.println("sending to "+caller+" ! "+msg);
		}
		return caller.sendnb(msg);
	}

	private boolean send_async_ok_port(int id, EPID caller, EPort port2) {
		ETuple msg = ETuple.make(am_inet_async, port(), ERT.box(id), 
					new ETuple2(ERT.am_ok, port2));
		if (ERT.DEBUG_PORT) {
			System.out.println("sending to "+caller+" ! "+msg);
		}
		return caller.sendnb(msg);
	}

	private short enq_async(EPID caller, ByteBuffer reply, int req) {
		return enq_async_w_tmo(caller, reply, req, INET_INFINITY, (ERef)null);
	}
	
	static AtomicInteger aid = new AtomicInteger(0);

	private short enq_async_w_tmo(EPID caller, ByteBuffer reply, int req, int timeout,
			ERef monitor) {
		
		AsyncOp op = new AsyncOp();
		op.caller = caller;
		op.req = req;
		op.id = (short)(aid.incrementAndGet() & 0xffff);
		op.timeout = timeout;
		op.monitor = monitor;
		
		if (reply != null) {
			reply.putShort(op.id);
		}
		
		opt.put(op);
		
		return op.id;
	}

	private InetSocketAddress inet_set_address(byte family, ByteBuffer cmd) {

		int port = cmd.getShort() & 0xffff;
		
		byte[] bytes;
		if (family == INET_AF_INET) {
			bytes = new byte[4];
		} else if (family == INET_AF_INET6) {
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
		
		InetSocketAddress res = new InetSocketAddress(addr , port);
		System.out.println("addr: "+res);
		return res;
	}

	private boolean is_connecting() {
		return (state & INET_F_CON) == INET_F_CON;
	}
		
	private boolean is_bound() {
		return (state & INET_F_BOUND) == INET_F_BOUND;
	}

	private boolean is_conected() {
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

	private boolean save_subscriber(List<EObject> subs, EPID pid) {

		subs.add(pid);
		
		return true;
	}

	private ByteBuffer inet_ctl_open(byte domain, SockType type) {
		if (state != INET_STATE_CLOSED) {
			return ctl_xerror(EXBADSEQ);
		}

		// since we don't know if we will be connecting or listening we 
		// need to delay that decision; for now, create a (client) socket.
		
		try {
			this.fd = SocketChannel.open();
			fd.configureBlocking(false);
			
		} catch (IOException e) {
			int code = IO.exception_to_posix_code(e);
			return ctl_error(code);
		}
		
		this.state = INET_STATE_OPEN;
		this.type = type;
		this.family = domain;
		
	    return ctl_reply(INET_REP_OK);
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
		ByteBuffer buf = ByteBuffer.allocate(1+data.length);
		buf.put((byte) code);
		buf.put(data);
		return buf;
	}

	private ByteBuffer ctl_reply(int code, String data) {
		ByteBuffer buf = ByteBuffer.allocate(1+data.length());
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
	
	

}
