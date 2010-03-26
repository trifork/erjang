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
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;

import erjang.EObject;
import erjang.ERef;
import erjang.EString;
import erjang.driver.EAsync;
import erjang.driver.EDriverInstance;
import erjang.driver.efile.Posix;

public class TCPINet extends EDriverInstance {

	static enum SockType {
		SOCK_STREAM, SOCK_DGRAM, SOCK_SEQPACKET;
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

	private final EString command;

	public TCPINet(EString command) {
		this.command = command;
	}

	@Override
	protected EObject call(int command, EObject data) {
		throw new erjang.NotImplemented();

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
	protected void timeout() {
		throw new erjang.NotImplemented();

	}
	
	@Override
	protected ByteBuffer control(int command, ByteBuffer[] out) {
		
		switch (command) {
		case INET_REQ_OPEN:
			if (out.length == 1 && out[0].remaining()==1) {
				byte family = out[0].get();
				
				if (family == INET_AF_INET || family == INET_AF_INET6) {
					return inet_ctl_open(family, SockType.SOCK_STREAM);
				}
			}
			
			return ctl_error(Posix.EINVAL);

		}
		
		throw new erjang.NotImplemented("tcp_inet control(cmd="+command+")");
		
	}

	private ByteBuffer inet_ctl_open(byte family, SockType sockStream) {
		throw new erjang.NotImplemented();
		
	}

	private ByteBuffer ctl_error(int einval) {
		throw new erjang.NotImplemented();
		
	}

}
