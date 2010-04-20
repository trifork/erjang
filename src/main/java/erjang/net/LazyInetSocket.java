package erjang.net;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.concurrent.atomic.AtomicLong;

import erjang.NotImplemented;
import erjang.driver.efile.Posix;

public class LazyInetSocket extends InetSocket {

	static final int OPEN_FLAG = 0x01;

	int status = OPEN_FLAG;
	
	static AtomicLong next_id = new AtomicLong();
	private final long id;
	private final ProtocolFamily domain;
	private final ProtocolType type;
	private final Protocol protocol;
	private InetSocket delegate;

	InetSocketAddress bindingAddress;

	// Server, Client, Datagram
	public Integer so_rcv_buf;
	public Integer so_snd_buf;
	public Integer so_timeout;
	public Boolean so_reuse_addr;

	// Client
	public Boolean so_keep_alive;
	public Boolean so_oob_inline;
	public Boolean so_nodelay;

	public Integer so_traffic_class;
	public Integer so_linger;

	// Datagram, Multicast
	private Boolean so_broadcast;
	private Boolean blocking;

	void init(InetSocket s) throws IOException {

		if (blocking != null) {
			s.configureBlocking(blocking);
		}
		
		if (so_rcv_buf != null) {
			s.setReceiveBufferSize(so_rcv_buf);
		}

		if (so_snd_buf != null) {
			s.setSendBufferSize(so_snd_buf);
		}

		if (so_timeout != null) {
			setTimeout(so_timeout);
		}

		if (so_reuse_addr != null) {
			s.setReuseAddress(so_reuse_addr);
		}

		if (so_keep_alive != null) {
			s.setKeepAlive(so_keep_alive);
		}

		if (so_oob_inline != null) {
			s.setOOBInline(so_oob_inline);
		}

		if (so_nodelay != null) {
			s.setTcpNoDelay(so_nodelay);
		}

		if (so_broadcast != null) {
			s.setBroadcast(so_broadcast);
		}

		if (so_traffic_class != null) {
			s.setTrafficClass(so_traffic_class);
		}

		if (so_linger != null) {
			if (so_linger > 0) {
				s.setLinger(true, so_linger);
			} else {
				s.setLinger(false, 0);
			}
		}

		if (bindingAddress != null) {
			s.bind(bindingAddress);
		}

	}

	LazyInetSocket(ProtocolFamily domain, ProtocolType type,
			Protocol protocol) {
		this.domain = domain;
		this.type = type;
		this.protocol = protocol;
		this.id = next_id.incrementAndGet();
	}

	public void bind(InetSocketAddress localAddress) throws IOException {

		if (delegate != null) {
			delegate.bind(localAddress);
			return;
		} else {
			this.bindingAddress = localAddress;
		}

	}

	/**
	 * try to connect
	 * 
	 * @see SocketChannel#connect(SocketAddress)
	 */
	public boolean connect(InetSocketAddress remote) throws IOException {
		if (delegate == null) {
			if (protocol == Protocol.TCP) {
				delegate = new InetClientSocket(this);
			} else if (protocol == Protocol.UDP) {
				delegate = new InetDatagramSocket(this);
			} else {
				throw new NotImplemented();
			}
		}

		if (delegate.connect(remote)) {
			return true;
		} else {
			return false;
		}
	}

	@Override
	public boolean finishConnect() throws IOException {
		if (delegate == null) {
			if (protocol == Protocol.TCP) {
				delegate = new InetClientSocket(this);
			} else if (protocol == Protocol.UDP) {
				delegate = new InetDatagramSocket(this);
			} else {
				throw new NotImplemented();
			}
		}

		return delegate.finishConnect();
	}


	/** this is the signifier, that we are a server socket */
	public void listen(int backlog) throws IOException {

		if (delegate == null) {

			switch (protocol) {
			case TCP:
				delegate = new InetServerSocket(this, new Integer(backlog));
				break;

			default:
				throw new PosixIOException(Posix.EOPNOTSUPP,
						"socket is not of type SOCK_STREAM and thus does not accept connections");
			}
		}

		delegate.listen(backlog);
	}

	/**
	 * May return null if non-blocking
	 * 
	 * @see ServerSocketChannel#accept()
	 */
	@Override
	public InetSocket accept() throws IOException {
		if (delegate == null) {

			switch (protocol) {
			case TCP:
				delegate = new InetServerSocket(this, null);
				break;

			default:
				throw new PosixIOException(Posix.EOPNOTSUPP,
						"socket is not of type SOCK_STREAM and thus does not accept connections");
			}
		}

		return delegate.accept();
	}

	@Override
	public int send(ByteBuffer packet, SocketAddress target) throws IOException {
		if (delegate == null) {
			delegate = new InetDatagramSocket(this);
		}

		return delegate.send(packet, target);
	}

	@Override
	public InetSocketAddress getLocalSocketAddress() {
		if (delegate == null)
			return bindingAddress;
		
		return delegate.getLocalSocketAddress();
	}

	@Override
	public
	SelectableChannel channel() {
		if (delegate == null) {
			return null;
		}

		return delegate.channel();
	}
	
	@Override
	public void configureBlocking(boolean block) throws IOException {
		if (delegate == null) {
			this.blocking = Boolean.valueOf(block);
			return;
		}
		
		delegate.channel().configureBlocking(block);
	}

	@Override
	public void setKeepAlive(boolean val) throws IOException {
		if (delegate == null) {
			so_keep_alive = Boolean.valueOf(val);
		} else {
			delegate.setKeepAlive(val);
		}
	}
	
	@Override
	public boolean getKeepAlive() throws IOException {
		if (delegate == null) {
			return delegate.getKeepAlive();
		} else {
			return so_keep_alive == null ? false : so_keep_alive.booleanValue();
		}
	}



	@Override
	public void setOOBInline(boolean val) throws IOException {
		if (delegate == null) {
			so_oob_inline = Boolean.valueOf(val);
		} else {
			delegate.setOOBInline(val);
		}
	}

	@Override
	public void setReuseAddress(boolean val) throws IOException {
		if (delegate == null) {
			so_reuse_addr = Boolean.valueOf(val);
		} else {
			delegate.setReuseAddress(val);
		}
	}

	@Override
	public boolean getReuseAddress() throws IOException {
		if (delegate == null) {
			return delegate.getReuseAddress();
		} else {
			return so_reuse_addr == null ? false : so_reuse_addr.booleanValue();
		}
	}


	@Override
	public void setTcpNoDelay(boolean val) throws IOException {
		if (delegate == null) {
			so_nodelay = Boolean.valueOf(val);
		} else {
			delegate.setTcpNoDelay(val);
		}
	}
	
	@Override
	public boolean getNoDelay() throws IOException {
		if (delegate == null) {
			return delegate.getNoDelay();
		} else {
			return so_nodelay == null ? false : so_nodelay.booleanValue();
		}
	}

	@Override
	public void setBroadcast(boolean val) throws IOException {
		if (delegate == null) {
			so_broadcast = Boolean.valueOf(val);
		} else {
			delegate.setBroadcast(val);
		}
	}

	@Override
	public void setTrafficClass(int ival) throws IOException {
		if (delegate == null) {
			so_traffic_class = new Integer(ival);
		} else {
			delegate.setTrafficClass(ival);
		}
	}
	
	@Override
	public void setReceiveBufferSize(int size) throws IOException {
		if (delegate == null) {
			so_rcv_buf = new Integer(size);
		} else {
			delegate.setReceiveBufferSize(size);
		}
	}
	
	@Override
	public int getReceiveBufferSize() throws IOException {
		if (delegate == null) {
			return delegate.getReceiveBufferSize();
		} else {
			return so_rcv_buf == null ? 0 : so_rcv_buf.intValue();
		}
	}

	@Override
	public void setSendBufferSize(int size) throws IOException {
		if (delegate == null) {
			so_snd_buf = new Integer(size);
		} else {
			delegate.setSendBufferSize(size);
		}
	}

	@Override
	public int getSendBufferSize() throws IOException {
		if (delegate == null) {
			return delegate.getSendBufferSize();
		} else {
			return so_snd_buf == null ? 0 : so_snd_buf.intValue();
		}
	}

	@Override
	public void setTimeout(int timeout) throws IOException {
		if (delegate == null) {
			so_timeout = new Integer(timeout);
		} else {
			delegate.setTimeout(timeout);
		}
	}

	@Override
	public void setLinger(boolean on, int timeout) throws IOException {
		if (delegate == null) {
			if (on == false) {
				so_linger = new Integer(-1);
			} else {
				so_linger = new Integer(timeout);
			}
		} else {
			delegate.setLinger(on, timeout);
		}

	}

	@Override
	public void close() throws IOException {
		if (delegate != null) {
			delegate.close();
		} else {
			status &= ~OPEN_FLAG;
		}
	}

	public boolean isBound() {
		if (delegate != null) {
			return delegate.isBound();
		}
		
		return this.bindingAddress != null;
	}

	@Override
	public boolean isOpen() {
		if (delegate != null) {
			return delegate.isOpen();
		}
		
		return (this.status & OPEN_FLAG) == OPEN_FLAG;
	}
	
	@Override
	public String toString() {
		if (delegate == null) {
			
			return "LazySocket["
				+ "bound="+this.bindingAddress
				+ "; linger="+this.so_linger
				+ "; rcvbuf="+this.so_rcv_buf
				+ "; sndbuf="+this.so_snd_buf
				+ "; timeout="+this.so_timeout
				+ "; reuse="+this.so_reuse_addr
				+ "; keepalive="+this.so_keep_alive 
				+ "]";
			
		} else {
			return delegate.toString();
		}
	}
}
