package erjang.net;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.NetworkInterface;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;

import erjang.driver.efile.Posix;

public abstract class InetSocket {

	public static InetSocket open(ProtocolFamily domain, ProtocolType type, Protocol protocol) {
		return new LazyInetSocket(domain, type, protocol);
	}

	public abstract SelectableChannel channel();

	public abstract boolean connect(InetSocketAddress remote) throws IOException;

	public abstract void bind(InetSocketAddress localAddress)
			throws IOException;

	public abstract boolean isBound();

	public abstract void listen(int backlog) throws IOException;

	public abstract InetSocket accept() throws IOException;

	public abstract InetSocketAddress getLocalSocketAddress();

	public abstract void setReuseAddress(boolean reuse) throws IOException;

	public abstract void setReceiveBufferSize(int size) throws IOException;

	public abstract void setTimeout(int timeout) throws IOException;

	public abstract void setSendBufferSize(int size) throws IOException;

	public void setKeepAlive(boolean on) throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"option SO_KEEPALIVE not supported on "
						+ this.getClass().getName());
	}


	public  void setOOBInline(boolean on) throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"option SO_OOB_INLINE not supported on "
						+ this.getClass().getName());
	}

	public void setTcpNoDelay(boolean on) throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"option SO_TCP_NODELAY not supported on "
						+ this.getClass().getName());
	}

	public void setLinger(boolean on, int timeout) throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"option SO_LINGER not supported on "
						+ this.getClass().getName());
	}

	public void setBroadcast(boolean on) throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"option SO_BROADCAST not supported on "
						+ this.getClass().getName());
	}

	public int send(ByteBuffer packet, SocketAddress target)
			throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"send(datagram) not supported on "
						+ this.getClass().getName());
	}

	public void close() throws IOException {
		channel().close();
	}

	public void configureBlocking(boolean block) throws IOException {
		channel().configureBlocking(block);
	}
	
	public boolean isOpen() {
		return channel().isOpen();
	}

	public boolean finishConnect() throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"finishConnect() not supported on "
						+ this.getClass().getName());
	}

	public int getLocalPort() {
		InetSocketAddress addr = (InetSocketAddress) getLocalSocketAddress();
		if (addr == null) { return 0; }
		return addr.getPort();
	}

	public void setTrafficClass(int ival) throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"setTrafficClass() not supported on "
						+ this.getClass().getName());
	}

	public void setTimeToLive(int ival) throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"setTimeToLive() not supported on "
						+ this.getClass().getName());
	}

	public void setLoopbackMode(boolean on) throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"setLoopbackMode() not supported on "
						+ this.getClass().getName());
	}

	public void setInterface(InetAddress addr) throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"setInterface() not supported on "
						+ this.getClass().getName());
	}

	public void joinGroup(SocketAddress mcastaddr, NetworkInterface netIf) throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"joinGroup() not supported on "
						+ this.getClass().getName());
	}

	public void leaveGroup(SocketAddress mcastaddr, NetworkInterface netIf) throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"leaveGroup() not supported on "
						+ this.getClass().getName());
	}

	public void setNonBlocking() throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"setNonBlocking() not supported on "
						+ this.getClass().getName());
	}

	public boolean getNoDelay() throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"getNoDelay() not supported on "
						+ this.getClass().getName());
	}

	public boolean getKeepAlive() throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"getKeepAlive() not supported on "
						+ this.getClass().getName());
	}

	public boolean getReuseAddress() throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"getReuseAddress() not supported on "
						+ this.getClass().getName());
	}

	public int getSendBufferSize() throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"getSendBufferSize() not supported on "
						+ this.getClass().getName());
	}

	public int getReceiveBufferSize() throws IOException {
		throw new PosixIOException(Posix.EINVAL,
				"getReceiveBufferSize() not supported on "
						+ this.getClass().getName());
	}

	public InetSocketAddress getRemoteAddress() {
		throw new erjang.NotImplemented();
		
	}


}
