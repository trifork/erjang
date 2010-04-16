package erjang.net;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectableChannel;

import erjang.driver.efile.Posix;

public class InetDatagramSocket extends InetSocket {

	private DatagramChannel ch;

	public InetDatagramSocket(LazyInetSocket sock) throws IOException {
		ch = DatagramChannel.open();
		sock.init(this);
	}

	@Override
	public InetSocket accept() throws IOException {
		throw new erjang.NotImplemented();
	}

	@Override
	public void bind(InetSocketAddress localAddress) throws IOException {
		ch.socket().bind(localAddress);
	}

	@Override
	public
	SelectableChannel channel() {
		return ch;
	}

	@Override
	public boolean connect(InetSocketAddress remote) throws IOException {
		ch.socket().connect(remote);
		return true;
	}

	@Override
	public boolean finishConnect() throws IOException {
		// should not happen
		return true;
	}


	@Override
	public InetSocketAddress getLocalSocketAddress() {
		return (InetSocketAddress) ch.socket().getLocalSocketAddress();
	}

	@Override
	public boolean isBound() {
		return ch.socket().isBound();
	}

	@Override
	public void listen(int backlog) throws IOException {
		throw new PosixIOException(Posix.EINVAL, "Cannot listen on UDP socket");
	}

	@Override
	public void setReceiveBufferSize(int size) throws IOException {
		ch.socket().setReceiveBufferSize(size);
	}

	@Override
	public void setReuseAddress(boolean on) throws IOException {
		ch.socket().setReuseAddress(on);
	}

	@Override
	public void setSendBufferSize(int size) throws IOException {
		ch.socket().setSendBufferSize(size);
	}

	@Override
	public void setTimeout(int timeout) throws IOException {
		ch.socket().setSoTimeout(timeout);
	}

	@Override
	public void setBroadcast(boolean on) throws IOException {
		ch.socket().setBroadcast(on);
	}
	
	@Override
	public void setTrafficClass(int ival) throws IOException {
		ch.socket().setTrafficClass(ival);
	}
	
	@Override
	public int send(ByteBuffer packet, SocketAddress target) throws IOException {
		return ch.send(packet, target);
	}
	
}
