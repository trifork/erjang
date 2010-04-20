package erjang.net;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SocketChannel;

public class InetClientSocket extends InetSocket {

	private SocketChannel ch;

	public InetClientSocket(LazyInetSocket sock) throws IOException {
		ch = SocketChannel.open();		
		sock.init(this);
	}

	public InetClientSocket(SocketChannel sock) {
		this.ch = sock;
	}

	@Override
	public
	SelectableChannel channel() {
		return ch;
	}

	@Override
	public boolean isBound() {
		return ch.socket().isBound();
	}
	
	@Override
	public void bind(InetSocketAddress addr) throws IOException {
		ch.socket().bind(addr);
	}
	
	@Override
	public void setKeepAlive(boolean on) throws SocketException {
		ch.socket().setKeepAlive(on);
	}
	
	@Override
	public boolean getKeepAlive() throws IOException {
		return ch.socket().getKeepAlive();
	}
	
	@Override
	public void setReuseAddress(boolean reuse) throws IOException {
		ch.socket().setReuseAddress(reuse);
	}

	@Override
	public boolean getReuseAddress() throws IOException {
		return ch.socket().getReuseAddress();
	}
	
	@Override
	public void setOOBInline(boolean on) throws IOException {
		ch.socket().setOOBInline(on);
	}
	
	@Override
	public void setTcpNoDelay(boolean on) throws IOException {
		ch.socket().setTcpNoDelay(on);
	}
	
	@Override
	public void setReceiveBufferSize(int size) throws IOException {
		ch.socket().setReceiveBufferSize(size);
	}
	
	@Override
	public int getReceiveBufferSize() throws IOException {
		return ch.socket().getReceiveBufferSize();
	}
	
	@Override
	public void setTimeout(int timeout) throws IOException {
		ch.socket().setSoTimeout(timeout);
	}
	
	@Override
	public void setSendBufferSize(int size) throws IOException {
		ch.socket().setSendBufferSize(size);
	}
	
	@Override
	public int getSendBufferSize() throws IOException {
		return ch.socket().getSendBufferSize();
	}
	
	@Override
	public void setLinger(boolean on, int timeout) throws IOException {
		ch.socket().setSoLinger(on, timeout);
	}
	
	

	@Override
	public InetSocket accept() throws IOException {
		throw new erjang.NotImplemented();
	}

	@Override
	public boolean connect(InetSocketAddress remote) throws IOException {
		return ch.connect(remote);
	}

	@Override
	public boolean finishConnect() throws IOException {
		return ch.finishConnect();
	}

	@Override
	public InetSocketAddress getLocalSocketAddress() {
		return (InetSocketAddress) ch.socket().getLocalSocketAddress();
	}

	@Override
	public void listen(int backlog) throws IOException {
		throw new erjang.NotImplemented();		
	}

	@Override
	public void close() throws IOException {
		ch.close();
	}
	
	public void setTrafficClass(int ival) throws IOException {
		ch.socket().setTrafficClass(ival);
	}

	public void setNonBlocking() throws IOException {
		ch.configureBlocking(false);
	}
	
	@Override
	public InetSocketAddress getRemoteAddress() {
		return (InetSocketAddress) ch.socket().getRemoteSocketAddress();
	}
	
	@Override
	public String toString() {
		
		return "InetClientSocket[" + ch.toString()  
			+ " open="+ch.isOpen()
			+ " addr="+ch.socket().getLocalSocketAddress()
			+ " remote="+ch.socket().getRemoteSocketAddress()
		    + "]";		
	}
}
