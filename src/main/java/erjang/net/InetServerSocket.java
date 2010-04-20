package erjang.net;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.channels.SelectableChannel;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;

import erjang.driver.efile.Posix;

public class InetServerSocket extends InetSocket {

	ServerSocketChannel ch;
	private Integer backlog;
	
	public InetServerSocket(LazyInetSocket sock, Integer backlog) throws IOException {
		ch = ServerSocketChannel.open();
		this.backlog = backlog;
		sock.init(this);
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
	public InetSocket accept() throws IOException {
		
		SocketChannel sock = ch.accept();
		if (sock == null) {
			return null;
		}
		
		return new InetClientSocket(sock);
	}
	
	@Override
	public void setReuseAddress(boolean reuse) throws IOException {
		ch.socket().setReuseAddress(reuse);
	}

	@Override
	public void setOOBInline(boolean on) throws IOException {
		// ignore
	}
	
	@Override
	public void setTcpNoDelay(boolean on) throws IOException {
		// ignore
	}
	
	@Override
	public void setLinger(boolean on, int timeout) throws IOException {
		// ignore
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
		// ignore //
	}

	public boolean getReuseAddress() throws IOException {
		return ch.socket().getReuseAddress();
	}

	@Override
	public int getLocalPort() {
		return ch.socket().getLocalPort();
	}
	
	@Override
	public void bind(InetSocketAddress addr) throws IOException {
		if (backlog == null) {
			ch.socket().bind(addr);
		} else {
			ch.socket().bind(addr, backlog);
		}
	}

	@Override
	public boolean connect(InetSocketAddress remote) throws IOException {
		throw new erjang.NotImplemented();
	}

	@Override
	public InetSocketAddress getLocalSocketAddress() {
		return (InetSocketAddress) ch.socket().getLocalSocketAddress();
	}

	@Override
	public void listen(int backlog) throws IOException {
		this.backlog = new Integer(backlog);
	}

	@Override
	public void setKeepAlive(boolean on) throws IOException {
		// ignored //
	}
	
	public void setNonBlocking() throws IOException {
		ch.configureBlocking(false);
	}

	@Override
	public String toString() {		
		return "InetServerSocket[" + ch.toString()  
			+ " open="+ch.isOpen()
			+ " blocking="+ch.isBlocking()
			+ " addr="+ch.socket().getLocalSocketAddress()
		    + "]";		
	}

}
