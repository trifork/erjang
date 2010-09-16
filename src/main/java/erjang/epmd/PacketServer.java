package erjang.epmd;

import java.io.*;
import java.net.*;
import java.util.*;
import java.nio.*;
import java.nio.channels.*;

/**
 * a simple message switch using NIO based socket i/o part of the pkwnet package
 * a very simple text message switching program default command line is java
 * StreamSwitch -p5050 -i600 -p port to listen on -i default idle time in
 * seconds user commands start with $ and consist of blank seperated arguments
 * other lines sent by the user are forwarded $on nickname targets sign on as
 * nickname, sending to targets $to targets change target list, reports current
 * value $list nicknames list status of specified nicknames $list list all
 * connected users $off sign off
 * 
 * @author PKWooster
 * @version 1.0 June 14,2004
 */
public abstract class PacketServer {
	static int debugLevel = 2;
	private ServerSocket ss; // the listening socket
	private ServerSocketChannel sschan; // the listening channel
	private Selector selector; // the only selector
	private int bufsz = 8192;

	protected void listen(int port) {
		int n = 0;
		Iterator<SelectionKey> it;
		SelectionKey key;

		Functions.dout(1, "listening on port=" + port);
		try {
			sschan = ServerSocketChannel.open();
			sschan.configureBlocking(false);
			ss = sschan.socket();
			ss.bind(new InetSocketAddress(InetAddress.getByAddress(new byte[] {127,0,0,1}), port));
			selector = Selector.open();
			sschan.register(selector, SelectionKey.OP_ACCEPT);
		} catch (IOException ie) {
			ie.printStackTrace();
			System.exit(0);
		}

		while (true) {
			// now we select any pending io
			try {
				n = selector.select();
			} // select
			catch (Exception e) {
				Functions.fail(e, "select failed");
			}
			Functions.dout(0, "select n=" + n);

			// process any selected keys
			Set<SelectionKey> selectedKeys = selector.selectedKeys();
			it = selectedKeys.iterator();
			while (it.hasNext()) {
				key = (SelectionKey) it.next();
				int kro = key.readyOps();
				Functions.dout(0, "kro=" + kro);
				if ((kro & SelectionKey.OP_READ) == SelectionKey.OP_READ)
					doRead(key);
				if ((kro & SelectionKey.OP_WRITE) == SelectionKey.OP_WRITE)
					doWrite(key);
				if ((kro & SelectionKey.OP_ACCEPT) == SelectionKey.OP_ACCEPT)
					doAccept(key);
				if ((kro & SelectionKey.OP_CONNECT) == SelectionKey.OP_CONNECT)
					doConnect(key);
				if (key.isValid() && key.interestOps() == 0) {
					it.remove(); // remove the key
				}
			}
		}
	}

	private void doAccept(SelectionKey sk) {
		ServerSocketChannel sc = (ServerSocketChannel) sk.channel();
		Functions.dout(2, "accept");
		SocketChannel usc = null;
		ByteBuffer data;
		try {
			usc = sc.accept();
			if (usc == null) return;
			usc.configureBlocking(false);
			Socket sock = usc.socket();
			String nm = sock.getInetAddress() + ":" + sock.getPort();
			System.out.println("connection from " + nm);
			sock.setKeepAlive(true);
			data = ByteBuffer.allocate(bufsz);
			data.position(data.limit()); // looks like write complete
			SelectionKey dsk = usc.register(selector, SelectionKey.OP_READ, null);
			Connection conn = newConnection(dsk); // contains socket i/o code
			conn.setName(nm);
			dsk.attach(conn); // link it to the key so we can find it
		} catch (IOException re) {
			Functions.fail(re, "registration error");
		}
	}

	protected abstract PacketConnection newConnection(SelectionKey dsk);
	
	private void doRead(SelectionKey sk) {
		PacketConnection conn = (PacketConnection) sk.attachment(); // get our
		// connection
		conn.doRead();
	}

	private void doWrite(SelectionKey sk) {
		PacketConnection conn = (PacketConnection) sk.attachment(); // get our
		// connection
		conn.doWrite();
	}

	private void doConnect(SelectionKey sk) {
		PacketConnection conn = (PacketConnection) sk.attachment(); // get our
		// connection
		conn.doConnect();
	}

}
