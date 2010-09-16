package erjang.epmd;

import java.io.IOException;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.GatheringByteChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;

public abstract class PacketConnection implements Connection {
	protected SelectionKey sk;
	private int state;

	private ByteBuffer recvBuffer = null;
	private String name = "";

	ArrayList<ByteBuffer> outbuf = new ArrayList<ByteBuffer>();

	protected void receivePacket(ByteBuffer buf) {
		// ignore //
	}

	protected void connectionClosed() {
		// do nothing //
	}

	/**
	 * @param buf byte buffer of packet (will be flip'ed)
	 * @param includeSize
	 */
	void sendPacket(ByteBuffer buf, boolean includeSize) {
		if (includeSize) {
			ByteBuffer size = ByteBuffer.allocate(2);
			size.putShort(0, (short) buf.position());
			outbuf.add(size);
		}
		
		buf.flip();
		outbuf.add(buf);

		sk.interestOps(SelectionKey.OP_WRITE | sk.interestOps());
	}
	
	/**
	 * construct a NIOConnection from a selection key
	 */
	PacketConnection(SelectionKey sk) {
		SocketChannel sch = (SocketChannel) sk.channel();
		if (sch.isConnected()) // connected immediatedly if local on *nix
		{
			sk.interestOps(SelectionKey.OP_READ);
			state = Connection.OPEN;
		} else if (sch.isConnectionPending()) {
			sk.interestOps(SelectionKey.OP_CONNECT);
			state = Connection.OPENING;
		}
		this.sk = sk; // link this to the key
		sk.attach(this);

		recvBuffer = ByteBuffer.allocate(8196);
	}

	/**
	 * process a connect complete selection
	 */
	public void doConnect() {
		SocketChannel sc = (SocketChannel) sk.channel();
		try {
			sc.finishConnect();
			sk.interestOps(sk.interestOps() & ~SelectionKey.OP_CONNECT); 
			Functions.dout(2, "connect complete");
			state = Connection.OPEN;
		} catch (IOException e) {
			e.printStackTrace();
			closeComplete();
		}
	}

	ByteBuffer rcvBuf;
	protected boolean keep;

	/**
	 * process a read ready selection
	 */
	public void doRead() {
		SocketChannel sc = (SocketChannel) sk.channel();
		if (sc.isOpen()) {

			int len;
			recvBuffer.clear();
			try {
				len = sc.read(recvBuffer);
			} catch (IOException e) {
				//e.printStackTrace();
				len = -1;
			} // error look like eof
			Functions.dout(1, "read len=" + len);

			if (len == -1) {
				close();
				return;
			}

			int start = 0;

			while (recvBuffer.position() > start + 2) {

				int pklen = recvBuffer.getShort(start) & 0xffff;
				if (recvBuffer.position() >= start + 2 + pklen) {
					// got a complete package!

					ByteBuffer buf = slice(recvBuffer, start + 2, pklen);
					receivePacket(buf);
					
					if (!this.keep && outbuf.isEmpty()) {
						close();
						return;
					}
					
					start += 2 + pklen;

				} else if (recvBuffer.position() == recvBuffer.limit()) {
					// receive buffer is full; reallocate it!

					int available = recvBuffer.position() - start;

					byte[] data = new byte[pklen + 2];
					recvBuffer.position(start);
					recvBuffer.get(data, 0, available);
					recvBuffer = ByteBuffer.wrap(data);

					return;
				}
			}

			// move contents of recvBuffer from
			// start ... position
			// down to
			// 0 .. (position - start)

			if (start > 0 && recvBuffer.position() > 0) {
				if (recvBuffer.position() == start) {
					recvBuffer.clear();
				} else {
					int left = recvBuffer.position() - start;
					byte[] data = new byte[left];
					recvBuffer.position(start);
					recvBuffer.get(data);
					recvBuffer.clear();
					recvBuffer.put(data);
				}
			}

		} else {
			close();
		}
	}

	private ByteBuffer slice(ByteBuffer buf, int start, int len) {
		int savePos = buf.position();
		int saveLim = buf.limit();

		buf.position(start);
		buf.limit(start + len);

		ByteBuffer res = buf.slice();

		buf.position(savePos);
		buf.limit(saveLim);

		return res;
	}

	/**
	 * process a write ready selection
	 */
	public void doWrite() {
		Functions.dout(12, "write ready");

		if (!outbuf.isEmpty()) {
			GatheringByteChannel bc = (GatheringByteChannel) sk.channel();
			ByteBuffer[] out = outbuf.toArray(new ByteBuffer[outbuf.size()]);

			// do the vector write
			try {
				bc.write(out);
			} catch (IOException e) {
				e.printStackTrace();
			}

			for (int i = 0; i < out.length && !out[i].hasRemaining(); i++) {
				outbuf.remove(0);
			}
		}

		if (outbuf.isEmpty()) {
			sk.interestOps(sk.interestOps() & ~SelectionKey.OP_WRITE);
		}
		
		if (!this.keep && outbuf.isEmpty()) {
			close();
		}

	}

	/*
	 * close the connection and its socket
	 */
	public void close() {
		if (state != Connection.CLOSED) {
			SocketChannel sc = (SocketChannel) sk.channel();
			if (sc.isOpen()) {
				if (state == Connection.OPEN) // open attempt graceful
				// shutdown
				{
					Functions.dout(2, "shutting down");
					state = Connection.CLOSING;
					Socket sock = sc.socket();
					try {
						sock.shutdownOutput();
					} catch (IOException se) {
						Functions.dout(12, "shutdown failed");
						se.printStackTrace();
					}
				} else
					closeComplete();
			} else
				Functions.dout(12, "already closed");
		}
	}

	// called internally if already closing or closed by partner
	private void closeComplete() {
		Functions.dout(2, "closing channel");
		try {
			sk.interestOps(0);
			SocketChannel sc = (SocketChannel) sk.channel();
			if (sc != null && sc.isOpen())
				sc.close();
			sk.selector().wakeup();
			sk.attach(null);
		} catch (IOException ce) {
			Functions.fail(ce, "close failed");
		}
		state = Connection.CLOSED;
		connectionClosed();
	}

	public String getName() {
		return name;
	}

	public void setName(String nm) {
		name = nm;
	}

	public int getState() {
		return state;
	}
}