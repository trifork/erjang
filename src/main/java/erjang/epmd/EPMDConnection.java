package erjang.epmd;

import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.util.HashMap;
import java.util.Map;

import erjang.driver.IO;

public class EPMDConnection extends PacketConnection {

	private NodeInfo connectedNode;

	private int epmdPort;

	static final byte EPMD_ALIVE_REQ = 'a';
	static final byte EPMD_ALIVE_OK_RESP = 'Y';

	static final byte EPMD_PORT_REQ = 'p';
	static final byte EPMD_NAMES_REQ = 'n';
	static final byte EPMD_DUMP_REQ = 'd';
	static final byte EPMD_KILL_REQ = 'k';
	static final byte EPMD_STOP_REQ = 's';

	static final int ALIVE2_REQ = 'x';
	static final int ALIVE2_RESP = 'y';
	
	static final int PORT2_REQ = 'z';
	static final byte PORT2_RESP = 'w';
		

	public EPMDConnection(int epmdPort, SelectionKey dsk) {
		super(dsk);
		this.epmdPort = epmdPort;
	}

	@Override
	protected void receivePacket(ByteBuffer buf) {
		
		int op = buf.get();
		
		Functions.dout(1, "processing op "+op+" from "+getName());
		
		switch (op) {
		case ALIVE2_REQ: 
		{
			int eport = buf.getShort() & 0xffff;
			int nodetype = buf.get();
			int proto = buf.get();
			int highvsn = buf.getShort() & 0xffff;
			int lowvsn = buf.getShort() & 0xffff;
			int nlen = buf.getShort() & 0xffff;
			byte[] name = new byte[nlen];
			buf.get(name);
			int elen = buf.getShort() & 0xffff;
			byte[] extra = new byte[elen];
			buf.get(extra);
			
			NodeInfo node;
			if ((node = node_reg2(name, eport, nodetype, proto, highvsn, lowvsn, extra)) == null) {
				ByteBuffer resp = ByteBuffer.allocate(4);
				resp.put((byte) ALIVE2_RESP);
				resp.put((byte) 1); // error
				resp.putShort((short) 99);
				this.sendPacket(resp, false);
			} else {
				ByteBuffer resp = ByteBuffer.allocate(4);
				resp.put((byte) ALIVE2_RESP);
				resp.put((byte) 0);
				resp.putShort(node.creation);
				this.keep = true;
				this.sendPacket(resp, false);
			}
			return;
		}
		
		case PORT2_REQ: {
			int len = buf.remaining();
			byte[] data = new byte[len];
			buf.get(data);
			String n = new String(data, IO.ISO_LATIN_1);
			
			NodeInfo node = nodes.get(n);
			if (n == null) {
				ByteBuffer resp = ByteBuffer.allocate(2);
				resp.put((byte) PORT2_RESP);
				resp.put((byte) 1);
				sendPacket(resp, false);
				return;
			} else {
				ByteBuffer resp = ByteBuffer.allocate(20 + node.extra.length + node.nodeName.length);
				resp.put(PORT2_RESP);
				resp.put((byte) 0);
				resp.putShort((short) node.portNo);
				resp.put((byte) node.nodeType);
				resp.put((byte) node.protocol);
				resp.putShort((short) node.highVersion);
				resp.putShort((short) node.lowVersion);
				resp.putShort((short) node.nodeName.length);
				resp.put(node.nodeName);
				resp.putShort((short) node.extra.length);
				resp.put(node.extra);
				
				sendPacket(resp, false);
			}
			
			return;
		}
		
		case EPMD_NAMES_REQ: {
			StringBuffer sb = new StringBuffer();
			for (NodeInfo n : nodes.values()) {
				sb.append("name ").append(n.name).append(" at port ").append(n.portNo).append('\n');
			}
			
			ByteBuffer resp = ByteBuffer.allocate(4 + sb.length());
			resp.putInt ( epmdPort );
			resp.put(sb.toString().getBytes(IO.ISO_LATIN_1));
			sendPacket(resp, false);
			return;
		}
		
		case EPMD_DUMP_REQ: {
			StringBuffer sb = new StringBuffer();
			for (NodeInfo n : nodes.values()) {
				sb.append("active name     <").append(n.name).append("> at port ")
					.append(n.portNo).append(", fd = ")
					.append(n.fd)
					.append('\n');
			}
			
			for (NodeInfo n : nodes.values()) {
				sb.append("old/unused name <").append(n.name).append("> at port ")
					.append(n.portNo).append(", fd = ")
					.append(n.fd)
					.append('\n');
			}
			
			ByteBuffer resp = ByteBuffer.allocate(4 + sb.length());
			resp.putInt ( epmdPort );
			resp.put(sb.toString().getBytes(IO.ISO_LATIN_1));
			sendPacket(resp, false);
			return;
		}
		
		case EPMD_KILL_REQ: {
			ByteBuffer resp = ByteBuffer.allocate(7);
			resp.put("NOEXIST".getBytes(IO.ISO_LATIN_1));
			sendPacket(resp, false);
			return;
		}
			
		default:
			Functions.dout(0, "Unknown message: "+op);
			System.exit(1);
		}

	}
	
	static Map<String,NodeInfo> unreg = new HashMap<String,NodeInfo>();
	static Map<String,NodeInfo> nodes = new HashMap<String,NodeInfo>();
	
	private NodeInfo node_reg2(byte[] name, int eport, int nodetype, int proto,
			int highvsn, int lowvsn, byte[] extra) {

		String n = new String(name, IO.ISO_LATIN_1);
		
		// fail if already registered
		if (nodes.containsKey(n)) {
			return null;
		}
		
		NodeInfo node = unreg.remove(n);
		if (node == null) {
			node = new NodeInfo();
			node.creation = (short) ((System.currentTimeMillis() % 3) + 1);
		} else {
			node.creation = (short) ((node.creation % 3) + 1);
		}
		
		node.name = n;
		node.portNo = eport;
		node.nodeType = nodetype;
		node.protocol = proto;
		node.highVersion = highvsn;
		node.lowVersion = lowvsn;
		node.extra = extra;
//		node.connection = this;
		node.fd = 0;
		
		nodes.put(n, node);
		this.connectedNode = node;
		
		return node;
	}

	@Override
	protected void connectionClosed() {
		NodeInfo node = this.connectedNode;
		this.connectedNode = null;
		
		if (node != null) {
			nodes.remove(node.name);
			unreg.put(node.name, node);
		}
	}

}
