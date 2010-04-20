package erjang.net;

import erjang.driver.tcp_inet.TCPINet;

public enum ProtocolFamily {
	INET(TCPINet.INET_AF_INET), 
	INET6(TCPINet.INET_AF_INET6), 
	LOOPBACK(TCPINet.INET_AF_LOOPBACK), 
	ANY(TCPINet.INET_AF_ANY);
	
	public final byte code;

	private ProtocolFamily(byte code) {
		this.code = code;
	}
}
