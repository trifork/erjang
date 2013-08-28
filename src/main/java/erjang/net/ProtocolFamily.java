package erjang.net;

import erjang.driver.tcp_inet.TCPINet;

public enum ProtocolFamily {
	INET(TCPINet.INET_AF_INET), 
	INET6(TCPINet.INET_AF_INET6), 
	ANY(TCPINet.INET_AF_ANY),
	LOOPBACK(TCPINet.INET_AF_LOOPBACK);
	
	public final byte code;

	  public static ProtocolFamily fromOrdinal(int ordinal) {
		    switch(ordinal) {
		    case TCPINet.INET_AF_INET: return INET;
		    case TCPINet.INET_AF_INET6: return INET6;
		    case TCPINet.INET_AF_ANY: return ANY;
		    case TCPINet.INET_AF_LOOPBACK: return LOOPBACK;
		    default:
		    	throw new IllegalArgumentException();
		    }
		  }

	private ProtocolFamily(byte code) {
		this.code = code;
	}
}
