package erjang.driver.tcp_inet;

public abstract class PacketCallbacks<T> {
	abstract void http_error(T src, byte[] data, int pos, int len);
}
