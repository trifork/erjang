package erjang.driver.tcp_inet;

import java.nio.ByteBuffer;

import erjang.NotImplemented;

public class Packet {

	public static void get_body(PacketParseType htype, ByteBuffer out) {

		switch (htype) {
		case TCP_PB_1:
			out.position(out.position() + 1);
			break;
		case TCP_PB_2:
			out.position(out.position() + 2);
			break;
		case TCP_PB_4:
			out.position(out.position() + 4);
			break;
		case TCP_PB_FCGI:
			out.limit(out.limit() - (0xff & out.get(6)));
			break;
		default:
			;/* Return other packets "as is" */
		}

	}

	public static <T> int parse(PacketParseType htype, byte[] buf, int start,
			int len, IntCell statep, PacketCallbacks<T> pcb, T arg) {

		switch (htype) {
		case TCP_PB_HTTP:
		case TCP_PB_HTTPH:
		case TCP_PB_HTTP_BIN:
		case TCP_PB_HTTPH_BIN:
			if (parse_http(buf, start, len, statep, pcb, arg) < 0)
				pcb.http_error(arg, buf, start, len);
			return 1;
		case TCP_PB_SSL_TLS:
			return parse_ssl(buf, start, len, pcb, arg);
		default:
			;
		}
		return 0;

	}
	
	public static <T> int parse_http(byte[] buf, int start,
			int len, IntCell statep, PacketCallbacks<T> pcb, T arg) {
		throw new NotImplemented();
	}

	public static <T> int parse_ssl(byte[] buf, int start,
			int len, PacketCallbacks<T> pcb, T arg) {
		throw new NotImplemented();
	}


}
