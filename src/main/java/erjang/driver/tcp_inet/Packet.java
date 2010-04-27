/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/

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
		}
		return 0;

	}

	public static <T> int parse_http(byte[] buf, int start, int len,
			IntCell statep, PacketCallbacks<T> pcb, T arg) {
		throw new NotImplemented();
	}

	public static <T> int parse_ssl(byte[] buf, int start, int len,
			PacketCallbacks<T> pcb, T arg) {
		throw new NotImplemented();
	}

	public static int get_length(PacketParseType htype, byte[] data,
			int offset, int n, int max_plen, int trunc_len, IntCell httpState) {

		int hlen, plen;

		switch (htype) {
		case TCP_PB_RAW:
			if (n == 0)
				return 0;
			else
				return n;

		case TCP_PB_1:
			hlen = 1;
			if (n < hlen)
				return 0;
			plen = data[offset] & 0xff;
			break;

		case TCP_PB_2:
			hlen = 2;
			if (n < hlen)
				return 0;
			
			plen = ((data[offset] & 0xff) << 8) | (data[offset+1] & 0xff) ;
			break;

		case TCP_PB_4:
			hlen = 4;
			if (n < hlen)
				return 0;
			
			plen = ((data[offset] & 0xff) << 24) 
		 	     | ((data[offset+1] & 0xff) << 16) 
		 	     | ((data[offset+2] & 0xff) << 8) 
		 	     |  (data[offset+3] & 0xff) ;
			break;

		default:
			throw new NotImplemented();
		}

		// remain case...
		int tlen = hlen + plen;
		if ((max_plen != 0 && plen > max_plen) || tlen < hlen) {
			return -1;
		}
		return tlen;

	}

}
