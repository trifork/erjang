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

import kilim.Pausable;

import erjang.EAtom;
import erjang.NotImplemented;
import erjang.driver.IO;
import erjang.driver.tcp_inet.PacketHttpURI.URIType;

public class Packet {

	private static final int HTTP_HDR_HASH_SIZE = 53;
	private static final int HTTP_METH_HASH_SIZE = 13;
	private static final int HTTP_MAX_NAME_LEN = 20;

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

	public static <T> int parse(PacketParseType htype, byte[] data, int buf,
			int len, IntCell statep, PacketCallbacks<T> pcb, T arg) throws Pausable {

		switch (htype) {
		case TCP_PB_HTTP:
		case TCP_PB_HTTPH:
		case TCP_PB_HTTP_BIN:
		case TCP_PB_HTTPH_BIN:
			if (parse_http(data, buf, len, statep, pcb, arg) < 0)
				pcb.http_error(arg, data, buf, len);
			return 1;
		case TCP_PB_SSL_TLS:
			return parse_ssl(data, buf, len, pcb, arg);
		default:
		}
		return 0;

	}

	public static <T> int parse_http(byte[] data, int buf, int len,
			IntCell statep, PacketCallbacks<T> pcb, T arg) throws Pausable {

		int ptr = buf;
		int p0;
		int n = len;

		/* remove trailing CRNL (accept NL as well) */
		if ((n >= 2) && (data[buf + n - 2] == '\r'))
			n -= 2;
		else if ((n >= 1) && (data[buf + n - 1] == '\n'))
			n -= 1;

		if (statep.get() == 0) {
			/* start-line = Request-Line | Status-Line */

			if (n >= 5 && (strncmp(data, buf, "HTTP/", 5) == 0)) {
				int major = 0;
				int minor = 0;
				int status = 0;
				/*
				 * Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase
				 * CRNL HTTP-Version = "HTTP" "/" 1*DIGIT "." 1*DIGIT
				 */
				ptr += 5;
				n -= 5;
				p0 = ptr;
				while (n != 0 && isdigit(data, ptr)) {
					major = 10 * major + (data[ptr] - '0');
					ptr++;
					n--;
				}
				if (ptr == p0 || n == 0 || (data[ptr] != '.'))
					return -1;
				ptr++;
				n--;
				p0 = ptr;
				while (n != 0 && isdigit((int) data[ptr])) {
					minor = 10 * minor + (data[ptr] - '0');
					ptr++;
					n--;
				}
				if (ptr == p0)
					return -1;
				p0 = ptr;
				while (n != 0 && SP(data, ptr)) {
					ptr++;
					n--;
				}
				if (ptr == p0)
					return -1;

				while (n != 0 && isdigit((int) data[ptr])) {
					status = 10 * status + (data[ptr] - '0');
					ptr++;
					n--;
				}
				p0 = ptr;
				while (n != 0 && SP(data, ptr)) {
					ptr++;
					n--;
				}
				if (ptr == p0)
					return -1;

				/* NOTE: the syntax allows empty reason phrases */
				statep.set(~0);

				return pcb.http_response(arg, major, minor, status, data, ptr,
						n);
			} else {
				/* Request-Line = Method SP Request-URI SP HTTP-Version CRLF */
				HTTPAtom meth;
				int meth_ptr = buf;
				int meth_len;
				PacketHttpURI uri;
				int uri_ptr;
				int uri_len;
				int major = 0;
				int minor = 0;
				int h = 0;

				while (n != 0 && !is_tspecial(data[ptr])) {
					h = hash_update(h, data[ptr]);
					ptr++;
					n--;
				}
				meth_len = ptr - meth_ptr;
				if (n == 0 || meth_len == 0 || !SP(data, ptr))
					return -1;

				meth = http_hash_lookup(data, meth_ptr, meth_len, h,
						http_meth_hash);

				while (n != 0 && SP(data, ptr)) {
					ptr++;
					n--;
				}
				uri_ptr = ptr;
				while (n != 0 && !SP(data, ptr)) {
					ptr++;
					n--;
				}
				if ((uri_len = (ptr - uri_ptr)) == 0)
					return -1;
				while (n != 0 && SP(data, ptr)) {
					ptr++;
					n--;
				}
				if (n == 0) {
					statep.set(~0);
					uri = http_parse_uri(data, uri_ptr, uri_len);
					return pcb.http_request(arg, meth, data, meth_ptr,
							meth_len, uri, 0, 9);
				}
				if (n < 8)
					return -1;
				if (strncmp(data, ptr, "HTTP/", 5) != 0)
					return -1;
				ptr += 5;
				n -= 5;

				p0 = ptr;
				while (n != 0 && isdigit((int) data[ptr])) {
					major = 10 * major + (data[ptr] - '0');
					ptr++;
					n--;
				}
				if (ptr == p0 || n == 0 || (data[ptr] != '.'))
					return -1;
				ptr++;
				n--;
				p0 = ptr;
				while (n != 0 && isdigit((int) data[ptr])) {
					minor = 10 * minor + (data[ptr] - '0');
					ptr++;
					n--;
				}
				if (ptr == p0)
					return -1;

				statep.set(~0);
				uri = http_parse_uri(data, uri_ptr, uri_len);
				return pcb.http_request(arg, meth, data, meth_ptr, meth_len,
						uri, major, minor);
			}
		} else {
			int up = 1; /* make next char uppercase */
			HTTPAtom name;
			byte[] name_buf = new byte[HTTP_MAX_NAME_LEN];
			int name_ptr = 0;
			int name_len;
			int h;

			if (n == 0) {
				/* end of headers */
				statep.set(0); /* reset state (for next request) */
				return pcb.http_eoh(arg);
			}
			h = 0;
			name_len = 0;
			while (!is_tspecial(data[ptr])) {
				if (name_len < HTTP_MAX_NAME_LEN) {
					byte c = data[ptr];
					if (up != 0) {
						if (islower(c)) {
							c = toupper(c);
						}
						up = 0;
					} else {
						if (isupper(c))
							c = tolower(c);
						else if (c == '-')
							up = 1;
					}
					name_buf[name_len] = c;
					h = hash_update(h, c);
				}
				name_len++;
				ptr++;
				if (--n == 0)
					return -1;
			}
			while (n != 0 && SP(data, ptr)) { /* Skip white space before ':' */
				ptr++;
				n--;
			}
			if (data[ptr] != ':') {
				return -1;
			}
			if (name_len <= HTTP_MAX_NAME_LEN) {
				name = http_hash_lookup(name_buf, 0, name_len, h, http_hdr_hash);
			} else {
				/* Is it ok to return original name without case adjustments? */
				name_ptr = buf;
				name = null;
			}
			ptr++;
			n--;
			/* Skip white space after ':' */
			while (n != 0 && SP(data, ptr)) {
				ptr++;
				n--;
			}
			return pcb.http_header(arg, name, name_buf, name_ptr, name_len,
					data, ptr, n);
		}

		// unreachable:
		// return -1;

	}

	private static byte tolower(byte c) {
		return (byte) Character.toLowerCase(c);
	}

	private static boolean isupper(byte c) {
		return Character.isUpperCase(c);
	}

	private static byte toupper(byte c) {
		return (byte) Character.toUpperCase(c);
	}

	private static boolean islower(byte c) {
		return Character.isLowerCase(c);
	}

	private static HTTPAtom http_hash_lookup(byte[] data, int ptr, int len,
			int h, HTTPAtom[] hash) {

		int ix = Math.abs(h) % hash.length;
		for (HTTPAtom entry = hash[ix]; entry != null; entry = entry.next) {

			if (h == entry.h && len == entry.len
					&& strncmp(data, ptr, entry.name, 0, len) == 0) {
				return entry;
			}
		}

		return null;
	}

	/** return 0 on success 
	 * @param string_ptr TODO*/
	private static int strncmp(byte[] data, int ptr, byte[] string, int string_ptr, int i) {
		for (int p = 0; p < i; p++) {
			int val = string[string_ptr+p] - data[ptr+p];
			if (val != 0)
				return val;
		}

		return 0;
	}

	/** return 0 on success */
	private static int strncmp(byte[] data, int ptr, String string, int i) {
		for (int p = 0; p < i; p++) {
			int val = string.charAt(p) - data[ptr+p];
			if (val != 0)
				return val;
		}

		return 0;
	}

	/*
	 * * Handle URI syntax:** Request-URI = "*" | absoluteURI | abs_path*
	 * absoluteURI = scheme ":" *( uchar | reserved )* net_path = "//" net_loc [
	 * abs_path ]* abs_path = "/" rel_path* rel_path = [ path ] [ ";" params ] [
	 * "?" query ]* path = fsegment *( "/" segment )* fsegment = 1*pchar*
	 * segment = *pchar* params = param *( ";" param )* param = *( pchar | "/" )
	 * * query = *( uchar | reserved )** http_URL = "http:" "//" host [ ":" port
	 * ] [ abs_path ]** host = <A legal Internet host domain name* or IP address
	 * (in dotted-decimal form),* as defined by Section 2.1 of RFC 1123>* port =
	 * *DIGIT** {absoluteURI, <scheme>, <host>, <port>, <path+params+query>}*
	 * when <scheme> = http | https* {scheme, <scheme>, <chars>}* wheb <scheme>
	 * is something else then http or https* {abs_path, <path>}** <string>
	 * (unknown form)*
	 */

	private static PacketHttpURI http_parse_uri(byte[] data, int uri_ptr,
			int uri_len) {

		PacketHttpURI uri = new PacketHttpURI();

		if ((uri_len == 1) && (data[uri_ptr + 0] == '*'))
			uri.type = URIType.URI_STAR;
		else if ((uri_len <= 1) || (data[uri_ptr + 0] == '/')) {
			uri.type = URIType.URI_ABS_PATH;
			uri.s1_data = data;
			uri.s1_ptr = uri_ptr;
			uri.s1_len = uri_len;
		} else if ((uri_len >= 7)
				&& (STRNCASECMP(data, uri_ptr, "http://", 7) == 0)) {
			uri_len -= 7;
			uri_ptr += 7;
			uri.type = URIType.URI_HTTP;
			http_parse_absoluteURI(uri, data, uri_ptr, uri_len);
		} else if ((uri_len >= 8)
				&& (STRNCASECMP(data, uri_ptr, "https://", 8) == 0)) {
			uri_len -= 8;
			uri_ptr += 8;
			uri.type = URIType.URI_HTTPS;
			http_parse_absoluteURI(uri, data, uri_ptr, uri_len);
		} else {
			int ptr;
			if ((ptr = memchr(data, uri_ptr, ':', uri_len)) == -1) {
				uri.type = URIType.URI_STRING;
				uri.s1_ptr = uri_ptr;
				uri.s1_len = uri_len;
			} else {
				int slen = ptr - uri_ptr;
				uri.type = URIType.URI_SCHEME;
				uri.s1_ptr = uri_ptr;
				uri.s1_len = slen;
				uri.s2_data = data;
				uri.s2_ptr = uri_ptr + (slen + 1);
				uri.s2_len = uri_len - (slen + 1);
			}
		}

		return uri;
	}

	static final byte[] SLASH = new byte[] { '/' };
	
	private static void http_parse_absoluteURI(PacketHttpURI uri, byte[] data,
			int uri_ptr, int uri_len) {

	    int p;
	    
	    if ((p = memchr(data, uri_ptr, '/', uri_len)) == -1) {
	        /* host [":" port] */
	    	
	    	uri.s2_data = SLASH;
	        uri.s2_ptr = 0;
	        uri.s2_len = 1;
	    }
	    else {
	        int n = (p - uri_ptr);
	        uri.s2_data = data;
	        uri.s2_ptr = p;
	        uri.s2_len = uri_len - n;
	        uri_len = n;
	    }

	    uri.s1_data = data;
	    uri.s1_ptr = uri_ptr;
	    uri.port = 0; /* undefined */
	    /* host[:port]  */
	    if ((p = memchr(data, uri_ptr, ':', uri_len)) == -1) {
	        uri.s1_len = uri_len;
	    }
	    else {
	        int n = (p - uri_ptr);
	        int port = 0;        
	        uri.s1_len = n;
	        n = uri_len - (n+1);
	        p++;
	        while(n!=0 && isdigit(data[p])) {
	            port = port*10 + (data[p] - '0');
	            n--;
	            p++;
	        }
	        if (n==0 && port!=0)
	            uri.port = port;
	  }
		
	}

	private static int STRNCASECMP(byte[] data, int s1, String s2, int n) {

		int i;

		for (i = 0; i < n - 1 && data[s1 + i] != 0 && i < s2.length()
				&& toupper(data[s1 + i]) == toupper((byte) s2.charAt(i)); ++i)
			;
		return (toupper(data[s1 + i]) - toupper((byte) s2.charAt(i)));

	}

	static class HTTPAtom {
		private final HTTPAtom next;
		private final int h;
		private final byte[] name;
		private final int len;
		final EAtom atom;
		final int index;

		public HTTPAtom(String am, int index, HTTPAtom[] hash) {
			this.index = index;
			this.atom = EAtom.intern(am);
			this.name = am.getBytes(IO.ISO_LATIN_1);

			int ptr = 0, len = 0, h = 0;
			while (ptr != name.length) {
				h = hash_update(h, name[ptr]);
				ptr++;
				len++;
			}
			int ix = h % hash.length;

			this.len = len;
			this.h = h;

			this.next = hash[ix];
			hash[ix] = this;

		}
	}

	static boolean[] tspecial = new boolean[128];
	static HTTPAtom[] http_hdr_hash = new HTTPAtom[HTTP_HDR_HASH_SIZE];
	static HTTPAtom[] http_meth_hash = new HTTPAtom[HTTP_METH_HASH_SIZE];

	static String http_hdr_strings[] = { "Cache-Control", "Connection", "Date",
			"Pragma", "Transfer-Encoding", "Upgrade", "Via", "Accept",
			"Accept-Charset", "Accept-Encoding", "Accept-Language",
			"Authorization", "From", "Host", "If-Modified-Since", "If-Match",
			"If-None-Match", "If-Range", "If-Unmodified-Since", "Max-Forwards",
			"Proxy-Authorization", "Range", "Referer", "User-Agent", "Age",
			"Location", "Proxy-Authenticate", "Public", "Retry-After",
			"Server", "Vary", "Warning", "Www-Authenticate", "Allow",
			"Content-Base", "Content-Encoding", "Content-Language",
			"Content-Length", "Content-Location", "Content-Md5",
			"Content-Range", "Content-Type", "Etag", "Expires",
			"Last-Modified", "Accept-Ranges", "Set-Cookie", "Set-Cookie2",
			"X-Forwarded-For", "Cookie", "Keep-Alive", "Proxy-Connection", null };

	static String http_meth_strings[] = { "OPTIONS", "GET", "HEAD", "POST",
			"PUT", "DELETE", "TRACE", null };

	static {
		int i;

		for (i = 0; i < 33; i++)
			tspecial[i] = true;
		String specials = "()<>@,;:\\\"/[]?={} \t";
		for (i = 0; i < specials.length(); i++) {
			char val = specials.charAt(i);
			tspecial[val] = true;
		}

		for (i = 0; i < HTTP_HDR_HASH_SIZE; i++)
			http_hdr_hash[i] = null;
		for (i = 0; http_hdr_strings[i] != null; i++) {
			assert (http_hdr_strings[i].length() <= HTTP_MAX_NAME_LEN);
			new HTTPAtom(http_hdr_strings[i], i, http_hdr_hash);
		}

		for (i = 0; i < HTTP_METH_HASH_SIZE; i++)
			http_hdr_hash[i] = null;
		for (i = 0; http_meth_strings[i] != null; i++) {
			new HTTPAtom(http_meth_strings[i], i, http_meth_hash);
		}
	}

	private static boolean is_tspecial(byte x) {
		if (x < 0) return false;
		return tspecial[x];
	}

	static final int hash_update(int h, int c) {
		c &= 0xff;
		int __g;
		(h) = ((h) << 4) + (c);
		if ((__g = (h) & 0xf0000000) != 0) {
			(h) ^= (__g >>> 24);
			(h) ^= __g;
		}

		return h;
	}

	private static boolean isdigit(byte[] data, int ptr) {
		byte ch = data[ptr];
		return (ch >= '0' && ch <= '9');
	}

	private static boolean isdigit(int data) {
		return (data >= '0' && data <= '9');
	}

	public static <T> int parse_ssl(byte[] buf, int start, int len,
			PacketCallbacks<T> pcb, T arg) {
		throw new NotImplemented();
	}

	/*
	 * Return > 0 Total packet length.in bytes = 0 Length unknown, need more
	 * data. < 0 Error, invalid format.
	 */
	public static int get_length(PacketParseType htype, byte[] data, int ptr,
			int n, int max_plen, int trunc_len, IntCell statep) {

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
			plen = data[ptr] & 0xff;
			break;

		case TCP_PB_2:
			hlen = 2;
			if (n < hlen)
				return 0;

			plen = ((data[ptr] & 0xff) << 8) | (data[ptr + 1] & 0xff);
			break;

		case TCP_PB_4:
			hlen = 4;
			if (n < hlen)
				return 0;

			plen = ((data[ptr] & 0xff) << 24) | ((data[ptr + 1] & 0xff) << 16)
					| ((data[ptr + 2] & 0xff) << 8) | (data[ptr + 3] & 0xff);
			break;

		case TCP_PB_LINE_LF: {
			int ptr2;
			if ((ptr2 = memchr(data, ptr, '\n', n)) == -1) {
				if (n >= trunc_len && trunc_len != 0) {
					return trunc_len;
				}
				return 0;
			}

			int len = (ptr2 - ptr) + 1;
			if (len > trunc_len && trunc_len != 0) {
				return trunc_len;
			}
			return len;
		}

		case TCP_PB_HTTPH:
		case TCP_PB_HTTPH_BIN:
			statep.set(~0);
		case TCP_PB_HTTP:
		case TCP_PB_HTTP_BIN:
			/* TCP_PB_HTTP: data \r\n(SP data\r\n)* */
			plen = n;
			if (((plen == 1) && NL(data, ptr))
					|| ((plen == 2) && CRNL(data, ptr)))
				return plen;
			else {
				int ptr1 = ptr;
				int len = plen;

				while (true) {
					int ptr2 = memchr(data, ptr1, '\n', len);

					if (ptr2 == -1) {
						if (n >= trunc_len && trunc_len != 0) { /* buffer full */
							plen = trunc_len;
							return plen;
						}
						return 0;
					} else {
						plen = (ptr2 - ptr) + 1;

						if (statep.get() == 0)
							return plen;

						if (plen < n) {
							if (SP(data, ptr2 + 1) && plen > 2) {
								/* header field value continue on next line */
								ptr1 = ptr2 + 1;
								len = n - plen;
							} else
								return plen;
						} else
							return 0;
					}
				}
			}

		default:
			throw new NotImplemented("packet parser for " + htype);
		}

		// remain case...
		int tlen = hlen + plen;
		if ((max_plen != 0 && plen > max_plen) || tlen < hlen) {
			return -1;
		}
		return tlen;

	}

	private static boolean SP(byte[] data, int ptr) {
		return data[ptr] == ' ' || data[ptr] == '\t';
	}

	private static boolean CRNL(byte[] data, int ptr) {
		return data[ptr] == '\r' && data[ptr + 1] == '\n';
	}

	private static boolean NL(byte[] data, int ptr) {
		return data[ptr] == '\n';
	}

	private static int memchr(byte[] data, int offset, char c, int n) {
		for (int i = 0; i < n; i++) {
			if (data[offset + i] == c)
				return offset + i;
		}
		return -1;
	}

}
