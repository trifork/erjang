package erjang.driver.tcp_inet;

import kilim.Pausable;
import erjang.EAtom;
import erjang.EBinary;
import erjang.EObject;
import erjang.ERT;
import erjang.EString;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.driver.tcp_inet.Packet.HTTPAtom;
import erjang.driver.tcp_inet.TCPINet.ActiveType;
import erjang.driver.tcp_inet.TCPINet.AsyncOp;

public class TCPINetCallbacks extends PacketCallbacks<TCPINet> {

	private static final EAtom am_http_request = EAtom.intern("http_request");
	private static final EAtom am_inet_async = EAtom.intern("inet_async");
	private static final EAtom am_http = EAtom.intern("http");
	private static final EAtom am_star = EAtom.intern("*");
	private static final EAtom am_abs_path = EAtom.intern("abs_path");
	private static final EAtom am_https = EAtom.intern("https");
	private static final EAtom am_absoluteURI = EAtom.intern("absoluteURI");
	private static final EAtom am_scheme = EAtom.intern("scheme");
	private static final EAtom am_http_header = EAtom.intern("http_header");
	private static final EAtom am_http_eoh = EAtom.intern("http_eoh");
	private static final EAtom am_http_error = EAtom.intern("http_error");

	@Override
	int http_eoh(TCPINet desc) throws Pausable {
		return send_http(desc, am_http_eoh);
	}

	@Override
	int http_error(TCPINet desc, byte[] data, int pos, int len) throws Pausable {

		EObject line = load_string(desc, data, pos, len);

		ETuple2 req = new ETuple2(am_http_error, line);
		
		if (desc.active == ActiveType.PASSIVE) {
	        /* {inet_async, S, Ref, {error,{http_error,Line}}} */

			AsyncOp op = desc.deq_async();
			if (op == null) {
				return -1;
			}
			
			ETuple2 ok = new ETuple2(ERT.am_error, req);
			ETuple msg = ETuple.make(am_inet_async, desc.port(), ERT.box(op.id), ok);
			
			desc.driver_send_term(op.caller, msg);

		} else {
	        /* {http, S, {http_error, Line}}} */
			
			ETuple http = ETuple.make(am_http, desc.port(), req);
			
			desc.driver_output_term(http);
		}
		
		return 0;
	}

	@Override
	int http_header(TCPINet desc, HTTPAtom name, byte[] nameBuf, int namePtr,
			int nameLen, byte[] valBuf, int valPtr, int valLen) throws Pausable {

		EObject bit = (name == null)
					? ERT.box(0)
					: ERT.box( name.index + 1 );
		EObject nam = (name == null)
					? load_string(desc, nameBuf, namePtr, nameLen)
					: name.atom;
		
		EObject value = load_string(desc, valBuf, valPtr, valLen);
					
					
		ETuple req = ETuple.make(am_http_header, bit, nam, ERT.am_undefined, value);

		return send_http(desc, req);
	}

	@Override
	int http_request(TCPINet desc, HTTPAtom method, byte[] data, int meth_ptr,
			int meth_len, PacketHttpURI uri, int major, int minor) throws Pausable {
		
		EObject meth = 
				(method == null) 
				? load_string(desc, data, meth_ptr, meth_len)
				: method.atom; 
		
		ETuple version = new ETuple2(ERT.box(major), ERT.box(minor));
		ETuple req = ETuple.make(am_http_request, meth, load_uri(desc, uri), version);
		
		return send_http(desc, req);

	}

	private int send_http(TCPINet desc, EObject req) throws Pausable {
		if (desc.active == ActiveType.PASSIVE) {
	        /* {inet_async, S, Ref, {ok,{http_request,Meth,Uri,Version}}} */

			AsyncOp op = desc.deq_async();
			if (op == null) {
				return -1;
			}
			
			ETuple2 ok = new ETuple2(ERT.am_ok, req);
			ETuple msg = ETuple.make(am_inet_async, desc.port(), ERT.box(op.id), ok);
			
			desc.driver_send_term(op.caller, msg);

		} else {
	        /* {http, S, {http_request,Meth,Uri,Version}}} */
			
			ETuple http = ETuple.make(am_http, desc.port(), req);
			
			desc.driver_output_term(http);

		}
		
		return 0;
	}

	// HttpUri = '*'
	//         | {absoluteURI, http|https, Host=HttpString, Port=int()|undefined, Path=HttpString}
	//         | {scheme, Scheme=HttpString, HttpString}
	//         | {abs_path, HttpString}
	//         | HttpString

	private EObject load_uri(TCPINet desc, PacketHttpURI uri) {
		EAtom scheme = null;
		switch (uri.type) {
		case URI_STAR: 
			return (EAtom)am_star;
		case URI_ABS_PATH: 
			return new ETuple2(am_abs_path, load_string(desc, uri.s1_data, uri.s1_ptr, uri.s1_len));
		case URI_HTTPS:
			scheme = am_https;
		case URI_HTTP:
			if (scheme == null) { scheme = am_http; }
			return ETuple.make(am_absoluteURI,
							   scheme,
							   load_string(desc, uri.s1_data, uri.s1_ptr, uri.s1_len),
							   (uri.port==0 ? ERT.am_undefined : ERT.box(uri.port)),
							   load_string(desc, uri.s2_data, uri.s2_ptr, uri.s2_len));
		case URI_STRING:
			return load_string(desc, uri.s1_data, uri.s1_ptr, uri.s1_len);
		case URI_SCHEME:
			return ETuple.make(am_scheme,
					   load_string(desc, uri.s1_data, uri.s1_ptr, uri.s1_len),
					   load_string(desc, uri.s2_data, uri.s2_ptr, uri.s2_len));
		}

		throw new InternalError("should not happen");
		
	}

	private EObject load_string(TCPINet desc, byte[] data, int data_off,
			int data_len) {

		switch(desc.htype) {
		case TCP_PB_HTTPH_BIN:
		case TCP_PB_HTTP_BIN:
			return EBinary.make(data, data_off, data_len, 0 /*extra bits*/);
		default:
			return EString.make(data, data_off, data_len);
		} 
	}

	@Override
	int http_response(TCPINet arg, int major, int minor, int status,
			byte[] data, int off, int len) {
		throw new erjang.NotImplemented();

	}

}
