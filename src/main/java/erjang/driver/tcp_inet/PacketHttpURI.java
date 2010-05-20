package erjang.driver.tcp_inet;

class PacketHttpURI {

	static enum URIType {
		URI_STAR,    /* '*' */
		URI_STRING,  /* "string(s1)" */
		URI_ABS_PATH,/* {abs_path, "path(s1)"} */
		URI_SCHEME,  /* {scheme, "scheme(s1)", "string(s2)"} */
		URI_HTTP,    /* {absoluteURI, http, "host(s1)", Port, "path(s2)"} */
		URI_HTTPS    /* {absoluteURI, https, ... */
	};


	URIType type;
	public byte[] s1_data;
	public int s1_ptr;
	public int s1_len;
	
	public byte[] s2_data;
	public int s2_ptr;
	public int s2_len;

	public int port;
}
