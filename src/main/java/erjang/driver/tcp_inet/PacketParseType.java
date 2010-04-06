/**
 * 
 */
package erjang.driver.tcp_inet;

public enum PacketParseType {
    TCP_PB_RAW,
    TCP_PB_1,
    TCP_PB_2,
    TCP_PB_4,
    TCP_PB_ASN1,
    TCP_PB_RM,
    TCP_PB_CDR,
    TCP_PB_FCGI,
    TCP_PB_LINE_LF,
    TCP_PB_TPKT,
    TCP_PB_HTTP,
    TCP_PB_HTTPH,
    TCP_PB_SSL_TLS,
    TCP_PB_HTTP_BIN,
    TCP_PB_HTTPH_BIN
	;

	public static PacketParseType valueOf(int ival) {
		switch (ival) {
		case 0: return TCP_PB_RAW;
		case 1: return TCP_PB_1;
		case 2: return TCP_PB_2;
		case 3: return TCP_PB_4;
		case 4: return TCP_PB_ASN1;
		case 5: return TCP_PB_RM;
		case 6: return TCP_PB_CDR;
		case 7: return TCP_PB_FCGI;
		case 8: return TCP_PB_LINE_LF;
		case 9: return TCP_PB_TPKT;
		case 10: return TCP_PB_HTTP;
		case 11: return TCP_PB_HTTPH;
		case 12: return TCP_PB_SSL_TLS;
		case 13: return TCP_PB_HTTP_BIN;
		case 14: return TCP_PB_HTTPH_BIN;
		default: throw new IllegalArgumentException();
		}

	}

}