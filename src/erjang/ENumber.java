package org.erlang;

import java.math.BigInteger;

public abstract class ENumber extends ETerm {

	public abstract int intValue();

	public static ENumber parseInt(String str) {
        try {     
        	int val = java.lang.Integer.parseInt(str);
        	return new EInteger(val);
         } catch (NumberFormatException e) {
        	 BigInteger val = new java.math.BigInteger(str);
        	 return new EBig(val);
         }
	}

	public ENumber asNumber() {
		return this;
	}


	public abstract ENumber asb();

	public ENumber minus(ENumber n2) {
		throw new NotImplemented();
	}

	public ENumber trunc() {
		throw new NotImplemented();
	}

	public ENumber rem(ENumber n2) {
		throw new NotImplemented();
	}

	public ENumber rem(int v2) {
		throw new NotImplemented();
	}

	public ENumber minus(int v2) {
		throw new NotImplemented();
	}

	public ENumber add(ENumber n2) {
		throw new NotImplemented();
	}

	public ENumber multiply(ENumber n2) {
		throw new NotImplemented();
	}

}
