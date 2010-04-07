package erjang.net;

import java.io.IOException;

import erjang.driver.efile.Posix;

@SuppressWarnings("serial")
public class PosixIOException extends IOException {

	public final int errno;

	public PosixIOException(int errno, String string) {
		super(Posix.errno_id(errno).toUpperCase()+": "+string);
		this.errno = errno;
	}

}
