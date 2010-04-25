package erjang.driver.inet_gethost;

import java.util.concurrent.locks.ReentrantLock;

import erjang.EString;
import erjang.driver.EDriver;
import erjang.driver.EDriverControl;

public class Driver implements EDriver {

	@Override
	public String driverName() {
		return "inet_gethost";
	}

	@Override
	public boolean useDriverLevelLocking() {
		return false;
	}

	@Override
	public void finish() {
	}

	@Override
	public ReentrantLock getLock() {
		throw new erjang.NotImplemented();

	}

	@Override
	public EDriverControl start(EString command) {
		return new GetHostDriver(this, command);
	}

}
