package erjang.driver;

import java.util.concurrent.locks.ReentrantLock;

import erjang.EString;

public class FDDriver implements EDriver {

	@Override
	public String driverName() {
		throw new erjang.NotImplemented();

	}

	@Override
	public void finish() {
		throw new erjang.NotImplemented();

	}

	@Override
	public EDriverControl start(EString command) {
		throw new erjang.NotImplemented();

	}

	@Override
	public boolean useDriverLevelLocking() {
		return false;
	}

	@Override
	public ReentrantLock getLock() {
		throw new erjang.NotImplemented();
		
	}
}
