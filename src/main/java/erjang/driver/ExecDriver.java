package erjang.driver;

import java.util.concurrent.locks.ReentrantLock;

import erjang.EString;
import erjang.ETuple2;

public class ExecDriver implements EDriver {

	public ExecDriver(ETuple2 name) {
		// TODO Auto-generated constructor stub
	}

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
