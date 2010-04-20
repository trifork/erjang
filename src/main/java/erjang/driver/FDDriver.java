package erjang.driver;

import kilim.Lock;
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
	public Lock getLock() {
		throw new erjang.NotImplemented();
		
	}
}
