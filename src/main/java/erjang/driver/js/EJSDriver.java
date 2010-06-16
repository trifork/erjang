package erjang.driver.js;

import java.util.concurrent.locks.ReentrantLock;

import erjang.EString;
import erjang.driver.EDriver;
import erjang.driver.EDriverControl;

public class EJSDriver implements EDriver {

	private kilim.ReentrantLock lock;

	@Override
	public String driverName() {
		return "erlang_js_drv";
	}

	@Override
	public void finish() {
	}

	@Override
	public ReentrantLock getLock() {
		if (lock == null) {
			lock = new kilim.ReentrantLock();
		}
		return lock;
	}

	@Override
	public EDriverControl start(EString command) {
		return new EJSDriverInstance(this);
	}

	@Override
	public boolean useDriverLevelLocking() {
		return false;
	}

}
