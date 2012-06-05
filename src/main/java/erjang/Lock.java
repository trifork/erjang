package erjang;

import kilim.*;
import java.util.LinkedList;

/**
 * Simple reentrant lock that uses Kilim's means for suspending
 * 
 * @author krab@trifork.com
 */
public class Lock implements PauseReason, EventPublisher {

	Task owner = null;
	int count;
	LinkedList<Task> waiters;

	static boolean $isWoven = true;

	public void lock() throws Pausable {
		Task.errNotWoven();
	}

	public void lock(Fiber f) {
		while (!lock(f.task)) {
			f.down();
			Task.pause(this, f);
			if(f.up() == Fiber.PAUSING__NO_STATE) {
                            f.setState(/* this, */ null);
			}
		}
	}

	private boolean lock(Task currentTask) {
		synchronized (this) {
			if (owner == null) {
				owner = currentTask;
				count = 1;
				return true;
			} else if (owner == currentTask) {
				count = count + 1;
				return true;
			} else {
				addListener(currentTask);
				return false;
			}
		}
	}

	private synchronized Task _unlock(Task currentTask) {
		Task next = null;

			if (owner != currentTask) {
				throw new IllegalStateException();
			}

			if ((count = count-1) == 0) {
				owner = null;
				if (waiters != null && !waiters.isEmpty()) {
					next = waiters.removeFirst();
				}
			}

		return next;
	}

	public void unlock() throws Pausable {
		Task.errNotWoven();
	}

	public void unlock(Fiber f) {
		Task next = _unlock(f.task);
		_unlock2(next);
	}

	private void _unlock2(Task next) {
		if (next != null) {
			next.resume();
		}
	}

	private void addListener(Task eo) {
		if (waiters == null) {
			waiters = new LinkedList<Task>();
		}

		waiters.add(eo);
	}

	public boolean isValid(Task t) {
		synchronized(this) {
			return waiters != null && waiters.contains(t);
		}
	}

}
