package erjang.kilim;

import java.util.TimerTask;

import kilim.EventSubscriber;
import kilim.Mailbox;
import kilim.Pausable;
import kilim.Task;

public class EMailbox<T> extends Mailbox<T> {


	public EMailbox(int i, int maxMailboxSize) {
		super(i,maxMailboxSize);
	}

	/**
	 * @return non-null message.
	 * @throws Pausable
	 */
	public void untilHasMessage() throws Pausable {
		while (hasMessage(Task.getCurrentTask()) == false) {
			Task.pause(this);
		}
	}

	/**
	 * @return non-null message.
	 * @throws Pausable
	 */
	public void untilHasMessages(int num) throws Pausable {
		while (hasMessages(num, Task.getCurrentTask()) == false) {
			Task.pause(this);
		}
	}

	/**
	 * @return non-null message.
	 * @throws Pausable
	 */
	public boolean untilHasMessage(long timeoutMillis) throws Pausable {
		final Task t = Task.getCurrentTask();
		boolean has_msg = hasMessage(t);
		long end = System.currentTimeMillis() + timeoutMillis;
		while (has_msg == false) {
			TimerTask tt = new TimerTask() {
				public void run() {
					EMailbox.this.removeMsgAvailableListener(t);
                    t.onEvent(EMailbox.this, timedOut);
				}
			};
			Task.timer.schedule(tt, timeoutMillis);
			Task.pause(this);
			tt.cancel();
			has_msg = hasMessage(t);
			timeoutMillis = end - System.currentTimeMillis();
			if (timeoutMillis <= 0) {
				removeMsgAvailableListener(t);
				break;
			}
		}
		return has_msg;
	}

	/**
	 * @return non-null message.
	 * @throws Pausable
	 */
	public boolean untilHasMessages(int num, long timeoutMillis)
			throws Pausable {
		final Task t = Task.getCurrentTask();
		final long end = System.currentTimeMillis() + timeoutMillis;

		boolean has_msg = hasMessages(num, t);
		while (has_msg == false) {
			TimerTask tt = new TimerTask() {
				public void run() {
					EMailbox.this.removeMsgAvailableListener(t);
                    t.onEvent(EMailbox.this, timedOut);
				}
			};
			Task.timer.schedule(tt, timeoutMillis);
			Task.pause(this);
			if (!tt.cancel()) {
				removeMsgAvailableListener(t);
			}
			
			has_msg = hasMessages(num, t);
			timeoutMillis = end - System.currentTimeMillis();
			if (!has_msg && timeoutMillis <= 0) {
				removeMsgAvailableListener(t);
				break;
			}
		}
		return has_msg;
	}

	/**
	 * Non-blocking, nonpausing "wait-until-message-available".
	 * 
	 * @param eo
	 *            . If non-null, registers this observer and calls it with a
	 *            MessageAvailable event when a put() is done.
	 * @return true's one, or false
	 */
	public boolean hasMessage(EventSubscriber eo) {
		boolean has_msg;
		synchronized (this) {
			int n = numMsgs;
			if (n > 0) {
				has_msg = true;
			} else {
				has_msg = false;
				addMsgAvailableListener(eo);
			}
		}
		return has_msg;
	}

	public boolean hasMessages(int num, EventSubscriber eo) {
		boolean has_msg;
		synchronized (this) {
			int n = this.numMsgs;
			if (n >= num) {
				has_msg = true;
			} else {
				has_msg = false;
				addMsgAvailableListener(eo);
			}
		}
		return has_msg;
	}

	/**
	 * Non-blocking, nonpausing peek.
	 * 
	 * @return buffered message if there's one, or null
	 */
	public T peek(int idx) {
		assert idx >= 0 : "negative index";
		T msg;
		synchronized (this) {
			int n = numMsgs;
			if (idx < n) {
				int ic = icons;
				msg = msgs[(ic + idx) % msgs.length];
				
				assert msg != null : "peeked null message!";
			} else {
				msg = null;
			}
		}
		return msg;
	}

	public T remove(final int idx) {
		assert idx >= 0 : "negative index";
		T msg;
		synchronized (this) {
			int n = numMsgs;
			assert idx < numMsgs;
			if (idx < n) {
				int ic = icons;
				int mlen = msgs.length;
				msg = msgs[(ic + idx) % mlen];
				for (int i = idx; i > 0; i--) {
					msgs[(ic + i) % mlen] = msgs[(ic + i - 1) % mlen];
				}
				msgs[icons] = null;
				numMsgs -= 1;
				icons = (icons + 1) % mlen;
			} else {
				throw new IllegalStateException();
			}
		}
		return msg;
	}

	public synchronized Object[] messages() {
		synchronized (this) {
			Object[] result = new Object[numMsgs];
			for (int i = 0; i < numMsgs; i++) {
				result[i] = msgs[(icons + i) % msgs.length];
			}
			return result;
		}

	}

}
