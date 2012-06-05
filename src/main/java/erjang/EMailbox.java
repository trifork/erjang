

package erjang;

import kilim.*;
import java.util.TimerTask;

public class EMailbox extends kilim.Mailbox<EObject> {

    public EMailbox() {
        super(10, ETask.MAX_MAILBOX_SIZE);
    }

    public EObject get(long timeout) throws Pausable {
        if (untilHasMessage(timeout)) {
            return get();
        } else {
            return null;
        }
    }

    /**
     * @return non-null message.
     * @throws Pausable
     */
    public void untilHasMessage() throws Pausable{
        while (hasMessage(Task.getCurrentTask()) == false) {
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
        long begin = System.currentTimeMillis();
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
            if (System.currentTimeMillis() - begin > timeoutMillis) {
                break;
            }
            has_msg = hasMessage(t);
        }
        return has_msg;
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
			int n = numMsgs;
			if (n >= num) {
				has_msg = true;
			} else {
				has_msg = false;
				addMsgAvailableListener(eo);
			}
		}
		return has_msg;
	}

        private EObject _get(int i) {
            return (EObject)(msgs()[i]);
        }

        private int _msg_len() {
            return msgs().length;
        }

    /**
     * Non-blocking, nonpausing peek. 
     * @return buffered message if there's one, or null 
     */
    public EObject peek() {
        EObject msg;
        synchronized(this) {
            int n = numMsgs;
            if (n > 0) {
                int ic = icons;
                msg = _get(ic);
            } else {
                msg = null;
            }
        }
        return msg;
    }
    
	/**
	 * Non-blocking, nonpausing peek.
	 * 
	 * @return buffered message if there's one, or null
	 */
	public EObject peek(int idx) {
		assert idx >= 0 : "negative index";
		EObject msg;
		synchronized (this) {
			int n = numMsgs;
			if (idx < n) {
				int ic = icons;
				msg = _get((ic + idx) % _msg_len());

				assert msg != null : "peeked null message!";
			} else {
				msg = null;
			}
		}
		return msg;
	}

	public EObject remove(final int idx) {
		assert idx >= 0 : "negative index";
		EObject msg;
		synchronized (this) {
			int n = numMsgs;
			assert idx < numMsgs;
			if (idx < n) {
				int ic = icons;
				int mlen = _msg_len();
				msg = _get((ic + idx) % mlen);
				for (int i = idx; i > 0; i--) {
                                    msgs()[(ic + i) % mlen] = msgs()[(ic + i - 1) % mlen];
				}
				msgs()[icons] = null;
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
                            result[i] = _get((icons + i) % _msg_len());
			}
			return result;
		}

	}

}
