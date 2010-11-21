/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2009 by Trifork
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *  
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/


package erjang;

import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Collections;
import java.util.Map;

import kilim.Pausable;
import kilim.Task;

/**
 * 
 */
public abstract class ETimerTask extends TimerTask implements ExitHook {
	// TODO: implement a Kilim timer, that allows Pausable in on_timeout.
	// for now, we will live with the risk of having a 
	// blocking send in a timer...

	static ConcurrentHashMap<ERef, ETimerTask> timer_refs = new ConcurrentHashMap();
	static Timer send_timer = new Timer();
	
	final ERef ref;
	private final EInternalPID pid;
	long when = -1;
	
	public ETimerTask() {
		ref = ERT.getLocalNode().createRef();
		pid = null;
		timer_refs.put(ref, this);
	}
	
	public ETimerTask(EInternalPID pid) {
		ref = ERT.getLocalNode().createRef();
		
		this.pid = pid;

		if (pid != null) {
			pid.add_exit_hook(this);
		}
		timer_refs.put(ref, this);
	}
	
	/** called when the timer fires */
	@Override
	public synchronized final void run() {
		if (timer_refs.remove(ref) == null) {
			return;
		}
		
		if (pid != null) { 
			pid.remove_exit_hook(this); 
		}

		ERT.scheduler.schedule( new Task() {
			@Override
			public void execute() throws Pausable, Exception {
				on_timeout();
			}
		} );
	}

	@Override
	public boolean cancel() {
		return cancel_timer() >= 0;
	}
	
	public synchronized long cancel_timer() {
		if (super.cancel()) {
			timer_refs.remove(ref);
			if (pid != null) {
				pid.remove_exit_hook(this);
			}
		
			return when - System.currentTimeMillis();
		} else {
			return -1;
		}
	}

	public static long cancel(ERef ref) {
		ETimerTask timer_task = timer_refs.get(ref);
		if (timer_task != null) {
			return timer_task.cancel_timer();
		}
		
		return -1;
	}
	
	public synchronized final void on_exit(EInternalPID pid) throws Pausable {
		assert (pid == this.pid) : "received on_exit callback from unknown pid";
		timer_refs.remove(ref);
		this.cancel();
	};

	protected abstract void on_timeout() throws Pausable;

	/**
	 * @param longValue
	 */
	public void schedule(long ms_delay) {
		when = System.currentTimeMillis() + ms_delay;
		send_timer.schedule(this, ms_delay);
	}

	/**
	 * @param timerRef
	 * @return
	 */
	public static long read_timer(ERef ref) {
		ETimerTask timer_task = timer_refs.get(ref);
		if (timer_task != null) {
			return timer_task.when - System.currentTimeMillis();
		}
		
		return -1;
	}
}
