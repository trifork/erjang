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

package erjang.driver;

import java.nio.channels.CancelledKeyException;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.util.HashMap;
import java.util.Map;


/**
 * This class holds information per selectable channel in an IOSelector.  
 * Instances of this class are stored inside the SelectionKey.attachment.
 * 
 * 
 * 
 */
class NIOChannelInfo {

	static class Interest {
		SelectableChannel ch;
		NIOHandler handler;
		int ops;
		boolean releaseNotify;

		@Override
		public String toString() {
			return "{ch="+ch+"; ops="+Integer.toBinaryString(ops)+"; release="+releaseNotify+"}";
		}
		
		/**
		 * @param op
		 * @param timeout2
		 */
		public Interest(SelectableChannel ch, NIOHandler handler, int ops,
				boolean releaseNotify) {
			this.ch = ch;
			this.handler = handler;
			this.ops = ops;
			this.releaseNotify = releaseNotify;
		}

		/**
		 * @param old
		 * @return
		 */
		public Interest combine(Interest old) {
			return new Interest(ch, handler, ops|old.ops, releaseNotify||old.releaseNotify);
		}

	}

	public static final int ALL_OPS = SelectionKey.OP_ACCEPT
			| SelectionKey.OP_CONNECT | SelectionKey.OP_READ
			| SelectionKey.OP_WRITE;

	// TODO: use an optimized data structure for this
	Map<NIOHandler, Interest> interest = new HashMap<NIOHandler, Interest>();

	/**
	 * @param req
	 */
	public NIOChannelInfo(Interest interest) {
		add(interest);
	}

	/**
	 * @param req
	 */
	public void add(Interest ops) {
		Interest old = interest.get(ops.handler);
		if (old == null) {
			interest.put(ops.handler, ops);
		} else {
			interest.put(ops.handler, ops.combine(old));
		}
	}

	/**
	 * Called in top of main select loop, when a client has requested that
	 * select interest be removed.
	 * 
	 * @param ioRequest
	 */
	public void clear_interest(Interest req) {
		NIOHandler handler = req.handler;
		boolean releaseNotify = req.releaseNotify;
		SelectableChannel ch = req.ch;

		Interest old = interest.get(handler);
		if (old == null) {
			if (releaseNotify && handler != null) {
				handler.released(ch);
			}

		} else {
			// normal case, there has actually been listening
			// going on for this handler

			old.releaseNotify |= releaseNotify;

			old.ops &= ~req.ops;
			// is he still interested in some ops?

			if (old.ops == 0 && !old.releaseNotify) {
				interest.remove(handler);

			}
		}

	}

	/**
	 * @param key
	 */
	public int updateInterestOpsFor(SelectionKey key) {
		int ops = computeOps();
		key.interestOps(ops);
		return ops;
	}

	/**
	 * @return
	 */
	private int computeOps() {
		int ops = 0;
		for (Interest i : interest.values()) {
			ops |= i.ops;
		}
		return ops;
	}

	/**
	 * Called in main select loop, after select when we know that 
	 * there is something ready with this channel...
	 * 
	 * @param key
	 */
	public void ready(SelectionKey key) {
		final SelectableChannel ch = key.channel();
		int readyOps;
		
		try {
			readyOps = key.readyOps();
		} catch (CancelledKeyException e) {

			// TODO: releaseNotify ?

			return; // just ignore this condition
		}
			
		Interest[] ents = interest.values().toArray(
				new Interest[interest.size()]);

		// walk thru each handler registered for this channel...
		for (Interest want : ents) {

			int gotOps = want.ops & readyOps;

			// did he get what he wanted?
			if (gotOps != 0) {

				NIOHandler handler = want.handler;
				if (handler != null) {
					handler.ready(ch, gotOps);
				}
				
				// remove interest in the operations we got
				want.ops &= ~gotOps;

				if (want.ops == 0 && !want.releaseNotify) {
					interest.remove(handler);
				}
			}
		}
		
		try {
			key.interestOps(computeOps());
		
			if (interest.isEmpty()) {
				key.cancel();
			}
		} catch (CancelledKeyException e) {
			// ok
		}
	}

	/**
	 * 
	 */
	public void cancelled() {

		// are we done, so we can close?
		for (Interest i : interest.values()) {
			if (i.releaseNotify && i.handler != null) {
				i.handler.released(i.ch);
			}
		}

		// is that worth it?
		interest.clear();

	}

}