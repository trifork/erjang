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

import java.io.IOException;
import java.nio.channels.CancelledKeyException;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.ClosedSelectorException;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentLinkedQueue;

import erjang.driver.NIOChannelInfo.Interest;

/**
 * 
 */
public class NIOSelector extends Thread {

	static final NIOSelector INSTANCE = new NIOSelector();
	private Selector selector;

	public NIOSelector() {
		setDaemon(true);
		try {
			selector = Selector.open();
		} catch (IOException e) {
			e.printStackTrace();
		}
		start();
	}

	static public SelectionKey interest(SelectableChannel ch) {
		return ch.keyFor(INSTANCE.selector);
	}
	
	static enum SetOrClear {
		SET, CLEAR;
	}
	
	static class SelectMsg {
		SetOrClear set;
		NIOChannelInfo.Interest interest;		
	}
	
	ArrayBlockingQueue<SelectMsg> mbox = new ArrayBlockingQueue<SelectMsg>(2);

	@Override
	public void run() {
		try {
			run0();
		} catch (Throwable e) {
			System.err.println("unhandled exception in Select loop");
			e.printStackTrace(System.err);
		}
	}

	public void run0() {
		// List<NIOChannelInfo> cancellations = new ArrayList<NIOChannelInfo>();

		select_loop: while (true) {

			SelectMsg msg;
			msg_loop: while ((msg = mbox.poll()) != null) {
				
				// System.err.println("SELECT_MSG: "+msg.set+" >> "+msg.interest);
				
				if (msg.set == SetOrClear.SET) {
					process_add_interest_request(msg.interest);
				} else {
					process_clear_interest_request(msg.interest);
					
					try {
						selector.selectNow();
					} catch (IOException e) {
						e.printStackTrace();
						// xx //
					}
				}
				
			}
			
			
			int num;

				if (false) {
					System.err.println("select loop");
					
					for (SelectionKey key : selector.keys()) {
						if (key.isValid()) {
							
							SelectableChannel ch = key.channel();
							int interst = key.interestOps();
							System.err.println(Integer.toBinaryString(interst) + ":"  + ch);
							
						}
					}
				}
				
				
				try {
					num = selector.select(10000);
				} catch (CancelledKeyException e) {
					// ignore this, as per
					// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4729342

				} catch (ClosedSelectorException e) {
					e.printStackTrace();
					return;
				} catch (IOException e) {
					e.printStackTrace();
				}


			// now, process the readyset

			Set<SelectionKey> ready = selector.selectedKeys();

			for (SelectionKey key : ready) {
				// System.err.println("READY: "+key.channel()+":"+Integer.toBinaryString(key.readyOps()));
				
				NIOChannelInfo req = (NIOChannelInfo) key.attachment();
				if (req == null) {
					System.err.println("Internal error, SelectionKey's attachement is null");
				} else {
					req.ready(key);
				}
			}

		}

	}

	/**
	 * @param remove
	 * @param cancellations
	 */
	private void process_clear_interest_request(Interest interest) {

		SelectableChannel ch = interest.ch;
		SelectionKey key = ch.keyFor(selector);
		NIOHandler handler = interest.handler;

		if (key == null || !key.isValid()) {
			
			// TODO: maybe this should be considered an error?
			if (handler != null && interest.releaseNotify) {
				handler.released(ch);
			}
			
		} else {
			
			// get channel info
			NIOChannelInfo info = (NIOChannelInfo) key.attachment();
			
			if (info == null) {
				// error!
				throw new InternalError();
			}
			
			// tell info that we're note interested in this
			info.clear_interest(interest);
			
			// does that add up to, that we are not interested at all?
			if (info.updateInterestOpsFor(key) == 0) {
				
				// cancel the key
				key.cancel();
				
			}
		}
		
	}

	/**
	 * @param remove
	 * @param active2
	 */
	private void process_add_interest_request(Interest interest) {
		SelectableChannel ch = interest.ch;
		SelectionKey key = ch.keyFor(selector);
		NIOChannelInfo info = null;
		
		if (key != null && !key.isValid()) {
			try {
				selector.selectNow();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			key = null;
		}
		if (key == null || !key.isValid()) {
			try {
				key = ch.register(selector, interest.ops, 
						info = new NIOChannelInfo(interest));
			} catch (ClosedChannelException e) {
				interest.handler.exception(ch, e);
			}
		} else {
			info = (NIOChannelInfo) key.attachment();
			info.add(interest);
			info.updateInterestOpsFor(key);
		}
		
	}
	
	static public void setInterest(SelectableChannel ch, int op, 
			NIOHandler handler) {
		INSTANCE._addInterest(ch, op, handler);
	}

	void _addInterest(SelectableChannel ch, int op, 
			NIOHandler handler) {
		SelectMsg msg = new SelectMsg();
		msg.set = SetOrClear.SET;
		msg.interest = new NIOChannelInfo.Interest(ch, handler, op, false);
		try {
			// System.err.println("SELECT_MSG: "+msg.set+" ++>> "+msg.interest);

			mbox.put(msg);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		selector.wakeup();
	}
	
	static public void clearInterest(SelectableChannel ch, int op, boolean releaseNotify,
			NIOHandler handler) {
		INSTANCE._removeInterest(ch, op, releaseNotify, handler);
	}

	void _removeInterest(SelectableChannel ch, int op, boolean releaseNotify,
			NIOHandler handler) {
		SelectMsg msg = new SelectMsg();
		msg.set = SetOrClear.CLEAR;
		msg.interest = new NIOChannelInfo.Interest(ch, handler, op, releaseNotify);
		try {
			// System.err.println("SELECT_MSG: "+msg.set+" ++>> "+msg.interest);

			mbox.put(msg);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		selector.wakeup();
	}

}
