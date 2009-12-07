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
import java.nio.channels.ClosedChannelException;
import java.nio.channels.ClosedSelectorException;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
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

	ConcurrentLinkedQueue<NIOChannelInfo.Interest> setting = new ConcurrentLinkedQueue<NIOChannelInfo.Interest>();
	ConcurrentLinkedQueue<NIOChannelInfo.Interest> clearing = new ConcurrentLinkedQueue<NIOChannelInfo.Interest>();

	@Override
	public void run() {
		List<NIOChannelInfo> cancellations = new ArrayList<NIOChannelInfo>();

		select_loop: while (true) {

			while (!setting.isEmpty()) {
				process_add_interest_request(setting.remove());
			}
			
			while(!clearing.isEmpty()) {
				process_clear_interest_request(clearing.remove(), cancellations);
			}

			int num;
			if (!cancellations.isEmpty()) {

				try {
					num = selector.selectNow();
				} catch (ClosedSelectorException e) {
					// we're doomed!
					e.printStackTrace();
					return;
				} catch (IOException e) {
					e.printStackTrace();
				}

				for (NIOChannelInfo info : cancellations) {
					info.cancelled();
				}

				cancellations.clear();

				continue select_loop;

			} else {

				try {
					num = selector.select();
				} catch (ClosedSelectorException e) {
					e.printStackTrace();
					return;
				} catch (IOException e) {
					e.printStackTrace();
				}

			}

			// now, process the readyset

			Set<SelectionKey> ready = selector.selectedKeys();

			for (SelectionKey key : ready) {
				NIOChannelInfo req = (NIOChannelInfo) key.attachment();
				req.ready(key);
			}

		}

	}

	/**
	 * @param remove
	 * @param cancellations
	 */
	private void process_clear_interest_request(Interest interest, 
			List<NIOChannelInfo> cancellations) {

		SelectableChannel ch = interest.ch;
		SelectionKey key = ch.keyFor(selector);
		NIOHandler handler = interest.handler;

		if (key == null) {
			
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
				
				// make sure we get a callback after the selectNow call
				cancellations.add(info);
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
		
		if (key == null) {
			try {
				key = ch.register(selector, interest.ops, info = new NIOChannelInfo(interest));
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
		setting.add(new NIOChannelInfo.Interest(ch, handler, op, false));
		selector.wakeup();
	}
	
	static public void clearInterest(SelectableChannel ch, int op, boolean releaseNotify,
			NIOHandler handler) {
		INSTANCE._removeInterest(ch, op, releaseNotify, handler);
	}

	void _removeInterest(SelectableChannel ch, int op, boolean releaseNotify,
			NIOHandler handler) {
		clearing.add(new NIOChannelInfo.Interest(ch, handler, op, releaseNotify));
		selector.wakeup();
	}

}
