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


package erjang.driver.efile;

import java.nio.channels.FileChannel;

import kilim.Pausable;

import erjang.driver.EAsync;

/**
 * 
 */
public abstract class FileAsync implements EAsync {

	public int command;

	public boolean again;
	protected FileChannel fd;
	protected int level;
	protected int flags;
	protected boolean reply;
	protected int posix_errno;
	protected boolean result_ok;

	public abstract void async();
	
	@Override
	public abstract void ready() throws Pausable;

	/**
	 * Used by WriteAsync and PWriteVAsync to deque output buffer
	 */
	public void deq_free_size() {
	}
	
	protected void reply(EFile efile) throws Pausable {
		if (result_ok) { 
			efile.reply_ok();
		} else {
			efile.reply_posix_error(posix_errno);
		}
	}

}
