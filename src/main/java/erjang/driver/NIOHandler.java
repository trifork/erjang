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
import java.nio.channels.SelectableChannel;


/**
 * 
 */
public interface NIOHandler {

	/**
	 * Exception happened during IO/Select
	 * @param ch 
	 * 
	 * @param e
	 */
	void exception(SelectableChannel ch, IOException e);

	/**
	 * @param ch
	 * @param i
	 */
	void ready(SelectableChannel ch, int readyOps);

	/**
	 * Called when <code>ch</code> is free to be closed.
	 * 
	 * @param ch
	 */
	void released(SelectableChannel ch);

}
