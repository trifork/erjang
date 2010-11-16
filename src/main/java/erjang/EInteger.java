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

import java.math.BigInteger;
import java.io.IOException;


/**
 * 
 */
public abstract class EInteger extends ENumber {

	abstract BigInteger bigintValue();

	public EInteger testInteger() {
		return this;
	}

	public EInteger asInteger() {
		return this;
	}

	public abstract EInteger dec();
	
	/**
	 * @return
	 */
	public long longValue() {
		return bigintValue().longValue();
	}

	public static EInteger read(EInputStream ei) throws IOException {
		return ei.read_tagged_integer();
	}

}
