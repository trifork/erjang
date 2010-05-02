/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

/** default allocator for EFun's coming from the wire */
public class EClassEFunMaker implements EFunMaker {

	private Constructor<? extends EFun> cons;

	@SuppressWarnings("unchecked")
	public EClassEFunMaker(Class<? extends EFun> c) {
		Constructor<?>[] cons = c.getConstructors();
		this.cons = (Constructor<? extends EFun>) cons[0];
	}
	
	@Override
	public EFun make(EPID pid, EObject[] freevars) {
		
		Object[] initargs = new Object[freevars.length+1];
		initargs[0] = pid;
		System.arraycopy(freevars, 0, initargs, 1, freevars.length);
		
		try {
			return cons.newInstance(initargs);
		} catch (InstantiationException e) {
			e.printStackTrace();
			throw new InternalError();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			throw new InternalError();
		} catch (InvocationTargetException e) {
			e.printStackTrace();
			throw new InternalError();
		}
		
	}

}
