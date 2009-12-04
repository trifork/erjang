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


package erjang.m.timer;

import java.util.concurrent.TimeUnit;

import kilim.Pausable;
import erjang.BIF;
import erjang.EAtom;
import erjang.EFun;
import erjang.EModule;
import erjang.ENative;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.ETuple;
import erjang.FunID;

/**
 * 
 */
public class Native extends ENative {

	/* (non-Javadoc)
	 * @see erjang.ENative#getNativeClasses()
	 */
	@Override
	protected Class<?>[] getNativeClasses() {
		return new Class[] { Native.class };
	}

	@BIF
	public static final EObject tc(EProc proc, EObject mod, EObject fun, EObject args) throws Pausable
	{
		long before = System.nanoTime();
		
		EAtom m = mod.testAtom();
		EAtom f = fun.testAtom();
		ESeq  a = args.testSeq();
		
		if (m==null||f==null||a==null) throw ERT.badarg(mod, fun);
		
		EFun efun = EModule.resolve(new FunID(m,f,a.length()));

		EObject result = efun.apply(proc, a);
		
		long after = System.nanoTime();
		
		long micros = TimeUnit.MICROSECONDS.convert(after-before, TimeUnit.NANOSECONDS);
		
		return ETuple.make(ERT.box(micros), result);
	}
}
