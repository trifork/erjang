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
package erjang.m.net_kernel;

import erjang.ENative;
import erjang.BIF;
import erjang.EObject;
import erjang.EAtom;
import erjang.EPID;
import erjang.ERT;

public class Native extends ENative {
	@BIF
	public static EAtom dflag_unicode_io(EObject pid0) {
	    EPID pid;
	    if ((pid = pid0.testPID()) == null)
		throw ERT.badarg(pid0);
	    return ERT.TRUE; // A bit too simple perhaps.
	}
}
