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


package erjang.m.ets;

import erjang.EAtom;
import erjang.EBitString;
import erjang.ECons;
import erjang.EFun;
import erjang.ENumber;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.ERef;
import erjang.ETuple;

public abstract class ETermPattern {
	public boolean match(ETuple t, EMatchContext r) {
		return false;
	}

	public boolean match(ENumber n, EMatchContext r) {
		return false;
	}

	public boolean match(EAtom a, EMatchContext r) {
		return false;
	}

	public boolean match(ECons c, EMatchContext r) {
		return false;
	}

	public boolean match(EPID p, EMatchContext r) {
		return false;
	}

	public boolean match(EPort p, EMatchContext r) {
		return false;
	}

	public boolean match(EBitString bs, EMatchContext r) {
		return false;
	}

	public boolean match(EFun eFun, EMatchContext r) {
		return false;
	}

	public boolean match(ERef eRef, EMatchContext r) {
		return false;
	}
}