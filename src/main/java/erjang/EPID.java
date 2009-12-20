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

import java.util.Set;

import erjang.m.ets.EPattern;
import erjang.m.ets.ETermPattern;


public abstract class EPID extends EHandle {

	/**
	 * 
	 */
	public EPID(ENode node) {
		super(node);
	}
	
	@Override
	int cmp_order() {
		return CMP_ORDER_PID;
	}
	
	public EPID testPID() {
		return this;
	}

	public boolean match(ETermPattern matcher, EObject[] r) {
		return matcher.match(this, r);
	}
	
	@Override
	public ETermPattern compileMatch(Set<Integer> out) {
		return EPattern.compilePattern(this, out);
	}

	public EString getName() {
		return EString.fromString(toString());
	}
	
	@Override
	int compare_same(EObject rhs) {
		return toString().compareTo(rhs.toString());
	}

	/**
	 * @param gl
	 */
	public abstract void set_group_leader(EPID gl);

	/**
	 * erlang:process_info/0
	 */
	public abstract EObject process_info();
	
	/**
	 * erlang:process_info/1
	 */
	public abstract EObject process_info(EObject spec);

	public static EPID read(EInputStream ei) {
		throw new NotImplemented();
	}

	/**
	 * @param node
	 * @param id
	 * @param serial
	 * @param creation
	 * @return
	 */
	public static EPID make(EAtom node, int id, int serial, int creation) {
		throw new NotImplemented();
	}

	/**
	 * @return
	 */
	public abstract boolean is_alive();

}
