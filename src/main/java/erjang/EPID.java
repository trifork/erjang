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

import java.io.IOException;
import java.util.Set;

import erjang.m.ets.EMatchContext;
import erjang.m.ets.EPattern;
import erjang.m.ets.ETermPattern;


public abstract class EPID extends EHandle {

	/**
	 * 
	 */
	public EPID(EAbstractNode node) {
		super(node);
	}
	
	@Override
	int cmp_order() {
		return CMP_ORDER_PID;
	}
	
	public EPID testPID() {
		return this;
	}
	
	@Override
	public String toString() {
		if (node()==ERT.getLocalNode().node) {
			return "<" + creation() + "." + serial() + "." + id() + ">";			
		} else {
			return "<" + node() + "." + creation() + "." + serial() + "." + id() + ">";
		}
	}

	public boolean match(ETermPattern matcher, EMatchContext r) {
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
		if (rhs == this) return 0;
		EPID op = (EPID) rhs;
		if (id() != op.id()) { if (id() < op.id()) return -1; return 1; } 
		if (serial() != op.serial()) { if (serial() < op.serial()) return -1; return 1; } 
		if (creation() != op.creation()) { if (creation() < op.creation()) return -1; return 1; } 
		return node().compareTo(op.node());
	}

	@Override
	public int hashCode() {
		return (100000007 * id() +
				200000033 * serial() +
				300000007 * creation() +
				400000009 * node().hashCode());
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

	public static EPID read(EInputStream ei) throws IOException {
		return ei.read_pid();
	}

	/**
	 * @param node
	 * @param id
	 * @param serial
	 * @param creation
	 * @return
	 */
	public static EPID make(EAtom node, int id, int serial, int creation) {
		if (node == ERT.getLocalNode().node) {

			EPID res = EProc.find(id, serial);
			if (res != null) return res;

			System.err.println("deadpid <"+id+"."+serial+"."+creation+">");
			// return DEADPID?
		}
		EAbstractNode peer = EPeer.get(node);

		if (peer instanceof EPeer) {
			return new EExternalPID((EPeer) peer, id, serial, creation);
		} else {
			System.err.println("localnode="+ERT.getLocalNode().node+"; asking="+node);
			// Presumably another node
			// might be local with different name
			return new EExternalPID(EPeer.get_or_create(node, creation, 0, 0), id, serial, creation);
		}
	}

	/**
	 * @return
	 */
	public abstract boolean is_alive();

	public void encode(EOutputStream eos) {
		eos.write_pid(node.node, id(), serial(), creation());
	}

	protected int serial() {
		throw new erjang.NotImplemented();
		
	}

	protected int id() {
		throw new erjang.NotImplemented();
		
	}

	protected int creation() {
		throw new erjang.NotImplemented();
		
	}

}
