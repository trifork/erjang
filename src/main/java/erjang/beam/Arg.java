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


package erjang.beam;

import org.objectweb.asm.Type;

import erjang.EObject;

/**
 * 
 */
public class Arg {

	public enum Kind {
		X, Y, F, IMMEDIATE, EXT_FUNC;

		public boolean isReg() {
			return this == X || this == Y || this == F;			
		}
	}

	public static final Arg[] NO_ARGS = new Arg[0];

	public final Kind kind;
	int no;
	public Type type;
	EObject value;
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return kind.name() + "{#" + no +", val="+String.valueOf(value)+", type="+String.valueOf(type)+"}"; 
	}
	
	public Arg(Kind kind, int reg) {
		this(kind, reg, kind==Kind.F ? Type.DOUBLE_TYPE : null);
	}

	public Arg(Kind kind, int reg, Type type) {
		if (kind == Kind.IMMEDIATE) { throw new IllegalArgumentException(); }
		this.kind = kind;
		this.no = reg;
		this.type = type;
	}

	public Arg(EObject value) {
		this.kind = Kind.IMMEDIATE;
		this.value = value;
		this.type = Type.getType(value.getClass());
	}
	
	public Arg(EObject value, Type type) {
		this.kind = Kind.IMMEDIATE;
		this.value = value;
		this.type = type;
	}

	/**
	 * @param arg1
	 * @param out
	 */
	public Arg(Arg arg1, Type type) {
		this.kind = arg1.kind;
		this.no = arg1.no;
		this.value = arg1.value;
		this.type = type;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Arg) {
			Arg arg = (Arg) obj;
			return kind == arg.kind && no == arg.no && value == arg.value;
		}
		return false;
	}
}
