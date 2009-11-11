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

import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;


import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import erjang.jbeam.ops.CodeAdapter;

public class EAtom extends ETerm implements CharSequence {

	public EAtom asAtom() {
		return this;
	}

	private final String value;

	private static ConcurrentHashMap<String, EAtom> interns = new ConcurrentHashMap<String, EAtom>();

	private EAtom(String name) {
		this.value = name;
	}

	Pattern ATOM = Pattern.compile("[a-z_]([a-z]|[A-Z]|@|_|[0-9])*");
	
	@Override
	public String toString() {
		if (ATOM.matcher(value).matches()) {
			return value;
		} else {
			return "'" + value + "'";
		}
	}
	
	public boolean isAtom(String name)
	{
		return interns.contains(name);
	}
	
	@Override
	public boolean equals(Object obj) {
		return this == obj;
	}
 
	@Override
	public int hashCode() {
		return value.hashCode();
	}
	
	public static EAtom intern(String name) {

		EAtom res = interns.get(name);
		if (res == null) {
			EAtom new_val = new EAtom(name);
			do {
				res = interns.putIfAbsent(name, new_val);
			} while (res == null);
		}

		return res;
	}



	@Override
	public char charAt(int index) {
		return value.charAt(index);
	}

	@Override
	public int length() {
		return value.length();
	}

	@Override
	public CharSequence subSequence(int start, int end) {
		return value.subSequence(start, end);
	}

	public String getName() {
		return value;
	}


	private static final Type EATOM_TYPE = Type.getType(EAtom.class);
	private static final Type STRING_TYPE = Type.getType(String.class);

	@Override
	public org.objectweb.asm.Type emit_const(CodeAdapter fa) {

		Type type = EATOM_TYPE;
		
		fa.visitLdcInsn(value);
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, type.getInternalName(),
					"intern", "(" + STRING_TYPE.getDescriptor() + ")" + type.getDescriptor());
		
		return type;	
	}


}
