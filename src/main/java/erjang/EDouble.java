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


import java.math.BigDecimal;
import java.math.BigInteger;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class EDouble extends ENumber {

	private static final Type EDOUBLE_TYPE = Type.getType(EDouble.class);
	public final double value;
	
	public EDouble(double value) {
		this.value = value;
	}

	public EDouble testFloat() {
		return this;
	}

	@Override
	public int intValue() {
		return (int) value;
	}

	public static EObject parseDouble(String string) {
		return new EDouble(Double.parseDouble(string));
	}
	

	@Override
	public EDouble abs() {
		return new EDouble (Math.abs(value));
	}

	@Override
	public String toString() {
		return String.valueOf(value);
	}
	
	public EInteger asInteger() {
		if (value >= Integer.MIN_VALUE && value <= Integer.MAX_VALUE) {
			return new ESmall((int)value);
		} else {
			return new EBig(BigDecimal.valueOf(value).toBigInteger());
		}
	}



	@Override
	public org.objectweb.asm.Type emit_const(MethodVisitor fa) {

		Type type = EDOUBLE_TYPE;
		
		fa.visitTypeInsn(Opcodes.NEW, type.getInternalName());
		fa.visitInsn(Opcodes.DUP);
		fa.visitLdcInsn(new Double(value));
		fa.visitMethodInsn(Opcodes.INVOKESPECIAL, type.getInternalName(), "<init>", "(D)V");
		
		return type;		
	}

	/**
	 * @return
	 */
	public EString to_list() {
		return new EString(String.valueOf(value));
	}

	
	public ENumber add(EObject other) { return other.add(value); }

	public ENumber r_add(int lhs) { 
		return ERT.box((long)lhs + value);
	}

	public ENumber add(double lhs) { 
		return ERT.box(lhs + value);
	}

	public ENumber add(BigInteger lhs) { 
		return ERT.box(lhs.doubleValue() + value);
	}

	
	public ENumber subtract(EObject other) { return other.r_subtract(value); }

	public ENumber r_subtract(int lhs) { 
		return ERT.box((long)lhs - value);
	}

	public ENumber r_subtract(double lhs) { 
		return ERT.box(lhs - value);
	}

	public ENumber r_subtract(BigInteger lhs) { 
		return ERT.box(lhs.doubleValue() - value);
	}




	
	public ENumber multiply(EObject other) { return other.multiply(value); }

	public ENumber multiply(int lhs) { 
		return ERT.box(lhs * value);
	}

	public ENumber multiply(double lhs) { 
		return ERT.box(lhs * value);
	}

	public ENumber multiply(BigInteger lhs) { 
		return ERT.box(lhs.doubleValue() * value);
	}


	

	public EDouble divide(EObject other) { return other.r_divide(value); }
	
	public EDouble r_divide(int lhs) { 
		return ERT.box(lhs / value);
	}

	public EDouble r_divide(double lhs) { 
		return ERT.box(lhs / value);
	}

	public EDouble r_divide(BigInteger lhs) { 
		return ERT.box(lhs.doubleValue() / value);
	}
	
	@Override
	public double doubleValue() {
		return value;
	}

	/* (non-Javadoc)
	 * @see erjang.ENumber#add(int)
	 */
	@Override
	public ENumber add(int rhs) {
		return ERT.box(value + rhs);
	}

}
