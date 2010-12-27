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
import java.util.Formatter;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class EDouble extends ENumber {

	private static final Type EDOUBLE_TYPE = Type.getType(EDouble.class);
	public final double value;

	public EDouble(double value) {
		this.value = value;
	}

	@Override
	public int hashCode() {
		long bits = Double.doubleToLongBits(value);
		return (int)(bits ^ (bits >>> 32));
	}

	@Override
	int compare_same(EObject rhs) {
		return rhs.r_compare_same(this);
	}

	int r_compare_same(ESmall lhs) {
		return lhs.value < value ? -1 : lhs.value == value ? 0 : 1;
	}

	int r_compare_same(EBig lhs) {
		double doubleValue = lhs.doubleValue();
		return doubleValue < value ? -1 : doubleValue == value ? 0 : 1;
	}

	int r_compare_same(EDouble lhs) {
		return lhs.value < value ? -1 : lhs.value == value ? 0 : 1;
	}

	public boolean equals(Object other) {
		if (other instanceof EDouble) {
			EDouble o = (EDouble) other;
			return o.value == value;
		}
		return false;
	}
	
	@Override
	public boolean equalsExactly(EObject rhs) {
		return rhs.r_equals_exactly(this);
	}

	boolean r_equals_exactly(EDouble lhs) {
		return lhs.value == value;
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
		return new EDouble(Math.abs(value));
	}

	@Override
	public String toString() {
		return String.valueOf(value);
	}

	public EInteger asInteger() {
		if (value >= Integer.MIN_VALUE && value <= Integer.MAX_VALUE) {
			return new ESmall((int) value);
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
		fa.visitMethodInsn(Opcodes.INVOKESPECIAL, type.getInternalName(),
				"<init>", "(D)V");

		return type;
	}

	
	/**
	 * @return
	 */
	public EString to_list() {
		Formatter form = new Formatter();
		form = form.format("%.20e", value);
		String value = form.toString();
		return new EString(value);
	}

	public ENumber add(EObject other, boolean guard) {
		return other.add(value, guard);
	}

	public ENumber r_add(int lhs) {
		return ERT.box((long) lhs + value);
	}

	public ENumber add(double lhs, boolean guard) {
		return ERT.box(lhs + value);
	}

	public ENumber add(BigInteger lhs, boolean guard) {
		return ERT.box(lhs.doubleValue() + value);
	}

	@Override
	public ENumber negate() {
		return new EDouble(-value);
	}

	public EDouble subtract(int rhs) {  
		return ERT.box(value - rhs);
	}

	public ENumber subtract(EObject other, boolean guard) {
		return other.r_subtract(value, guard);
	}

	public ENumber r_subtract(int lhs, boolean guard) {
		return ERT.box((long) lhs - value);
	}

	public ENumber r_subtract(double lhs, boolean guard) {
		return ERT.box(lhs - value);
	}

	public ENumber r_subtract(BigInteger lhs, boolean guard) {
		return ERT.box(lhs.doubleValue() - value);
	}

	public EDouble multiply(EObject other) {
		return other.r_multiply(value);
	}

	public ENumber r_multiply(int lhs) {
		return ERT.box(lhs * value);
	}

	public EDouble r_multiply(double lhs) {
		return ERT.box(lhs * value);
	}

	public ENumber r_multiply(BigInteger lhs) {
		return ERT.box(lhs.doubleValue() * value);
	}

	public EDouble divide(EObject other) {
		return other.r_divide(value);
	}

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

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.ENumber#add(int)
	 */
	@Override
	public ENumber add(int rhs, boolean guard) {
		return ERT.box(value + rhs);
	}

	public static EDouble read(EInputStream ei) throws java.io.IOException {
		return new EDouble(ei.read_double());
	}

	@Override
	public void encode(EOutputStream eos) {
		eos.write_double(value);
	}


}
