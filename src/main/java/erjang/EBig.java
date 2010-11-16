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

import java.math.BigInteger;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public final class EBig extends EInteger {

	private static final BigInteger BIG_32 = BigInteger.valueOf(32);
	private static final BigInteger BIG_MAX_INT = BigInteger.valueOf(Integer.MAX_VALUE);
	private static final BigInteger BIG_MIN_INT = BigInteger.valueOf(Integer.MIN_VALUE);

	final BigInteger value;

	public EBig(BigInteger value) {
		this.value = value;
		
		assert (value.compareTo(BIG_MAX_INT) > 0 || value.compareTo(BIG_MIN_INT) < 0);
	}
	
	public EInteger dec() { 
		return (EInteger) ESmall.MINUS_ONE.add(this); 
	}	

	public EBig testBig() {
		return this;
	}

	@Override
	int compare_same(EObject rhs) {
		return rhs.r_compare_same(this);
	}
	
	int r_compare_same(ESmall lhs) {
		return -value.signum();
	}

	int r_compare_same(EBig lhs) {
		return lhs.value.compareTo(value);
	}

	int r_compare_same(EDouble lhs) {
		double doubleValue = value.doubleValue();
		return lhs.value < doubleValue ? -1 : lhs.value == doubleValue ? 0 : 1;
	}

	@Override
	public boolean equalsExactly(EObject rhs) {
		return rhs.r_equals_exactly(this);
	}
	
	boolean r_equals_exactly(EBig lhs) {
		return lhs.value.equals(value);
	}


	
	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return value.hashCode();
	}

	public EBig(long res) {
		this.value = BigInteger.valueOf(res);
	}

	@Override
	public int intValue() {
		return value.intValue();
	}

	@Override
	public String toString() {
		return value.toString();
	}

	public static EBig fromString(String value) {
		return new EBig(new BigInteger(value));
	}

	private static final Type EBIG_TYPE = Type.getType(EBig.class);
	private static final Type STRING_TYPE = Type.getType(String.class);

	@Override
	public org.objectweb.asm.Type emit_const(MethodVisitor fa) {

		Type type = EBIG_TYPE;

		fa.visitLdcInsn(value.toString());
		fa.visitMethodInsn(Opcodes.INVOKESTATIC, type.getInternalName(),
				"fromString", "(" + STRING_TYPE.getDescriptor() + ")"
						+ type.getDescriptor());

		return type;
	}

	@Override
	public EBig abs() {
		return new EBig(value.abs());
	}

	public ENumber add(EObject other, boolean guard) {
		return other.add(value, guard);
	}

	public ENumber add(int lhs, boolean guard) {
		return ERT.box(BigInteger.valueOf(lhs).add(value));
	}

	public ENumber add(double lhs, boolean guard) {
		return ERT.box(lhs + value.doubleValue());
	}

	public ENumber add(BigInteger lhs, boolean guard) {
		return ERT.box(lhs.add(value));
	}

	public EInteger subtract(int rhs) {  
		return ERT.box(value.subtract(BigInteger.valueOf(rhs)));
	}

	public ENumber subtract(EObject other, boolean guard) {
		return other.r_subtract(value, guard);
	}

	ENumber r_subtract(int lhs, boolean guard) {
		return ERT.box(BigInteger.valueOf(lhs).subtract(value));
	}

	ENumber r_subtract(double lhs, boolean guard) {
		return ERT.box(lhs - value.doubleValue());
	}

	ENumber r_subtract(BigInteger lhs, boolean guard) {
		return ERT.box(lhs.subtract(value));
	}

	public EInteger idiv(EObject other) {
		return other.r_idiv(value);
	}

	public EInteger idiv(int rhs) { 
		return ERT.box(value.divide(BigInteger.valueOf(rhs)));
	}


	EInteger r_idiv(int lhs) {
		return ERT.box(BigInteger.valueOf(lhs).divide(value));
	}

	EInteger r_idiv(BigInteger lhs) {
		return ERT.box(lhs.divide(value));
	}

	public EInteger irem(EObject other) {
		return other.r_irem(value);
	}

	EInteger r_irem(int lhs) {
		return ERT.box(BigInteger.valueOf(lhs).remainder(value));
	}

	EInteger r_irem(BigInteger lhs) {
		return ERT.box(lhs.remainder(value));
	}

	public EDouble divide(EObject other) {
		return other.r_divide(value);
	}

	EDouble r_divide(int lhs) {
		return ERT.box(lhs / value.doubleValue());
	}

	EDouble r_divide(double lhs) {
		return ERT.box(lhs / value.doubleValue());
	}

	EDouble r_divide(BigInteger lhs) {
		return ERT.box(lhs.doubleValue() / value.doubleValue());
	}

	public ENumber multiply(EObject other) {
		return other.r_multiply(value);
	}

	public ENumber r_multiply(int lhs) {
		return ERT.box(BigInteger.valueOf(lhs).multiply(value));
	}

	public EDouble r_multiply(double lhs) {
		return ERT.box(lhs * value.doubleValue());
	}
	
	public ENumber r_multiply(BigInteger lhs) {
		return ERT.box(lhs.multiply(value));
	}

	public EInteger bsr(EObject other) {
		return other.r_bsr(value);
	}

	EInteger r_bsr(int lhs) {
		if (BIG_32.compareTo(value) <= 0) {
			if (lhs < 0)
				return ESmall.MINUS_ONE;
			else
				return ESmall.ZERO;
		} else {
			return ERT.box(lhs >> value.intValue());
		}
	}

	EInteger r_bsr(BigInteger lhs) {
		if (BIG_MAX_INT.compareTo(value) < 0) {
			if (BigInteger.ZERO.compareTo(lhs) > 0)
				return ESmall.MINUS_ONE;
			else
				return ESmall.ZERO;
		} else {
			return ERT.box(lhs.shiftRight(value.intValue()));
		}
	}

	public EInteger bsl(EObject other) {
		return other.r_bsl(value);
	}

	EInteger r_bsl(int lhs) {
		return ERT.box(BigInteger.valueOf(lhs).shiftLeft(value.intValue()));
	}

	EInteger r_bsl(BigInteger lhs) {
		return ERT.box(lhs.shiftLeft(value.intValue()));
	}

	// binary and

	public EInteger band(EObject other) {
		return other.band(value);
	}

	public EInteger band(int lhs) {
		return ERT.box(BigInteger.valueOf(lhs).and(value));
	}

	public EInteger band(BigInteger lhs) {
		return ERT.box(lhs.and(value));
	}

	// binary or

	public EInteger bor(EObject other) {
		return other.bor(value);
	}

	public EInteger bor(int lhs) {
		return ERT.box(BigInteger.valueOf(lhs).or(value));
	}

	public EInteger bor(BigInteger lhs) {
		return ERT.box(lhs.or(value));
	}

	// binary xor

	public EInteger bxor(EObject other) {
		return other.bxor(value);
	}

	public EInteger bxor(int lhs) {
		return ERT.box(BigInteger.valueOf(lhs).xor(value));
	}

	public EInteger bxor(BigInteger lhs) {
		return ERT.box(lhs.xor(value));
	}

	public EInteger bnot() {
		return ERT.box(value.not());
	}


	@Override
	public ENumber negate() {
               return ERT.box(value.negate());
	}
	

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.EInteger#bigintValue()
	 */
	@Override
	BigInteger bigintValue() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.ENumber#doubleValue()
	 */
	@Override
	public double doubleValue() {
		return value.doubleValue();
	}

	@Override
	public void encode(EOutputStream eos) {
		eos.write_big_integer(value);
	}

}
