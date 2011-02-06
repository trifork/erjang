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
import java.nio.ByteBuffer;
import java.util.List;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public final class ESmall extends EInteger {

	private static final Type ESMALL_TYPE = Type.getType(ESmall.class);
	public static final ESmall ZERO = new ESmall(0);
	public static final ESmall MINUS_ONE = new ESmall(-1);
	public static final ESmall ONE = new ESmall(1);
	public final int value;

	final static int PREALLOC_COUNT = 256; // Must be at least 256.
	static final ESmall[] little = new ESmall[PREALLOC_COUNT];
	static {
		for (int i = 0; i < little.length; i++) {
			little[i] = new ESmall(i);
		}
	}


	@Override
	public ESmall testSmall() {
		return this;
	}

	public ESmall(int value) {
		this.value = value;
	}
	
	@Override
	public int hashCode() {
		return value;
	}

	public long longValue() {
		return value;
	}
	
	public EInteger inc() { 
		if (value==Integer.MAX_VALUE) { 
			return ERT.box((long)Integer.MAX_VALUE + 1L); 
		} else {
			return new ESmall(value+1);
		}
	}
	
	public EInteger dec() { 		
		if (value==Integer.MIN_VALUE) { 
			return ERT.box((long)Integer.MIN_VALUE - 1L); 
		} else {
			return new ESmall(value-1);
		}
	}

	public boolean is_zero() { return value==0; }

	@Override
	public boolean collectIOList(List<ByteBuffer> out) {
		ByteBuffer b = ByteBuffer.allocate(1);
		b.put(0, (byte) value);
		out.add(b);
		return true;
	}
	
	public boolean equals(EObject other) {
		if (other == this) return true;
		if (other instanceof ESmall)
			return ((ESmall)other).value == value;
		if (other instanceof EDouble)
			return ((EDouble)other).value == value;
		return false;
	}
	
	@Override
	int compare_same(EObject rhs) {
		return rhs.r_compare_same(this);
	}

	int r_compare_same(ESmall lhs) {
		if (lhs.value < value)
			return -1;
		if (lhs.value == value) 
			return 0;
		return 1;
	}

	int r_compare_same(EBig lhs) {
		return lhs.value.compareTo(BigInteger.valueOf(value));
	}

	int r_compare_same(EDouble lhs) {
		return lhs.value < value ? -1 : lhs.value == value ? 0 : 1;
	}

	@Override
	public boolean equalsExactly(EObject rhs) {
		return rhs.r_equals_exactly(this);
	}

	boolean r_equals_exactly(ESmall lhs) {
		return lhs.value == value;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.EObject#asInt()
	 */
	@Override
	public int asInt() {
		return value;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof ESmall) {
			ESmall o = (ESmall) obj;
			return o.value == value;
		}
		return false;
	}

	public int intValue() {
		return value;
	}

	@Override
	public String toString() {
		return String.valueOf(value);
	}

	@Override
	public org.objectweb.asm.Type emit_const(MethodVisitor fa) {

		Type type = ESMALL_TYPE;

		fa.visitTypeInsn(Opcodes.NEW, type.getInternalName());
		fa.visitInsn(Opcodes.DUP);
		fa.visitLdcInsn(new Integer(value));
		fa.visitMethodInsn(Opcodes.INVOKESPECIAL, type.getInternalName(),
				"<init>", "(I)V");

		return type;
	}

	@Override
	public EInteger abs() {
		// OBS: abs(Integer.MIN_VALUE) cannot be represented by an ESmall.
		return ERT.box(Math.abs((long)value));
	}

	/**
	 * @param arity
	 * @return
	 */
	public static ESmall make(int v) {
		if (v >= 0 && v < PREALLOC_COUNT) return little[v];
		else return new ESmall(v);
	}

	//
	// Arithmetic
	//

	public double doubleValue() {
		return value;
	}

	public BigInteger bigintValue() {
		return BigInteger.valueOf(value);
	}

	public ENumber add(EObject other, boolean guard) {
		return other.add(value, guard);
	}

	@BIF(name="+")
	public ENumber add(EObject rhs) { 
		return rhs.add(value, false);
	}

	public EInteger add(int rhs, boolean guard) {
		return ERT.box((long) value + (long) rhs);
	}

	@BIF(name="+")
	public EInteger add(ESmall rhs) { 
		return ERT.box((long)value + (long)rhs.value); 
	}

	public ENumber add(double lhs, boolean guard) {
		return ERT.box(lhs + value);
	}

	public ENumber add(BigInteger lhs, boolean guard) {
		return ERT.box(lhs.add(BigInteger.valueOf(value)));
	}

	/* subtract */

	public ENumber subtract(EObject other, boolean guard) {
		return other.r_subtract(value, guard);
	}

	public ENumber subtract(ESmall rhs) { 
		return ERT.box((long)value - (long)rhs.value);
	}

	@Deprecated
	public ENumber subtract(int rhs) { 
			return ERT.box((long)value - rhs);
	}

	public ENumber r_subtract(int lhs, boolean guard) {
		return ERT.box((long) lhs - (long) value);
	}

	public ENumber r_subtract(double lhs, boolean guard) {
		return ERT.box(lhs - value);
	}

	public ENumber r_subtract(BigInteger lhs, boolean guard) {
		return ERT.box(lhs.subtract(BigInteger.valueOf(value)));
	}

	// integer division erlang:'*'/2

	@BIF(name="*")
	public ENumber multiply(EObject other) {
		return other.r_multiply(value);
	}

	public ENumber r_multiply(int lhs) {
		return ERT.box((long) lhs * (long) value);
	}

	public EDouble r_multiply(double lhs) {
		return ERT.box(lhs * value);
	}

	public ENumber r_multiply(BigInteger lhs) {
		return ERT.box(lhs.multiply(BigInteger.valueOf(value)));
	}

	@Override
	public ENumber negate() {
		return new ESmall(-value);
	}
	
	// integer division erlang:div/2

	public EDouble divide(EObject other) {
		return other.r_divide(value);
	}

	public EDouble r_divide(int lhs) {
		if (value==0) throw ERT.badarith(lhs, this);
		return ERT.box((double) lhs / value);
	}

	public EDouble r_divide(double lhs) {
		if (value==0) throw ERT.badarith(ERT.box(lhs), this);
		return ERT.box(lhs / value);
	}

	public EDouble r_divide(BigInteger lhs) {
		if (value==0) throw ERT.badarith(ERT.box(lhs), this);
		return ERT.box(lhs.doubleValue() / value);
	}

	// integer division erlang:div/2

	public EInteger idiv(EObject other) {
		return other.r_idiv(value);
	}

	public EInteger idiv(int rhs) {
		if (rhs==0) throw ERT.badarith(this, ERT.box(rhs));
		return ERT.box(value / rhs);
	}

	public EInteger r_idiv(int lhs) {
		if (value==0) throw ERT.badarith(ERT.box(lhs), this);
		return ERT.box((long) lhs / (long) value);
	}

	public EInteger r_idiv(BigInteger lhs) {
		if (value==0) throw ERT.badarith(lhs, this);
		return ERT.box(lhs.divide(BigInteger.valueOf(value)));
	}

	// remainder erlang:rem/2

	public ESmall irem(int rhs) {
		if (rhs==0) throw ERT.badarith(this, ERT.box(rhs));
		return ERT.box(value % rhs);
	}

	public EInteger irem(EObject other) {
		return other.r_irem(value);
	}

	public EInteger r_irem(int lhs) {
		if (value==0) throw ERT.badarith(ERT.box(lhs), this);
		return ERT.box(lhs % value);
	}

	public EInteger r_irem(BigInteger lhs) {
		if (value==0) throw ERT.badarith(ERT.box(lhs), this);
		return ERT.box(lhs.remainder(BigInteger.valueOf(value)));
	}

	// shift right erlang:shr/2

	public EInteger bsr(EObject other) {
		return other.r_bsr(value);
	}

	public EInteger r_bsr(int lhs) {
		
		/** it's a small positive integer in the range 0..31 */
		if ((value & ~31) == 0) 
			return ERT.box((lhs >> value));

		if (value >= 32) {
			if (lhs < 0)
				return ESmall.MINUS_ONE;
			else
				return ZERO;
		} else /* if (value < 0) */ {
			return ERT.box(BigInteger.valueOf(lhs).shiftRight(value));
		}
	}

	public EInteger r_bsr(BigInteger lhs) {
		return ERT.box(lhs.shiftRight(value));
	}

	// shift right erlang:shl/2

	public EInteger bsl(EObject other) {
		return other.r_bsl(value);
	}

	public EInteger r_bsl(int lhs) {
		if ((value & ~31) == 0) 
			return ERT.box(((long)lhs) << value);
		return ERT.box(BigInteger.valueOf(lhs).shiftLeft(value));
	}

	public EInteger r_bsl(BigInteger lhs) {
		return ERT.box(lhs.shiftLeft(value));
	}

	// binary and - erlang:band/2

	public EInteger band(EObject other) {
		return other.band(value);
	}

	public EInteger band(int lhs) {
		return ERT.box((lhs & value));
	}

	public EInteger band(BigInteger lhs) {
		return ERT.box(lhs.and(BigInteger.valueOf(value)));
	}

	// binary or - erlang:band/2

	public EInteger bor(EObject other) {
		return other.bor(value);
	}

	public EInteger bor(int lhs) {
		return ERT.box((lhs | value));
	}

	public EInteger bor(BigInteger lhs) {
		return ERT.box(lhs.or(BigInteger.valueOf(value)));
	}

	// binary xor - erlang:band/2

	public EInteger bxor(EObject other) {
		return other.bxor(value);
	}

	public EInteger bxor(int lhs) {
		return ERT.box((lhs ^ value));
	}

	public EInteger bxor(BigInteger lhs) {
		return ERT.box(lhs.xor(BigInteger.valueOf(value)));
	}

	public EInteger bnot() {
		return ERT.box(~value);
	}

	@Override
	public void encode(EOutputStream eos) {
		eos.write_int(value);
	}
}
