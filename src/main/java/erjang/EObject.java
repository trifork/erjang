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
import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.Comparator;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Type;

import erjang.driver.EPortControl;
import erjang.m.ets.EMatchContext;
import erjang.m.ets.ETermPattern;
import erjang.m.java.JavaObject;

/** The base class for representing Erlang values.
 * This class embodíes in particular (the base of) the following concerns:
 * - Dynamic value kind testing;
 * - Erlang value ordering (incl. "compare-same") and "exact equality"
 *   (as used in matching).
 * - The unary and binary operators built into the Erlang language
 *
 * Furthermore, the class contains helper methods concerned with:
 * - building and flattening of lists;
 * - counting up and down, and integer zero-testing. (TODO: consider these)
 *
 * Regarding EObjects and collections:
 *
 * Erjang will need to form both hash tables and ordered collections
 * of EObjects.
 * For hashing purposes, EObject implements equals() and hashCode().
 * While EObjects have an ordering (the value ordering defined by Erlang),
 * it is not total: an integer and its floating-point equivalent are
 * considered equivalent as far as the ordering is concerned, though
 * not by equals().
 * EObject therefore <i>intentionally</i> does not implement Comparable;
 * instead, use the explicit comparator EObject.ERLANG_ORDERING.
 * (This is also the reason that its comparison method is called
 * erlangCompareTo() rather than plain compareTo().)
 */
public abstract class EObject {
	public static final ErlangOrderComparator ERLANG_ORDERING =
		new ErlangOrderComparator();

	public ECons cons(EObject h)
	{
		ESmall sm;
		if ((sm=h.testSmall()) != null && ((sm.value&0xff)==sm.value)) {
			return new EBinList((byte) sm.value, this);
		} else {
			return new EPair(h, this);
		}
	}
	
	public JavaObject testJavaObject() {
		return null;
	}

	public ERef testReference() {
		return null;
	}
	

	public EString testString()	{
		return null;
	}
	
	public EBigString testBigString()	{
		return null;
	}
	
	public EFun testFunction() {
		return null;
	}
	
	public EFun testFunction2(int nargs) {
		return null;
	}
	
	public EAtom testAtom() {
		return null;
	}

	public ECons testNonEmptyList() {
		return null;
	}
	
	public ETuple testTuple() {
		return null;
	}

	public ESeq testSeq() {
		return null;
	}

	public EPID testPID() {
		return null;
	}

	public int asInt() {
		throw new RuntimeException("cannot convert " + this + " to int");
	}

	public ENumber testNumber() {
		return null;
	}

	public ECons testCons() {
		return null;
	}

	public EInteger testInteger() {
		return null;
	}

	public ENil testNil() {
		return null;
	}
	
	public boolean isNil() { return testNil() != null; }
	public boolean isBoolean() { return this==ERT.TRUE || this==ERT.FALSE; }

	public EAtom testBoolean() {
		return null;
	}

	public EBinary testBinary() {
		return null;
	}


	/**
	 * @return this if this object is an instance of EPort, otherwise null
	 */
	public EPort testPort() {
		return null;
	}

	/**
	 * @return this if this object is an instance of ESmall, otherwise null
	 */
	public ESmall testSmall() {
		return null;
	}
	
	public EBig testBig() {
		return null;
	}
	

	/**
	 * @return
	 */
	public EDouble testFloat() {
		return null;
	}
	
	public boolean collectIOList(List<ByteBuffer> out) {
		return false;
	}

	/**
	 * @throws CharCollector.CollectingException when encountering
	 * something that can't be decoded as characters using the given
	 * CharCollector.  Exception contains the undecoded part of the input.
	 * @throws IOException when out.output throws IOException.
	 * @throws InvalidElementException when the input contains an object
	 * which is neither a list, an integer or a binary (without extra
	 * bits), or the input contains an integer in non-head position.
	 */
	public void collectCharList(CharCollector out)
		throws CharCollector.CollectingException,
		CharCollector.InvalidElementException,
		IOException
	{
		throw new CharCollector.InvalidElementException();
	}
	
	public Type emit_const(MethodVisitor mv) {
		throw new NotImplemented("emit_const for "+this.getClass().getName());
	}

	//
	// 
	//
	
	@BIF(name="-")
	public ENumber negate() { throw ERT.badarith(this); }

	@BIF(name="+")
	public ENumber add(EObject rhs) { return add(rhs, false); }

	@BIF(name="+")
	public ENumber add(ESmall rhs) { return add(rhs.value, false); }

	@BIF(name="+", type=BIF.Type.GUARD)
	public final ENumber add$g(ESmall rhs) { return add(rhs.value, true); }

	public ENumber add(EObject rhs, boolean guard) { if (guard) return null; throw ERT.badarith(this, rhs); }
	public ENumber add(int lhs, boolean guard) { if (guard) return null; throw ERT.badarith(lhs, this); }
	public ENumber add(double lhs, boolean guard) { if (guard) return null; throw ERT.badarith(lhs, this); }
	public ENumber add(BigInteger lhs, boolean guard) { if (guard) return null; throw ERT.badarith(lhs, this); }

	@BIF(name="-")
	public ENumber subtract(EObject rhs) { return subtract(rhs, false); }
	public ENumber subtract(ESmall rhs) { return subtract(rhs, false); }
	public ENumber subtract(EObject rhs, boolean guard) { if (guard) return null; throw ERT.badarith(this, rhs); }
	public ENumber subtract(int rhs) { throw ERT.badarith(this, rhs); }
	ENumber r_subtract(int lhs, boolean guard) { if (guard) return null; throw ERT.badarith(lhs, this); }
	ENumber r_subtract(double lhs, boolean guard) { if (guard) return null; throw ERT.badarith(lhs, this); }
	ENumber r_subtract(BigInteger lhs, boolean guard) { if (guard) return null; throw ERT.badarith(lhs, this); }
	
	@BIF(name="div")
	public EInteger idiv(EObject rhs) { throw ERT.badarith(this, rhs); }
	public EInteger idiv(int rhs) { throw ERT.badarith(this, rhs); }
	EInteger r_idiv(int lhs) { throw ERT.badarith(lhs, this); }
	EInteger r_idiv(BigInteger lhs) { throw ERT.badarith(lhs, this); }

	@BIF(name="rem")
	public EInteger irem(EObject rhs) { throw ERT.badarith(this, rhs); }
	EInteger r_irem(int lhs) { throw ERT.badarith(lhs, this); }
	EInteger r_irem(BigInteger lhs) { throw ERT.badarith(lhs, this); }

	@BIF(name="/")
	public EDouble divide(EObject rhs) { throw ERT.badarith(this, rhs); }
	EDouble r_divide(int lhs) { throw ERT.badarith(lhs, this); }
	EDouble r_divide(double lhs) { throw ERT.badarith(lhs, this); }
	EDouble r_divide(BigInteger lhs) { throw ERT.badarith(lhs, this); }

	@BIF(name="*")
	public ENumber multiply(EObject rhs) { throw ERT.badarith(this, rhs); }
	public ENumber r_multiply(int lhs) { throw ERT.badarith(new ESmall(lhs), this); }
	public EDouble r_multiply(double lhs) { throw ERT.badarith(new EDouble(lhs), this); }
	public ENumber r_multiply(BigInteger lhs) { throw ERT.badarith(new EBig(lhs), this); }

	@BIF(name="bsr")
	public EInteger bsr(EObject rhs) { throw ERT.badarith(this, rhs); }
	EInteger r_bsr(int lhs) { throw ERT.badarith(lhs, this); }
	EInteger r_bsr(BigInteger lhs) { throw ERT.badarith(lhs, this); }

	@BIF(name="bsl")
	public EInteger bsl(EObject rhs) { throw ERT.badarith(this, rhs); }
	EInteger r_bsl(int lhs) { throw ERT.badarith(lhs, this); }
	EInteger r_bsl(BigInteger lhs) { throw ERT.badarith(lhs, this); }
	
	@BIF(name="band")
	public EInteger band(EObject rhs) { throw ERT.badarith(this, rhs); }
	public EInteger band(int lhs) { throw ERT.badarith(lhs, this); }
	public EInteger band(BigInteger lhs) { throw ERT.badarith(lhs, this); }

	@BIF(name="bor")
	public EInteger bor(EObject rhs) { throw ERT.badarith(this, rhs); }
	public EInteger bor(int lhs) { throw ERT.badarith(lhs, this); }
	public EInteger bor(BigInteger lhs) { throw ERT.badarith(lhs, this); }

	@BIF(name="bxor")
	public EInteger bxor(EObject rhs) { throw ERT.badarith(this, rhs); }
	@BIF(name="bxor")
	public EInteger bxor(ESmall rhs) { return bxor(rhs.value); }

	public EInteger bxor(int lhs) { throw ERT.badarith(lhs, this); }
	public EInteger bxor(BigInteger lhs) { throw ERT.badarith(lhs, this); }

	@BIF(name="bnot")
	public EInteger bnot() { throw ERT.badarith(this); }

	// extra convenience
	
	public EDouble divide(double rhs) { throw ERT.badarith(this,rhs); }
	public EInteger irem(int rhs) { throw ERT.badarith(this,rhs); }

	@Override
	public abstract int hashCode();

	@Override
	public boolean equals(Object other) {
		if (other == this) return true;
		if (other instanceof EObject) {
			return equalsExactly((EObject) other);
		} else {
			return false;
		}
	}
	
	public boolean equals(EObject other) {
		if (other == this) return true;
		return equalsExactly(other);
	}

	public final int erlangCompareTo(EObject rhs) {
		if (rhs == this) return 0;
		int cmp1 = cmp_order();
		int cmp2 = rhs.cmp_order();
		if ( cmp1 == cmp2 ) {
			return compare_same(rhs);
		} else if (cmp1 < cmp2) {
			return -1;
		} else {
			return 1;
		}
	}

	/** Compare two objects that have same cmp_order */
	int compare_same(EObject rhs) { throw new Error("cannot compare"); }
	
	int r_compare_same(ESmall lhs) { throw new NotImplemented(); }
	int r_compare_same(EBig lhs) { throw new NotImplemented(); }
	int r_compare_same(EDouble lhs) { throw new NotImplemented(); }
	int r_compare_same(EInternalPID lhs) { throw new NotImplemented(); }
	
	/** Erlang "equals exactly", also called "matches" (=:= operator).
	 *  Is overridden by some subclasses, inclusing composites.
	 */
	public boolean equalsExactly(EObject rhs) {
		return erlangCompareTo(rhs) == 0;
	}

	/** Erlang "compares same" (== operator). */
	public final boolean erlangEquals(EObject rhs) {
		return erlangCompareTo(rhs) == 0;
	}

	boolean r_equals_exactly(ESmall lhs) { return false; }
	boolean r_equals_exactly(EBig lhs) { return false; }
	boolean r_equals_exactly(EDouble lhs) { return false; }
	
	
	/** used as compare-order for "non-erlang terms", such as 
	 *  compiled ets queries and the tail marker */
	public static final int CMP_ORDER_ERJANG_INTERNAL = -1;
	public static final int CMP_ORDER_NUMBER = 0;
	public static final int CMP_ORDER_ATOM = 1;
	public static final int CMP_ORDER_REFERENCE = 2;
	public static final int CMP_ORDER_FUN = 3;
	public static final int CMP_ORDER_PORT = 4;
	public static final int CMP_ORDER_PID = 5;
	public static final int CMP_ORDER_TUPLE = 6;
	public static final int CMP_ORDER_LIST = 7;
	public static final int CMP_ORDER_BITSTRING = 8;
	
	/** 
	 * 	number[0] < atom[1] < reference[2] < fun[3] < port[4] < pid[5] < tuple[6] < list[7] < bit string[8]
	 * @return
	 */
	int cmp_order() { throw new Error("cannot compare"); }


	@BIF
	public EAtom is_function(EObject arity) {
		return ERT.FALSE;
	}
	
	/**
	 * @param o2
	 * @return
	 */
	public EAtom ge(EObject o2) {
		return ERT.box ( this.erlangCompareTo(o2) >= 0 );
	}


	/**
	 * @return
	 */
	public EBitString testBitString() {
		return null;
	}

	/**
	 * @return non-null if this is an internal port
	 */
	public EInternalPort testInternalPort() {
		return null;
	}

	/**
	 * @return
	 */
	public EPortControl testPortControl() {
		return null;
	}

	/**
	 * @return
	 */
	public EHandle testHandle() {
		return null;
	}

	/**
	 * @return
	 */
	public EInternalPID testInternalPID() {
		return null;
	}

	/**
	 * @return true if this term matches the given matcher
	 */
	public boolean match(ETermPattern matcher, EMatchContext r) {
		return false;
	}

	/**
	 * @param out
	 * @return
	 */
	public ETermPattern compileMatch(Set<Integer> out) {
		// this should continue to be "not implemented".  
		// subclasses should provide an implementation.
		throw ERT.badarg(this);
	}

	public EBinMatchState testBinMatchState() {
		return null;
	}

	public void encode(EOutputStream eos) {
		throw new NotImplemented("Encode for "+getClass().getName());
	}

	final public boolean is_eq(EObject other) {
		return erlangEquals(other);
	}
	
	final public boolean is_eq_exact(EObject other) {
		return equalsExactly(other);
	}
	
	final public boolean is_ne(EObject other) {
		return !erlangEquals(other);
	}
	
	final public boolean is_ne_exact(EObject other) {
		return !equalsExactly(other);
	}
	
	final public boolean is_lt(EObject other) {
		return this.erlangCompareTo(other) < 0;
	}
	
	final public boolean is_ge(EObject other) {
		return this.erlangCompareTo(other) >= 0;
	}
	
	/**
	 * @param c1
	 * @return
	 */
	public EObject prepend(ESeq list) {
		
		// first, rlist=lists:reverse(list)
		ESeq rlist = ERT.NIL;
		while (!list.isNil()) {
			rlist = rlist.cons(list.head());
			list = list.tail();
		}

		// then, prepend rlist on this
		EObject r = this;
		while(!rlist.isNil()) {
			r = r.cons(rlist.head());
			rlist = rlist.tail();
		} 
		
		return r;
	}

	public ENumber inc() { return ESmall.ONE.add(this); }
	public ENumber dec() { return ESmall.MINUS_ONE.add(this); }	
	public boolean is_zero() { return false; }


	public static abstract class ValueComparator implements Comparator<EObject> {
		public abstract int compare(EObject a, EObject b);
	}

	/** Comparator which doesn't distibguish between integers and floats.
	 * Note: this comparator imposes orderings that are inconsistent with equals.
	 */
	private static class ErlangOrderComparator extends ValueComparator {
		public int compare(EObject a, EObject b) {
			return a.erlangCompareTo(b);
		}
	}
}
