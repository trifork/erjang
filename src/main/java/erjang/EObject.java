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
import org.objectweb.asm.Type;

public class EObject {

	public ECons cons(EObject h)
	{
		return new EPair(h, this);
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


	/**
	 * @return
	 */
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
	

	/**
	 * @return
	 */
	public EDouble testFloat() {
		return null;
	}
	
	
	public Type emit_const(MethodVisitor mv) {
		throw new NotImplemented();
	}

	//
	// 
	//
	
	@BIF(name="-")
	public ENumber negate() { throw ERT.badarg(this); }

	@BIF(name="+")
	public ENumber add(EObject rhs) { throw ERT.badarg(this, rhs); }
	public ENumber add(int lhs) { throw ERT.badarg(lhs, this); }
	public ENumber add(double lhs) { throw ERT.badarg(lhs, this); }
	public ENumber add(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	@BIF(name="-")
	public ENumber subtract(EObject rhs) { throw ERT.badarg(this, rhs); }
	public ENumber subtract(int rhs) { throw ERT.badarg(this, rhs); }
	ENumber r_subtract(int lhs) { throw ERT.badarg(lhs, this); }
	ENumber r_subtract(double lhs) { throw ERT.badarg(lhs, this); }
	ENumber r_subtract(BigInteger lhs) { throw ERT.badarg(lhs, this); }
	
	@BIF(name="div")
	public EInteger idiv(EObject rhs) { throw ERT.badarg(this, rhs); }
	public EInteger idiv(int rhs) { throw ERT.badarg(this, rhs); }
	EInteger r_idiv(int lhs) { throw ERT.badarg(lhs, this); }
	EInteger r_idiv(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	@BIF(name="rem")
	public EInteger irem(EObject rhs) { throw ERT.badarg(this, rhs); }
	EInteger r_irem(int lhs) { throw ERT.badarg(lhs, this); }
	EInteger r_irem(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	@BIF(name="/")
	public EDouble divide(EObject rhs) { throw ERT.badarg(this, rhs); }
	EDouble r_divide(int lhs) { throw ERT.badarg(lhs, this); }
	EDouble r_divide(double lhs) { throw ERT.badarg(lhs, this); }
	EDouble r_divide(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	@BIF(name="*")
	public ENumber multiply(EObject rhs) { throw ERT.badarg(this, rhs); }
	public ENumber multiply(int lhs) { throw ERT.badarg(lhs, this); }
	public ENumber multiply(double lhs) { throw ERT.badarg(lhs, this); }
	public ENumber multiply(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	@BIF(name="bsr")
	public EInteger bsr(EObject rhs) { throw ERT.badarg(this, rhs); }
	EInteger r_bsr(int lhs) { throw ERT.badarg(lhs, this); }
	EInteger r_bsr(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	@BIF(name="bsl")
	public EInteger bsl(EObject rhs) { throw ERT.badarg(this, rhs); }
	EInteger r_bsl(int lhs) { throw ERT.badarg(lhs, this); }
	EInteger r_bsl(BigInteger lhs) { throw ERT.badarg(lhs, this); }
	
	@BIF(name="band")
	public EInteger band(EObject rhs) { throw ERT.badarg(this, rhs); }
	public EInteger band(int lhs) { throw ERT.badarg(lhs, this); }
	public EInteger band(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	@BIF(name="bor")
	public EInteger bor(EObject rhs) { throw ERT.badarg(this, rhs); }
	public EInteger bor(int lhs) { throw ERT.badarg(lhs, this); }
	public EInteger bor(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	@BIF(name="bxor")
	public EInteger bxor(EObject rhs) { throw ERT.badarg(this, rhs); }
	public EInteger bxor(int lhs) { throw ERT.badarg(lhs, this); }
	public EInteger bxor(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	@BIF(name="bnor")
	public EInteger bnot() { throw ERT.badarg(this); }

	// extra convenience
	
	public EDouble divide(double rhs) { throw ERT.badarg(this,rhs); }
	public EInteger irem(int rhs) { throw ERT.badarg(this,rhs); }




}
