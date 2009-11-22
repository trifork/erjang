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

	public ESmall testInteger() {
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
	 * @return
	 */
	public EPort testPort() {
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
	
	public ENumber negate(EObject other) { throw ERT.badarg(this, other); }

	public ENumber add(EObject other) { throw ERT.badarg(this, other); }
	public ENumber add(int lhs) { throw ERT.badarg(lhs, this); }
	public ENumber add(double lhs) { throw ERT.badarg(lhs, this); }
	public ENumber add(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	public ENumber subtract(EObject other) { throw ERT.badarg(this, other); }
	public ENumber r_subtract(int lhs) { throw ERT.badarg(lhs, this); }
	public ENumber r_subtract(double lhs) { throw ERT.badarg(lhs, this); }
	public ENumber r_subtract(BigInteger lhs) { throw ERT.badarg(lhs, this); }
	
	public EInteger idiv(EObject other) { throw ERT.badarg(this, other); }
	public EInteger r_idiv(int lhs) { throw ERT.badarg(lhs, this); }
	public EInteger r_idiv(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	public EInteger irem(EObject other) { throw ERT.badarg(this, other); }
	public EInteger r_irem(int lhs) { throw ERT.badarg(lhs, this); }
	public EInteger r_irem(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	public EDouble divide(EObject other) { throw ERT.badarg(this, other); }
	public EDouble r_divide(int lhs) { throw ERT.badarg(lhs, this); }
	public EDouble r_divide(double lhs) { throw ERT.badarg(lhs, this); }
	public EDouble r_divide(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	public ENumber multiply(EObject other) { throw ERT.badarg(this, other); }
	public ENumber multiply(int lhs) { throw ERT.badarg(lhs, this); }
	public ENumber multiply(double lhs) { throw ERT.badarg(lhs, this); }
	public ENumber multiply(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	public EInteger bsr(EObject other) { throw ERT.badarg(this, other); }
	public EInteger r_bsr(int lhs) { throw ERT.badarg(lhs, this); }
	public EInteger r_bsr(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	public EInteger bsl(EObject other) { throw ERT.badarg(this, other); }
	public EInteger r_bsl(int lhs) { throw ERT.badarg(lhs, this); }
	public EInteger r_bsl(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	
	public EInteger band(EObject other) { throw ERT.badarg(this, other); }
	public EInteger band(int lhs) { throw ERT.badarg(lhs, this); }
	public EInteger band(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	public EInteger bor(EObject other) { throw ERT.badarg(this, other); }
	public EInteger bor(int lhs) { throw ERT.badarg(lhs, this); }
	public EInteger bor(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	public EInteger bxor(EObject other) { throw ERT.badarg(this, other); }
	public EInteger bxor(int lhs) { throw ERT.badarg(lhs, this); }
	public EInteger bxor(BigInteger lhs) { throw ERT.badarg(lhs, this); }

	public EInteger bnot() { throw ERT.badarg(this); }




}
