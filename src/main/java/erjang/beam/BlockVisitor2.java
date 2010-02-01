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
import org.objectweb.asm.commons.Method;

/**
 * 
 */
public interface BlockVisitor2 extends BlockVisitor {
	void visitBegin(BeamExceptionHandler exh);

	void visitInsn(BeamOpcode insn);
	
	void visitInsn(BeamOpcode opcode, int failLabel, Arg arg1, Method bif);
	
	/** bif, gc_bif, arithfbif */
	void visitInsn(BeamOpcode opcode, int failLabel, Arg[] in, Arg out, BuiltInFunction bif);

	/**
	 * 
	 * @param test one of is_nil, is_nonempty_list
	 * @param failLabel
	 * @param arg1
	 * @param out 
	 */
	void visitTest(BeamOpcode test, int failLabel, Arg arg1, Type out);

	void visitInsn(BeamOpcode opcode, Arg arg);

	/**
	 * @param test
	 * @param failLabel
	 * @param args
	 * @param arg
	 * @param voidType
	 */
	void visitTest(BeamOpcode test, int failLabel, Arg[] args, Arg out,
			Type voidType);

	/**
	 * @param opcode
	 * @param arg1
	 * @param arg2
	 */
	void visitInsn(BeamOpcode opcode, Arg arg1, Arg arg2);

	/**
	 * @param fun
	 * @param args
	 * @param isTail
	 * @param isExternal
	 */
	void visitCall(ExtFunc fun, Arg[] args, boolean isTail, boolean isExternal);

	/**
	 * @param opcode
	 * @param ys
	 */
	void visitInsn(BeamOpcode opcode, Arg[] ys);

	/**
	 * @param opcode
	 * @param efun
	 * @param freevars
	 */
	void visitInsn(BeamOpcode opcode, ExtFunc efun, Arg[] freevars);

	/**
	 * @param opcode
	 * @param in
	 * @param out
	 */
	void visitInsn(BeamOpcode opcode, Arg[] in, Arg out);

	/**
	 * @param opcode
	 * @param asInt
	 * @param decodeOutArg
	 */
	void visitInsn(BeamOpcode opcode, int val, Arg out);

	/**
	 * @param opcode
	 * @param val
	 * @param tupleReg
	 * @param i
	 */
	void visitInsn(BeamOpcode opcode, Arg val, Arg out, int pos);

	/**
	 * @param in
	 * @param failLabel
	 * @param arities
	 * @param targets
	 */
	void visitSelectTuple(Arg in, int failLabel, int[] arities, int[] targets);

	/**
	 * @param in
	 * @param failLabel
	 * @param values
	 * @param targets
	 */
	void visitSelectValue(Arg in, int failLabel, Arg[] values, int[] targets);

	/**
	 * @param test
	 * @param failLabel
	 * @param arg
	 * @param arity
	 * @param tupleType
	 */
	void visitTest(BeamOpcode test, int failLabel, Arg arg, int arity,
			Type tupleType);

	/**
	 * @param decodeLabelref
	 */
	void visitJump(int decodeLabelref);

	/**
	 * @param opcode
	 * @param decodeLabelref
	 * @param decodeOutArg
	 */
	void visitReceive(BeamOpcode opcode, int block_label, Arg out);

	/**
	 * @param opcode
	 * @param failLabel
	 * @param in
	 * @param ex
	 */
	void visitInsn(BeamOpcode opcode, int failLabel, Arg[] in, Arg ex);

	/**
	 * @param test
	 * @param failLabel
	 * @param args
	 */
	void visitBitStringTest(BeamOpcode test, int failLabel, Arg[] args);

	/**
	 * @param size
	 * @param flags
	 * @param out
	 */
	void visitInitBitString(Arg size, Arg flags, Arg out);

	/**
	 * @param opcode
	 * @param elm
	 */
	void visitBitStringPut(BeamOpcode opcode, Arg value, Arg size, Arg flags);

	void visitBitStringAppend(BeamOpcode opcode, Arg extra_size, Arg src, Arg flags, Arg dst);

	/**
	 * @param opcode
	 * @param decodeArg
	 * @param arg2 
	 */
	void visitBS(BeamOpcode opcode, Arg arg1, Arg arg2);

	/**
	 * @param in
	 * @param out
	 */
	void visitBSAdd(Arg[] in, Arg out);


	void visitUnreachablePoint();
	void visitCatchBlockStart(BeamOpcode opcode, int label, Arg out, BeamExceptionHandler exh);
	void visitCatchBlockEnd(BeamOpcode opcode, Arg out, BeamExceptionHandler exh);
}
