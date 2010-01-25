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

package erjang.beam.analysis;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import org.objectweb.asm.Type;

import erjang.EAtom;
import erjang.EBig;
import erjang.EBinMatchState;
import erjang.EBinary;
import erjang.EBitString;
import erjang.ECons;
import erjang.EDouble;
import erjang.EFun;
import erjang.EInteger;
import erjang.EList;
import erjang.ENil;
import erjang.ENumber;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.ERT;
import erjang.ERef;
import erjang.ESeq;
import erjang.ESmall;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.beam.Arg;
import erjang.beam.BIFUtil;
import erjang.beam.BeamCodeBlock;
import erjang.beam.BeamFunction;
import erjang.beam.BeamInstruction;
import erjang.beam.BeamOpcode;
import erjang.beam.BeamExceptionHandler;
import erjang.beam.BlockVisitor;
import erjang.beam.BlockVisitor2;
import erjang.beam.BuiltInFunction;
import erjang.beam.ExtFunc;
import erjang.beam.FunctionAdapter;
import erjang.beam.FunctionVisitor;
import erjang.beam.FunctionVisitor2;
import erjang.beam.ModuleAdapter;
import erjang.beam.ModuleVisitor;
import erjang.beam.Arg.Kind;

public class BeamTypeAnalysis extends ModuleAdapter {
	/**
	 * 
	 */
	public BeamTypeAnalysis(ModuleVisitor mv) {
		super(mv);
	}

	static final Type ESMALL_TYPE = Type.getType(ESmall.class);
	static final Type EBIG_TYPE = Type.getType(EBig.class);
	static final Type EINTEGER_TYPE = Type.getType(EInteger.class);
	static final Type ENUMBER_TYPE = Type.getType(ENumber.class);
	static final Type EOBJECT_TYPE = Type.getType(EObject.class);
	static final Type EDOUBLE_TYPE = Type.getType(EDouble.class);
	static final Type ENIL_TYPE = Type.getType(ENil.class);
	static final Type EATOM_TYPE = Type.getType(EAtom.class);
	static final Type ETUPLE_TYPE = Type.getType(ETuple.class);
	static final Type EBINARY_TYPE = Type.getType(EBinary.class);
	static final Type EBITSTRING_TYPE = Type.getType(EBitString.class);
	static final Type ECONS_TYPE = Type.getType(ECons.class);
	static final Type ESEQ_TYPE = Type.getType(ESeq.class);
	static final Type ELIST_TYPE = Type.getType(EList.class);
	static final Type EFUN_TYPE = Type.getType(EFun.class);
	static final Type EPID_TYPE = Type.getType(EPID.class);
	static final Type EPORT_TYPE = Type.getType(EPort.class);
	static final Type EREFERENCE_TYPE = Type.getType(ERef.class);
	static final Type EMATCHSTATE_TYPE = Type.getType(EBinMatchState.class);

	static final EObject X_ATOM = EAtom.intern("x");
	static final EObject Y_ATOM = EAtom.intern("y");
	static final EObject FR_ATOM = EAtom.intern("fr");
	static final EObject NIL_ATOM = EAtom.intern("nil");
	static final EObject INTEGER_ATOM = EAtom.intern("integer");
	static final EObject STRING_ATOM = EAtom.intern("string");
	static final EObject FLOAT_ATOM = EAtom.intern("float");
	static final EObject ATOM_ATOM = EAtom.intern("atom");
	static final EObject LITERAL_ATOM = EAtom.intern("literal");
	static final EObject NOFAIL_ATOM = EAtom.intern("nofail");
	static final EObject F_ATOM = EAtom.intern("f");
	static final EObject FIELD_FLAGS_ATOM = EAtom.intern("field_flags");
	static final EObject EXTFUNC_ATOM = EAtom.intern("extfunc");
	static final EObject APPLY_ATOM = EAtom.intern("apply");
	static final EObject ERLANG_ATOM = EAtom.intern("erlang");
	static final EObject ERROR_ATOM = EAtom.intern("error");

	private static final ETuple X0_REG = ETuple.make(new EObject[] { X_ATOM,
			new ESmall(0) });
	private EAtom moduleName;

	private List<FV> functions = new ArrayList<FV>();

	@Override
	public FunctionVisitor visitFunction(EAtom name, int arity, int startLabel) {
		FV f = new FV(super.visitFunction(name, arity, startLabel), name,
				arity, startLabel);
		functions.add(f);
		return f;
	}

	class FV extends FunctionAdapter implements BeamFunction {

		BasicBlock makeBasicBlock(int label, int index) {
			assert ((label & 0xffff) == label);
			assert ((index & 0xffff) == index);

			int bbk = (label << 16) | index;
			BasicBlock bb = bbs.get(bbk);
			if (bb == null) {
				bbs.put(bbk, bb = new BasicBlock(label, index));
			}
			return bb;
		}

		TreeMap<Integer, BasicBlock> bbs = new TreeMap<Integer, BasicBlock>();
		/** Maps from label to list of exception handler labels. */
		TreeMap<Integer, List<BeamExceptionHandler>> blocks_with_ambiguous_exh = new TreeMap<Integer, List<BeamExceptionHandler>>();

		void live_analysis() {

			Integer[] all = bbs.keySet().toArray(new Integer[bbs.size()]);
			Arrays.sort(all);

			boolean change = false;

			int iter = 0;
			do {
				iter += 1;
				for (int n = all.length - 1; n >= 0; n--) {

					BasicBlock bb = bbs.get(all[n]);

					TreeSet<Integer> inq = bb.in;
					TreeSet<Integer> outq = bb.out;

					TreeSet<Integer> in_n = new TreeSet<Integer>();
					in_n.addAll(outq);
					in_n.removeAll(bb.kill);
					in_n.addAll(bb.use);
					bb.in = in_n;

					TreeSet<Integer> out_n = new TreeSet<Integer>();
					for (BasicBlock s : bb.succ) {
						TreeSet<Integer> in_s = s.in;
						if (in_s != null) {
							out_n.addAll(in_s);
						}
					}
					bb.out = out_n;

					change = (!inq.equals(in_n)) || (!outq.equals(out_n));

				}
			} while (change);

			// System.err.println("live analysis for " + name + "/" + arity
			// + " completed in " + iter + " iterations.");
		}

		public TypeMap getTypeMap(int i) {
			int label = i >> 16;
			int insn = i & 0xffff;

			LabeledBlock lbb = lbs.get(label);
			TypeMap res = lbb.map[i];

			return res;

		}

		Map<Integer, LabeledBlock> lbs = new TreeMap<Integer, LabeledBlock>();

		private final EAtom name;
		private final int arity;
		private final int startLabel;

		private SortedSet<LabeledBlock> needs_analyze = new TreeSet<LabeledBlock>(
				new Comparator<LabeledBlock>() {
					@Override
					public int compare(LabeledBlock o1, LabeledBlock o2) {
						return o2.block_label - o1.block_label;
					}
				});

		public int max_stack;

		public int max_xreg;

		public int max_freg;

		private boolean is_tail_recursive;

		public FV(FunctionVisitor fv, EAtom name, int arity, int startLabel) {
			super(fv);
			this.name = name;
			this.arity = arity;
			this.startLabel = startLabel;
		}

		@Override
		public void visitEnd() {

			if (this.name.getName().equals("load")) {
				dump();
			}

			LabeledBlock lb = lbs.get(startLabel);
			lb.merge_from(this.make_initial());

			try {
				while (!needs_analyze.isEmpty()) {
					lb = needs_analyze.first();
					needs_analyze.remove(lb);

					lb.analyze();
				}
			} catch (RuntimeException t) {
				dump();
				throw t;
			} catch (Error t) {
				dump();
				throw t;
			}

			// woo!
			// live_analysis();

			SortedSet<Integer> labels = new TreeSet<Integer>();
			labels.addAll(lbs.keySet());

			for (int i : labels) {
				lb = lbs.get(i);
				if (lb.initial == null) continue;
				ExceptionHandler e = lb.initial.exh;
				if (e == null) continue;
				List<BeamExceptionHandler> ambi = e.ambiguousities();
				if (ambi != null) {
					blocks_with_ambiguous_exh.put(i,ambi);
				}
			}

			boolean has_unreachable_code = false;
			for (int i : labels) {
				lb = lbs.get(i);
				if (lb.isDeadCode()) {
					if (BeamOpcode.get(lb.insns.get(0).elm(1).testAtom()) == BeamOpcode.func_info) {
						// ignore this
					} else {
						System.err.println("UNREACHABLE " + lb.block_label);
						has_unreachable_code = true;
					}
				}
			}
			if (has_unreachable_code) {
				this.dump();
			}

			function_visit_end();

		}

		private void function_visit_end() {

			if (fv instanceof FunctionVisitor2) {
				((FunctionVisitor2) fv).visitMaxs(this.max_xreg,
						this.max_stack, this.max_freg, this.is_tail_recursive);
			}

			for (LabeledBlock block : this.lbs.values()) {
				ExceptionHandler block_exh =
					block.initial==null? null :  block.initial.exh;
				assert(blocks_with_ambiguous_exh.containsKey(block.block_label) == (block_exh != null && block_exh.ambiguousities() != null));
				if (block_exh != null && block_exh.ambiguousities() != null) {
					// Handle block with ambiguous exception handler:
					List<BeamExceptionHandler> ambi = blocks_with_ambiguous_exh.get(block.block_label);
					for (BeamExceptionHandler e : ambi) {
						int ext_label = extendedLabel(block.block_label, e);
						function_visit_end_aux(block, ext_label, e);
					}
				} else {
					int ext_label = extendedLabel(block.block_label, block_exh);
					function_visit_end_aux(block, ext_label, block_exh);
				}
			}

			super.fv.visitEnd();
		}

		private void function_visit_end_aux(LabeledBlock block, int ext_label, BeamExceptionHandler exh) {
			BlockVisitor vis = super.fv.visitLabeledBlock(ext_label);
			try {
				block.accept(vis, exh);
			} catch (Error e) {
				dump();
				throw e;
			}
		}

		private void dump() {
			if (ERT.DEBUG2) {
			System.err.println("DUMPING " + name + "/" + arity);

			for (Map.Entry<Integer, LabeledBlock> ent : lbs.entrySet()) {
				ent.getValue().dump();
			}
			}
		}

		@Override
		public BlockVisitor visitLabeledBlock(int label) {
			return get_lb(label, true);
		}

		private TypeMap make_initial() {
			TypeMap res = new TypeMap(makeBasicBlock(startLabel, 0));
			for (int i = 0; i < arity; i++) {
				res = res.setx(i, EOBJECT_TYPE);
			}
			return res;
		}

		private LabeledBlock get_lb(int label, boolean create) {
			if (lbs.containsKey(label)) {
				LabeledBlock res = lbs.get(label);
				return res;
			} else if (create) {
				// System.out.println("creating " + name + "/" + arity + ":" +
				// label);

				LabeledBlock res = new LabeledBlock(label);
				lbs.put(label, res);
				return res;
			} else {
				return null;
			}
		}

		int extendedLabel(int label, BeamExceptionHandler exh) {
			assert ((label &~ 0xffff) == 0);
			int extLabel = label;
			if (blocks_with_ambiguous_exh.containsKey(label)) {
				// Add info about exception context:
				if (exh != null) {
					assert(! blocks_with_ambiguous_exh.containsKey(exh.getHandlerLabel()));
					int handlerLabel = exh.getHandlerLabel();
					assert ((handlerLabel &~ 0xffff) == 0);
					extLabel |= (handlerLabel << 16);
				} // else high 16 bits are 0.
			}
			return extLabel;
		}

		class LabeledBlock implements BlockVisitor, BeamCodeBlock {

			private final int block_label;
			TypeMap initial;
			boolean last = false;
			TypeMap[] map;

			public LabeledBlock(int label) {
				this.block_label = label;
				initial = null;
			}

			/**
			 * @param vis
			 */
			public void accept(BlockVisitor vis, BeamExceptionHandler exh) {

				try {
					if (!isDeadCode()) {

						if (vis instanceof BlockVisitor2) {
							accept_2((BlockVisitor2) vis, exh);
						} else {
							accept_1(vis);
						}

					}
				} finally {
					vis.visitEnd();
				}
			}

			private void accept_1(BlockVisitor vis) {
				for (int i = 0; i < insns.size(); i++) {
					ETuple insn = insns.get(i);
					BeamOpcode opcode = BeamOpcode.get(insn.elm(1).testAtom());
					vis.visitInsn(opcode, insn);
				}
			}

			private void accept_2(BlockVisitor2 vis, BeamExceptionHandler exh) {
				int tuple_pos = 0;
				Arg tuple_reg = null;

				vis.visitBegin(exh);

				for (int insn_idx = 0; insn_idx < insns.size(); insn_idx++) {
					ETuple insn = insns.get(insn_idx);
					BeamOpcode opcode = BeamOpcode.get(insn.elm(1).testAtom());
					TypeMap type_map = this.map[insn_idx];

					switch (opcode) {
					case func_info:
						// System.err.print("go: " + insn);
						vis.visitInsn(opcode, (Arg) new ExtFunc(insn.elm(2)
								.testTuple().elm(2).testAtom(), insn.elm(3)
								.testTuple().elm(2).testAtom(), insn.elm(4)
								.asInt()));
						break;

					case fconv:
					case fmove:
					case move: {
						// System.err.println(insn);
						Arg arg1 = decode_arg(insn_idx, insn.elm(2));
						Arg arg2 = decode_out_arg(insn_idx, insn.elm(3));

						if (arg2.kind != Kind.F) {
							if (arg1.kind == Kind.F) {
								arg2 = new Arg(arg2, EDOUBLE_TYPE);
							} else {
								arg2 = new Arg(arg2, arg1.type);
							}
						} else {
							// arg2.kind == F
						}

						vis.visitInsn(opcode, arg1, arg2);
						break;
					}

					case arithfbif: {
						// System.err.println("gen: " + insn);
						EAtom name = insn.elm(2).testAtom();
						int failLabel = decode_labelref(insn.elm(3), this.map[insn_idx].exh);
						ESeq parms = insn.elm(4).testSeq();
						Arg[] in = decode_args(insn_idx, parms.toArray());
						Arg out = decode_out_arg(insn_idx, insn.elm(5));

						BuiltInFunction bif = BIFUtil.getMethod(name.getName(),
								parmTypes(this.map[insn_idx], parms),
								failLabel != 0);

						vis.visitInsn(opcode, failLabel, in, out, bif);
						break;
					}

					case bif: {
						// System.err.println("gen: " + insn);
						EAtom name = insn.elm(2).testAtom();
						int failLabel = decode_labelref(insn.elm(3), this.map[insn_idx].exh);
						ESeq parms = insn.elm(4).testSeq();
						Arg[] in = decode_args(insn_idx, parms.toArray());
						Arg out = decode_out_arg(insn_idx, insn.elm(5));

						BuiltInFunction bif = BIFUtil.getMethod(name.getName(),
								parmTypes(this.map[insn_idx], parms),
								failLabel != 0);

						vis.visitInsn(opcode, failLabel, in, out, bif);
						break;
					}

					case gc_bif: {
						// System.err.println("gen: " + insn);
						EAtom name = insn.elm(2).testAtom();
						int failLabel = decode_labelref(insn.elm(3), this.map[insn_idx].exh);
						ESeq parms = insn.elm(5).testSeq();
						Arg[] in = decode_args(insn_idx, parms.toArray());
						Arg out = decode_out_arg(insn_idx, insn.elm(6));

						BuiltInFunction bif = BIFUtil.getMethod(name.getName(),
								parmTypes(this.map[insn_idx], parms),
								failLabel != 0);

						vis.visitInsn(opcode, failLabel, in, out, bif);
						break;
					}

					case test:
						accept_2_test(vis, insn, insn_idx);
						break;

					case K_return:
						vis.visitInsn(opcode, new Arg(Arg.Kind.X, 0,
								this.map[insn_idx].getx(0)));
						break;

					case allocate_heap_zero:
					case allocate_zero: {
						int depth = type_map.stacksize;
						int count = insn.elm(2).asInt();
						Arg[] ys = new Arg[count];
						for (int i = 0; i < count; i++) {
							ys[i] = new Arg(Arg.Kind.Y, depth + i, null);
						}
						vis.visitInsn(opcode, (Arg[]) ys);
						break;

					}

					case test_heap:
						break;

					case fclearerror:
					case fcheckerror:
						break;

					case call_ext_last:
					case call_ext_only:
						do_call(vis, insn_idx, insn, true, true);
						break;

					case call_ext:
						do_call(vis, insn_idx, insn, false, true);
						if (is_exceptional_call(insn))
							vis.visitUnreachablePoint();
						break;

					case call:
						do_call(vis, insn_idx, insn, false, false);
						break;

					case call_last:
					case call_only:
						do_call(vis, insn_idx, insn, true, false);
						break;

					case apply_last:
					case apply: {
						Arg[] args = new Arg[2 + insn.elm(2).asInt()];
						for (int i = 0; i < args.length; i++) {
							args[i] = new Arg(Arg.Kind.X, i, map[insn_idx]
									.getx(i));
						}
						vis.visitInsn(opcode, args);
						break;

					}

					case make_fun2: {
						ExtFunc efun = new ExtFunc(insn.elm(2).testTuple());
						int numfree = insn.elm(5).asInt();
						Arg[] free = new Arg[numfree];
						for (int i = 0; i < numfree; i++) {
							free[i] = new Arg(Arg.Kind.X, i, map[insn_idx]
									.getx(i));
						}
						vis.visitInsn(opcode, efun, free);
						break;
					}

					case init:
						vis.visitInsn(opcode, decode_out_arg(insn_idx, insn
								.elm(2)));
						break;

					case put_list: {
						Arg[] in = new Arg[] {
								decode_arg(insn_idx, insn.elm(2)),
								decode_arg(insn_idx, insn.elm(3)) };
						Arg out = decode_out_arg(insn_idx, insn.elm(4));

						vis.visitInsn(opcode, in, out);
						break;
					}

					case put_tuple: {

						int arity = insn.elm(2).asInt();
						Type tupleType = getTupleType(arity);
						tuple_reg = new Arg(decode_out_arg(insn_idx, insn
								.elm(3)), tupleType);
						vis.visitInsn(opcode, arity, tuple_reg);
						tuple_pos = 1;
						break;
					}

					case put: {
						Arg val = decode_arg(insn_idx, insn.elm(2));
						vis.visitInsn(opcode, val, tuple_reg, tuple_pos++);
						break;
					}

					case set_tuple_element: {
						Arg in = decode_arg(insn_idx, insn.elm(2));
						Arg out = decode_arg(insn_idx, insn.elm(3));
						int idx = insn.elm(4).asInt();

						vis.visitInsn(opcode, in, out, idx);

						break;
					}

					case allocate_heap:
					case allocate:
					case deallocate: {
						// need to zero out refs?
						break;
					}

					case select_tuple_arity: {
						int failLabel = decode_labelref(insn.elm(3), this.map[insn_idx].exh);
						Arg in = decode_arg(insn_idx, insn.elm(2));

						ESeq cases = insn.elm(4).testTuple().elm(2).testSeq();
						int len = cases.length() / 2;

						int[] arities = new int[len];
						int[] targets = new int[len];

						int idx = 0;

						while (cases != ERT.NIL) {
							arities[idx] = cases.head().asInt();
							EObject target = cases.tail().head();
							targets[idx] = decode_labelref(target, this.map[insn_idx].exh);

							cases = cases.tail().tail();
							idx += 1;
						}

						vis.visitSelectTuple(in, failLabel, arities, targets);

						break;
					}

					case select_val: {
						Arg in = decode_arg(insn_idx, insn.elm(2));
						int failLabel = decode_labelref(insn.elm(3), this.map[insn_idx].exh);
						ESeq cases = insn.elm(4).testTuple().elm(2).testSeq();
						int len = cases.length() / 2;

						Arg[] values = new Arg[len];
						int[] targets = new int[len];

						int idx = 0;

						while (cases != ERT.NIL) {
							values[idx] = decode_value(cases.head());
							EObject target = cases.tail().head();
							targets[idx] = decode_labelref(target, this.map[insn_idx].exh);

							cases = cases.tail().tail();
							idx += 1;
						}

						vis.visitSelectValue(in, failLabel, values, targets);

						break;
					}

					case get_tuple_element: {

						Arg in = decode_arg(insn_idx, insn.elm(2));
						int idx = insn.elm(3).asInt();
						Arg out = decode_out_arg(insn_idx, insn.elm(4));

						vis.visitInsn(opcode, in, out, idx);

						break;
					}

					case jump:
						vis.visitJump(decode_labelref(insn.elm(2), this.map[insn_idx].exh));
						break;

					case trim:
						break;

					case get_list:
						vis.visitInsn(opcode, new Arg[] {
								decode_arg(insn_idx, insn.elm(2)),
								decode_out_arg(insn_idx, insn.elm(3)),
								decode_out_arg(insn_idx, insn.elm(4)) });
						break;

					case try_case_end:
					case badmatch:
					case case_end:
						vis.visitInsn(opcode, decode_arg(insn_idx, insn.elm(2)));
						break;

					case if_end:
						vis.visitInsn(opcode);
						break;

					case send: {
						vis.visitInsn(opcode,
								new Arg[] { new Arg(Arg.Kind.X, 0),
										new Arg(Arg.Kind.X, 1) });
						break;
					}

					case K_try:
					case K_catch: {
						TypeMap type_map_after = this.map[insn_idx+1];
						vis.visitCatchBlockStart(opcode, decode_labelref(insn.elm(3), this.map[insn_idx].exh),
												 decode_arg(insn_idx, insn.elm(2)),
												 type_map_after.exh);
						break;
					}

					case raise: {
						EObject[] argExprs = insn.elm(3).testSeq().toArray();
						Arg[] in = decode_args(insn_idx, argExprs);
						Arg ex = decode_arg(insn_idx, insn.elm(4));
						int failLabel = decode_labelref(insn.elm(2), this.map[insn_idx].exh);
						
						vis.visitInsn(opcode, failLabel, in, ex);
						break;
					}
					
					case try_end:
					case try_case:
					case catch_end:
						vis.visitCatchBlockEnd(opcode,
								       decode_arg(insn_idx, insn.elm(2)),
								       type_map.exh);
						break;

					case loop_rec: /* loop receive */
						vis.visitReceive(opcode, decode_labelref(insn.elm(2), this.map[insn_idx].exh),
								decode_out_arg(insn_idx, insn.elm(3)));
						break;

					case remove_message:
						vis.visitInsn(opcode);
						break;

					case timeout:
						vis.visitInsn(opcode);
						break;

					case loop_rec_end:
						vis.visitInsn(opcode, decode_labelref(insn.elm(2), this.map[insn_idx].exh), null);
						break;

					case wait:
						vis.visitInsn(opcode, decode_labelref(insn.elm(2), this.map[insn_idx].exh),
								null);
						break;

					case wait_timeout:
						vis.visitInsn(opcode, decode_labelref(insn.elm(2), this.map[insn_idx].exh),
								decode_arg(insn_idx, insn.elm(3)));
						break;

					case call_fun: {
						int nargs = insn.elm(2).asInt();
						Arg[] args = new Arg[nargs + 1];
						for (int i = 0; i < args.length; i++) {
							args[i] = new Arg(Arg.Kind.X, i, map[insn_idx]
									.getx(i));
						}
						vis.visitInsn(opcode, args,
								new Arg(Arg.Kind.X, 0, null));
						break;
					}
					
					// {bs_add,{f,0},[{x,3},{x,4},1],{x,3}}
					case bs_add: {
						EObject[] args = insn.elm(3).testSeq().toArray();
						Arg[] in = decode_args(insn_idx, args);
						Arg out = decode_out_arg(insn_idx, insn.elm(4));
						vis.visitBSAdd(in, out);
						break;
					}
						

					
					case bs_context_to_binary:
						vis.visitBS(opcode, decode_arg(insn_idx, insn.elm(2)), null);
						// do nothing for now
						break;

					case bs_restore2:
					case bs_save2: {
						vis.visitBS(opcode, decode_arg(insn_idx, insn.elm(2)), decode_arg(insn_idx, insn.elm(3)));
						// do nothing for now
						break;
					}
					

					case bs_init2: {
						Arg size = decode_arg(insn_idx, insn.elm(3));
						Arg flags = decode_arg(insn_idx, insn.elm(6));
						Arg out = decode_out_arg(insn_idx, insn.elm(7));
						
						vis.visitInitBitString(size, flags, out);
						
						break;
					}
					

					// TODO: we don't use all args here, why?
					case bs_init_bits: {
						Arg size = decode_arg(insn_idx, insn.elm(3));
						Arg flags = decode_arg(insn_idx, insn.elm(6));
						Arg out = decode_out_arg(insn_idx, insn.elm(7));
						
						vis.visitInitBitString(size, flags, out);
						
						break;
					}
					
					case bs_put_string: {
						Arg str = decode_arg(insn_idx, insn.elm(3));
						vis.visitBitStringPut(opcode, str, null, null);
						
						break;
					}
						
					case bs_put_binary:
					case bs_put_integer: {
						Arg size = decode_arg(insn_idx, insn.elm(3));
						Arg flags = decode_arg(insn_idx, insn.elm(4));
						Arg value = decode_arg(insn_idx, insn.elm(6));
						vis.visitBitStringPut(opcode, value, size, flags);
						
						break;
					}
						

					
					default:
						throw new Error("unhandled insn: " + insn);
					}

				}
			}

			private void do_call(BlockVisitor2 vis, int insn_idx, ETuple insn,
					boolean is_tail, boolean is_external) throws Error {
				int arg_count = insn.elm(2).asInt();
				Arg[] args = new Arg[arg_count];
				for (int i = 0; i < arg_count; i++) {
					args[i] = new Arg(Kind.X, i, this.map[insn_idx].getx(i));
				}

				ETuple ft = insn.elm(3).testTuple();
				ExtFunc fun;
				if (ft.elm(1) != EXTFUNC_ATOM)
					fun = new ExtFunc(ft.elm(1).testAtom(), ft.elm(2)
							.testAtom(), ft.elm(3).asInt());
				else
					fun = new ExtFunc(ft.elm(2).testAtom(), ft.elm(3)
							.testAtom(), ft.elm(4).asInt());
				vis.visitCall(fun, args, is_tail, is_external);
			}

			private void accept_2_test(BlockVisitor2 vis, ETuple insn,
					int insn_idx) {

				int failLabel = decode_labelref(insn.elm(3), this.map[insn_idx].exh);

				EObject[] argExprs = insn.elm(4).testSeq().toArray();
				Arg[] args = decode_args(insn_idx, argExprs);

				BeamOpcode test = BeamOpcode.get(insn.elm(2).testAtom());
				switch (test) {
				case is_boolean:
				case is_atom:
					vis.visitTest(test, failLabel, args[0], EATOM_TYPE);
					break;

				case is_integer:
					vis.visitTest(test, failLabel, args[0], EINTEGER_TYPE);
					break;

				case is_float:
					vis.visitTest(test, failLabel, args[0], EDOUBLE_TYPE);
					break;

				case is_binary:
					vis.visitTest(test, failLabel, args[0], EBINARY_TYPE);
					break;

				case is_list:
					vis.visitTest(test, failLabel, args[0], ECONS_TYPE);
					break;

				case is_nonempty_list:
					vis.visitTest(test, failLabel, args[0], ESEQ_TYPE);
					break;

				case is_tuple:
					vis.visitTest(test, failLabel, args[0], ETUPLE_TYPE);
					break;

				case is_nil:
					vis.visitTest(test, failLabel, args[0], ENIL_TYPE);
					break;

				case is_number:
					vis.visitTest(test, failLabel, args[0], ENUMBER_TYPE);
					break;

				case is_bitstr:
					vis.visitTest(test, failLabel, args[0], EBITSTRING_TYPE);
					break;

				case is_pid:
					vis.visitTest(test, failLabel, args[0], EPID_TYPE);
					break;

				case is_port:
					vis.visitTest(test, failLabel, args[0], EPORT_TYPE);
					break;

				case is_reference:
					vis.visitTest(test, failLabel, args[0], EREFERENCE_TYPE);
					break;

				case is_function:
					vis.visitTest(test, failLabel, args[0], EFUN_TYPE);
					break;

				case is_function2:
					vis.visitTest(test, failLabel, args[0], argExprs[1]
							.testTuple().elm(2).asInt(), EFUN_TYPE);
					break;

				case is_lt:
				case is_ge:
				case is_eq_exact:
				case is_ne_exact:
				case is_ne:
				case is_eq:
					vis.visitTest(test, failLabel, args, (Arg) null,
							Type.VOID_TYPE);
					break;

				case test_arity:
					int arity = argExprs[1].asInt();
					vis.visitTest(test, failLabel, args[0], arity,
							getTupleType(arity));
					break;

				case bs_start_match2:
				case bs_match_string:
				case bs_get_integer2:
				case bs_test_tail2:
				case bs_get_binary2:
				case bs_skip_bits2:
				case bs_test_unit:
					vis.visitBitStringTest(test, failLabel, args);
					break;

				default:
					throw new Error("unhandled test: " + insn + " at index "
							+ insn_idx + " // " + test + ":" + test.ordinal());
				}

			}

			/**
			 * @param insn_idx
			 * @param array
			 * @return
			 */
			private Arg[] decode_args(int insn_idx, EObject[] input) {
				Arg[] output = new Arg[input.length];
				for (int i = 0; i < input.length; i++) {
					output[i] = decode_arg(insn_idx, input[i]);
				}
				return output;
			}

			/**
			 * @param insnIdx
			 * @param eObject
			 * @return
			 */
			private Arg decode_arg(int insn_idx, EObject src) {
				TypeMap current = this.map[insn_idx];

				if (src instanceof ETuple2) {
					ETuple2 tup = (ETuple2) src;
					if (tup.elem1 == X_ATOM) {
						int xreg = tup.elem2.asInt();
						return new Arg(Arg.Kind.X, xreg, current.getx(xreg));
					} else if (tup.elem1 == Y_ATOM) {
						int yreg = tup.elem2.asInt();
						return new Arg(Arg.Kind.Y, current.get_ypos(yreg),
								current.gety(yreg));
					} else if (tup.elem1 == FR_ATOM) {
						int freg = tup.elem2.asInt();
						return new Arg(Arg.Kind.F, freg, Type.DOUBLE_TYPE);
					} else if (tup.elem1 == ATOM_ATOM) {
						return new Arg(tup.elem2, EATOM_TYPE);
					} else if (tup.elem1 == LITERAL_ATOM) {
						return new Arg(tup.elem2);
					} else if (tup.elem1 == STRING_ATOM) {
						return new Arg(tup.elem2);
					} else if (tup.elem1 == INTEGER_ATOM) {
						return new Arg(tup.elem2);
					} else if (tup.elem1 == FLOAT_ATOM) {
						return new Arg(tup.elem2, Type.DOUBLE_TYPE);
					}

				} else if (src == NIL_ATOM) {
					return new Arg(ERT.NIL, ENIL_TYPE);

				}

				return new Arg(src);

				// return null;

			}

			/**
			 * @param insnIdx
			 * @param eObject
			 * @return
			 */
			private Arg decode_value(EObject src) {

				if (src instanceof ETuple2) {
					ETuple2 tup = (ETuple2) src;
					if (tup.elem1 == ATOM_ATOM) {
						return new Arg(tup.elem2, EATOM_TYPE);
					} else if (tup.elem1 == LITERAL_ATOM) {
						return new Arg(tup.elem2);
					} else if (tup.elem1 == INTEGER_ATOM) {
						return new Arg(tup.elem2);
					} else if (tup.elem1 == FLOAT_ATOM) {
						return new Arg(tup.elem2);
					}

				} else if (src == NIL_ATOM) {
					return new Arg(ERT.NIL, ENIL_TYPE);

				}

				throw new Error("unknown value:" + src);

			}

			private Arg decode_out_arg(int insn_idx, EObject src) {
				TypeMap current = this.map[insn_idx];

				if (src instanceof ETuple2) {
					ETuple2 tup = (ETuple2) src;
					if (tup.elem1 == X_ATOM) {
						int xreg = tup.elem2.asInt();
						return new Arg(Arg.Kind.X, xreg);
					} else if (tup.elem1 == Y_ATOM) {
						int yreg = tup.elem2.asInt();
						return new Arg(Arg.Kind.Y, current.get_ypos(yreg));
					} else if (tup.elem1 == FR_ATOM) {
						int freg = tup.elem2.asInt();
						return new Arg(Arg.Kind.F, freg);
					}
				}

				throw new Error();

			}

			/**
			 * @param nth
			 * @return
			 */
			private int decode_labelref(EObject f_tup, ExceptionHandler exh) {
				if (f_tup == NOFAIL_ATOM)
					return 0;
				assert (f_tup.testTuple().elm(1) == F_ATOM);
				return extendedLabel(f_tup.testTuple().elm(2).asInt(), exh);
			}

			public boolean isDeadCode() {
				return initial == null;
			}

			public void analyze() {
				try {
					analyze0();
				} catch (RuntimeException x) {

					// dump();

					throw x;
				}
			}

			private void dump() {
				if (ERT.DEBUG2 == true) return;
				next_insn: for (int i = 0; i < insns.size(); i++) {
					System.err.println(name + "(" + block_label + "):" + i
							+ " :: " + (map == null ? "?" : map[i]));
					System.err.println("     >> " + insns.get(i));
				}
			}

			public void analyze0() {
				TypeMap current = initial;
				BeamOpcode last_opcode = BeamOpcode.NONE;
				ETuple last_insn = null;

				map = new TypeMap[insns.size()];

				next_insn: for (int insn_idx = 0; insn_idx < insns.size(); insn_idx++) {

					update_max_regs(current);

					if (is_term(last_opcode)) {
						throw new Error("how did we get here then...? "
								+ this.block_label + ":" + insn_idx);
					}

					map[insn_idx] = current;
					ETuple insn = insns.get(insn_idx);
					BeamOpcode code = BeamOpcode.get(insn.elm(1).testAtom());
					last_opcode = code; last_insn = insn;
					/*
					 * System.out.println(name + "(" + bb_label + "):" + i +
					 * " :: " + current + "" + insn);
					 */

					if (current.exh != null && may_terminate_exceptionally(code))
						addExceptionEdge(current);

					switch (code) {
					case fmove:
					case move: {
						EObject src = insn.elm(2);
						EObject dst = insn.elm(3);

						Type srcType = getType(current, src);

						boolean boxed = false;
						if (sizeof(current, src) > sizeof(current, dst)) {
							// System.err.println(insn);
							if (getType(current, src).equals(Type.DOUBLE_TYPE)) {
								current = setType(current, dst, EDOUBLE_TYPE);
								boxed = true;
							} else {
								throw new Error("why?" + insn);
							}
						}

						if (!boxed) {
							current = setType(current, (ETuple2) dst, srcType);
						}
						continue next_insn;
					}

					case jump: {
						current = branch(current, insn.elm(2), insn_idx);
						continue next_insn;

					}

					case send: {
						current.touchx(0, 2);
						current = current.setx(0, current.getx(1));
						continue next_insn;
					}

					case arithfbif: {
						// System.err.println(insn);
						EAtom name = insn.elm(2).testAtom();
						ESeq parms = insn.elm(4).testSeq();

						checkArgs(current, parms, insn);

						Type type = getBifResult(name.getName(), parmTypes(
								current, parms), false);

						current = setType(current, insn.elm(5), type);

						continue next_insn;
					}

					case gc_bif: {
						// {gc_bif,BifName,F,Live,[A1,A2?],Reg};

						boolean is_guard = false;
						if (insn.elm(3).testTuple().elm(2).asInt() != 0) {
							is_guard = true;
						}

						// System.err.println(insn);
						current = branch(current, insn.elm(3), insn_idx);

						EAtom name = insn.elm(2).testAtom();
						ESeq parms = insn.elm(5).testSeq();

						checkArgs(current, parms, insn);

						Type type = getBifResult(name.getName(), parmTypes(
								current, parms), is_guard);

						current = setType(current, insn.elm(6), type);

						// dump();
						continue next_insn;
					}

					case bif: {
						// System.err.println(insn);
						current = branch(current, insn.elm(3), insn_idx);

						EAtom name = insn.elm(2).testAtom();
						ESeq parms = insn.elm(4).testSeq();

						checkArgs(current, parms, insn);

						Type type = getBifResult(name.getName(), parmTypes(
								current, parms), false);

						current = setType(current, insn.elm(5), type);

						continue next_insn;
					}

					case test: {
						try {
							current = analyze_test(current, insn, insn_idx);
						} catch (Error e) {
							throw new Error(" at "
									+ LabeledBlock.this.block_label + ":"
									+ insn_idx, e);
						}
						assert (current != null);
						continue next_insn;
					}

					case fconv: {
						// unbox and convert to DOUBLE
						getType(current, insn.elm(2));
						current = setType(current, insn.elm(3),
								Type.DOUBLE_TYPE);
						continue next_insn;
					}

					case init: {
						current = setType(current, insn.elm(2), ENIL_TYPE);
						continue next_insn;
					}

					case set_tuple_element: {
						getType(current, insn.elm(2));
						getType(current, insn.elm(3));
						continue next_insn;
					}

					case get_tuple_element: {
						EObject src = insn.elm(2);
						int idx = insn.elm(3).asInt();
						EObject dst = insn.elm(4);
						getType(current, src);
						current = setType(current, dst, EOBJECT_TYPE);
						continue next_insn;
					}

					case get_list: {
						EObject from = insn.elm(2);
						EObject head_into = insn.elm(3);
						EObject tail_into = insn.elm(4);

						current = setType(current, head_into, EOBJECT_TYPE);

						Type srctype = getType(current, from);
						if (srctype == ELIST_TYPE || srctype == ESEQ_TYPE) {
							current = setType(current, tail_into, ESEQ_TYPE);
						} else {
							current = setType(current, tail_into, EOBJECT_TYPE);
						}

						continue next_insn;
					}

					case put_list: {

						Type head_type = getType(current, insn.elm(2));
						Type tail_type = getType(current, insn.elm(3));

						if (tail_type == null) {
							throw new Error("value: " + insn.elm(3)
									+ " has no type");
						}

						if (tail_type.equals(ENIL_TYPE)
								|| tail_type.equals(ESEQ_TYPE)
								|| tail_type.equals(ELIST_TYPE)) {
							current = setType(current, insn.elm(4), ELIST_TYPE);
						} else {
							current = setType(current, insn.elm(4), ECONS_TYPE);
						}
						continue next_insn;
					}

					case put_tuple: {
						int arity = insn.elm(2).asInt();
						ETuple reg = insn.elm(3).testTuple();

						current = setType(current, reg, getTupleType(arity));
						continue next_insn;
					}

					case K_try:
						current = setType(current, insn.elm(2), EOBJECT_TYPE);
						current = installExceptionHandler(current, insn.elm(3), insn_idx);
						continue next_insn;

					case try_end:
						current = current.popExceptionHandler();
						continue next_insn;

					case try_case:
						getType(current, insn.elm(2));
						current = current.popExceptionHandler();
						current = current.setx(0, EATOM_TYPE); // reason
						current = current.setx(1, EOBJECT_TYPE); // value
						current = current.setx(2, EOBJECT_TYPE); // trace
						continue next_insn;

					case try_case_end:
						continue next_insn;

					case raise:
						boolean is_guard = false;
						if (insn.elm(2).testTuple().elm(2).asInt() != 0) {
							is_guard = true;
						}

						checkArgs(current, insn.elm(3), insn);
						current = setType(current, insn.elm(4), EOBJECT_TYPE);

						continue next_insn;

					case K_catch:
						current = installExceptionHandler(current, insn.elm(3), insn_idx);
						continue next_insn;

					case catch_end:
						current = current.popExceptionHandler();
						current = current.setx(0, EOBJECT_TYPE); // value
						continue next_insn;

					case make_fun2: {
						current.touchx(0, insn.elm(5).asInt());
						current = current.setx(0, EFUN_TYPE);
						continue next_insn;
					}

					case loop_rec: {
						current = branch(current, insn.elm(2), insn_idx);
						current = setType(current, insn.elm(3), EOBJECT_TYPE);
						continue next_insn;
					}

					case remove_message:
						// assume this insn overrides X0
						// current = current.setx(0, EOBJECT_TYPE);
						continue next_insn;

					case loop_rec_end:
					case timeout: {
						// System.err.println(insn);
						continue next_insn;
					}

					case wait_timeout: {
						Type timeout_type = getType(current, insn.elm(3));
					}
					case wait: {
						current = branch(current, insn.elm(2), insn_idx);
						continue next_insn;
					}

					case deallocate:
					case trim: {
						int howmuch = insn.elm(2).asInt();
						current = current.trim_y(howmuch);
						continue next_insn;
					}

						// this is really a no-op in jave
					case test_heap: {
						continue next_insn;
					}

					case allocate_zero:
					case allocate_heap_zero: {
						int slots = insn.elm(2).asInt();
						current = current.alloc_y(slots);
						for (int slot = 0; slot < slots; slot++) {
							current = current.sety(slot, ENIL_TYPE);
						}
						continue next_insn;
					}

					case allocate:
					case allocate_heap: {
						current = current.alloc_y(insn.elm(2).asInt());
						continue next_insn;
					}

					case fcheckerror:
					case fclearerror:
						continue next_insn;

					case put:
						getType(current, insn.elm(2));
						continue next_insn;

					case select_tuple_arity: {
						current = branch(current, insn.elm(3), insn_idx);

						// touch select reg
						getType(current, insn.elm(2));

						ESeq cases = insn.elm(4).testTuple().elm(2).testSeq();
						while (cases != ERT.NIL) {
							EObject value = cases.head();
							EObject target = cases.tail().head();

							current = setType(current, insn.elm(2),
									getTupleType(value.asInt()));

							current = branch(current, target, insn_idx);

							cases = cases.tail().tail();
						}

						continue next_insn;
					}

					case select_val: {
						current = branch(current, insn.elm(3), insn_idx);

						// touch select reg
						getType(current, insn.elm(2));

						ESeq cases = insn.elm(4).testTuple().elm(2).testSeq();
						while (cases != ERT.NIL) {
							EObject value = cases.head();
							EObject target = cases.tail().head();

							current = branch(current, target, insn_idx);

							cases = cases.tail().tail();
						}

						continue next_insn;
					}

						// we loose the type of the result

					case apply_last:
						is_tail_recursive = true;
					case apply:
					case call:
					case call_ext: {
						int argCount = insn.elm(2).asInt();
						current.touchx(0, argCount);
						current = current.setx(0, EOBJECT_TYPE);
						continue next_insn;
					}

						// all these exit
					case K_return:
						getType(current, X0_REG);
						continue next_insn;

					case call_last:
					case call_only:
					case call_ext_last:
					case call_ext_only:
						is_tail_recursive = true;
						int argCount = insn.elm(2).asInt();
						current.touchx(0, argCount);
						continue next_insn;

					case func_info:
						continue next_insn;

					case if_end:
					case badmatch:
					case case_end:
						continue next_insn;

					case bs_add: {
						EObject[] args = insn.elm(3).testSeq().toArray();
						Type in1 = getType(current, args[0]);
						Type in2 = getType(current, args[1]);
						current = setType(current, insn.elm(4), Type.INT_TYPE);
						continue next_insn;
					}
						
					case bs_context_to_binary: {
						Type ctx = getType(current, insn.elm(2));
						current = current.setx(0, EBINARY_TYPE);
						continue next_insn;
					}

					case bs_save2: {
						continue next_insn;
					}
					
					case bs_restore2: {
						current = setType(current, insn.elm(2), EMATCHSTATE_TYPE);
						continue next_insn;
					}					


					case bs_init2: {
						// int size = insn.elm(3).asInt();
						current = setType(current, insn.elm(7), EBINARY_TYPE);
						continue next_insn;
					}

					case bs_init_bits: {
						// int size = insn.elm(3).asInt();
						current = setType(current, insn.elm(7), EBITSTRING_TYPE);
						continue next_insn;
					}

					case bs_put_string: {
						continue next_insn;
					}

					case bs_put_binary: {
						continue next_insn;
					}

					case bs_put_integer: {
						continue next_insn;
					}

					case call_fun: {
						int nargs = insn.elm(2).asInt();
						for (int i = 0; i < nargs; i++) {
							current.getx(i);
						}
						current = current.setx(0, EOBJECT_TYPE);
						continue next_insn;
					}

					default:
						throw new Error("unhandled: " + insn + "::" + current);
					}
				}

				update_max_regs(current);

				if (is_term(last_opcode) == false &&
					!is_exceptional_call(last_insn))
				{
					LabeledBlock lbv;
					lbv = get_lb(this.block_label + 1, false);
					try {
						if (lbv != null)
							lbv.merge_from(current);
					} catch (Error e) {

						System.out.println("merge " + current + "\n    | "
								+ lbv.initial + "\n FAILED");
						throw e;

					}
				}
			}

			private void update_max_regs(TypeMap current) {
				max_stack = Math.max(max_stack, current.stacksize);
				max_xreg = Math.max(max_xreg, current.max_xreg());
				max_freg = Math.max(max_freg, current.max_freg());
			}

			boolean is_term(BeamOpcode code) {
				switch (code) {
				case K_return:
				case if_end:
				case badmatch:
				case case_end:
				case call_last:
				case call_only:
				case call_ext_last:
				case call_ext_only:
				case func_info:
				case apply_last:

				case wait:
				case select_tuple_arity:
				case select_val:

				case jump:
					return true;
				default:
					return false;
				}
			}

			boolean may_terminate_exceptionally(BeamOpcode code) {
				// A conservative approximation:
				switch (code) {
				case label:
				case jump:
				case K_return:

				case move:

				case K_try:
				case K_catch:
				case try_end:
				case catch_end:
					return false;
				default:
					return true;
				}
			}

			boolean is_exceptional_call(ETuple insn) {
				BeamOpcode opcode = BeamOpcode.get(insn.elm(1).testAtom());
				if (opcode == BeamOpcode.call_ext) {
				    ETuple ft = insn.elm(3).testTuple();
				    if (ft.elm(1) != EXTFUNC_ATOM) {
					EAtom module   = ft.elm(1).testAtom();
					EAtom function = ft.elm(2).testAtom();
					int arity      = ft.elm(3).asInt();
					if (module == ERLANG_ATOM &&
					    function == ERROR_ATOM &&
					    arity == 1) return true;
				    }
				}
				return false;
			}

			private int sizeof(TypeMap current, EObject cell) {

				ETuple at;
				if ((at = cell.testTuple()) != null) {
					if (at.arity() == 2) {
						if (at.elm(1) == X_ATOM)
							return 32;
						if (at.elm(1) == Y_ATOM)
							return 32;
						if (at.elm(1) == FR_ATOM)
							return 64;
					}
				}

				Type t = getType(current, cell);
				if (t == Type.DOUBLE_TYPE) {
					return 64;
				} else {
					return 32;
				}
			}

			private Type getBifResult(String name, Type[] parmTypes,
					boolean is_guard) {
				return BIFUtil.getBifResult(name, parmTypes, is_guard);
			}

			private Type[] parmTypes(TypeMap current, ESeq args) {
				ArrayList<Type> res = new ArrayList<Type>();

				while (args != ERT.NIL) {
					EObject arg = args.head();
					res.add(getType(current, arg));
					args = args.tail();
				}

				return res.toArray(new Type[res.size()]);
			}

			private void checkArgs(TypeMap current, EObject eTerm, ETuple insn) {
				ESeq args = eTerm.testSeq();

				while (args != ERT.NIL) {
					EObject arg = args.head();

					if (getType(current, arg) == null) {
						throw new Error("uninitialized " + arg + " in " + insn);
					}

					args = args.tail();
				}
			}

			private TypeMap analyze_test(TypeMap current, ETuple insn,
					int insn_idx) {

				current = branch(current, insn.elm(3), insn_idx);

				EObject[] args = insn.elm(4).testSeq().toArray();
				EObject arg1 = args[0];
				EObject arg2 = (args.length > 1) ? args[1] : null;

				BeamOpcode test = BeamOpcode.get(insn.elm(2).testAtom());
				switch (test) {
				case is_nil: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1.testTuple(), ENIL_TYPE);
				}

				case is_list:
				case is_nonempty_list: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1.testTuple(), ECONS_TYPE);
				}

				case is_binary: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1.testTuple(), EBINARY_TYPE);
				}
				case is_tuple: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1.testTuple(), ETUPLE_TYPE);
				}

				case test_arity: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1.testTuple(), getTupleType(arg2
							.asInt()));
				}

				case is_boolean:
				case is_atom: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1.testTuple(), EATOM_TYPE);
				}

				case is_integer: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1, EINTEGER_TYPE);
				}

				case is_bitstr: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1, EBITSTRING_TYPE);
				}

				case is_number: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1, ENUMBER_TYPE);
				}

				case is_pid: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1, EPID_TYPE);
				}

				case is_port: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1, EPORT_TYPE);
				}

				case is_reference: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1, EREFERENCE_TYPE);
				}

				case is_float: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1, EDOUBLE_TYPE);
				}

				case is_function2:
				case is_function: {
					checkArgs(current, insn.elm(4), insn);
					return setType(current, arg1, EFUN_TYPE);
				}

				case is_ge:
				case is_ne:
				case is_ne_exact:
					checkArgs(current, insn.elm(4), insn);
					return current;

				case is_eq:
				case is_eq_exact: {
					checkArgs(current, insn.elm(4), insn);
					Type t1 = getType(current, arg1);
					Type t2 = getType(current, arg2);

					if (!t1.equals(t2)) {
						if (isReg(arg1)) {
							current = setType(current, arg1.testTuple(), t2);
						} else if (isReg(arg2)) {
							current = setType(current, arg2.testTuple(), t1);
						}
					}

					return current;
				}

				case is_lt:
					checkArgs(current, insn.elm(4), insn);
					return current;

				case bs_start_match2:
					check(current, args[0]);
					current = setType(current, args[3], EMATCHSTATE_TYPE);
					return current;

				case bs_get_integer2: {
					if (!EMATCHSTATE_TYPE.equals(getType(current, args[0]))) {
						throw new Error("matching without a state");
					}

					ETuple tup;
					if ((tup = args[3].testTuple()) != null) {
						if (tup.elm(1) == INTEGER_ATOM
								&& tup.elm(2).asInt() <= 32) {
							current = setType(current, args[5], Type.INT_TYPE);
							return current;
						}
					}

					current = setType(current, args[5], ENUMBER_TYPE);
					return current;
				}

				case bs_get_binary2: {
					if (!EMATCHSTATE_TYPE.equals(getType(current, args[0]))) {
						throw new Error("matching without a state");
					}

					current = setType(current, args[5], EBINARY_TYPE);
					return current;
				}

				case bs_get_float2: {
					if (!EMATCHSTATE_TYPE.equals(getType(current, args[0]))) {
						throw new Error("matching without a state");
					}

					current = setType(current, args[5], Type.DOUBLE_TYPE);
					return current;
				}

					// these bit string matchers don't modify registers

				case bs_test_tail2:
				case bs_test_unit:
				case bs_skip_bits2:
				case bs_match_string:

					if (!EMATCHSTATE_TYPE.equals(getType(current, args[0]))) {
						throw new Error("matching without a state");
					}

					return current;

				default:
					throw new Error("unhandled test: " + insn);
				}

			}

			private void check(TypeMap current, EObject src) {
				if (getType(current, src) == null) {
					throw new Error("argument has no type");
				}
			}

			private TypeMap branch(TypeMap current, EObject nth, int idx) {
				if (nth != NOFAIL_ATOM) {

					ETuple tuple = nth.testTuple();
					if (tuple.elm(1) != F_ATOM)
						throw new Error("not a branch target: " + nth);

					int target = tuple.elm(2).asInt();
					if (target != 0) {
						get_lb(target, false).merge_from(current);
					}
				}

				return current.clearLive(makeBasicBlock(block_label, idx + 1));
			}

			private TypeMap installExceptionHandler(TypeMap current, EObject nth, int idx) {
				 ETuple tuple = nth.testTuple();
				 if (tuple.elm(1) != F_ATOM)
					  throw new Error("not a branch target: " + nth);

				 int target = tuple.elm(2).asInt();
				 TypeMap afterPush = current.pushExceptionHandler(target);

				 return afterPush.clearLive(makeBasicBlock(block_label, idx + 1));
			}

			private void addExceptionEdge(TypeMap current) {
				int handler_lbl = current.exh.getHandlerLabel();
				if (handler_lbl>=0) get_lb(handler_lbl, false).merge_from(current);
			}


			private Type getTupleType(int arity) {

				ETuple.get_tuple_class(arity);

				String tt = "L" + ETUPLE_TYPE.getInternalName() + arity + ";";
				return Type.getType(tt);
			}

			private boolean isReg(EObject arg2) {
				ETuple et = arg2.testTuple();
				return (et != null && et.elm(1) == X_ATOM
						|| et.elm(1) == Y_ATOM || et.elm(1) == FR_ATOM);
			}

			public void merge_from(TypeMap typeMap) {
				if (initial == null) {
					initial = typeMap.clearLive(makeBasicBlock(
							this.block_label, 0));
					needs_analyze.add(this);

				} else {
					TypeMap new_types = initial.mergeFrom(typeMap);
					if (new_types == initial) {
						// ignore //
					} else {

						// System.out.println("merge " + initial + "\n    | " +
						// typeMap + "\n   -> " + new_types);
						initial = new_types;
						needs_analyze.add(this);
					}
				}

				typeMap.add_succ(initial.bb);
			}

			List<ETuple> insns = new ArrayList<ETuple>();

			@Override
			public void visitEnd() {
			}

			@Override
			public void visitInsn(BeamOpcode opcode, ETuple insn) {
				insns.add(insn);
			}

			private TypeMap setType(TypeMap current, EObject dd, Type type) {
				ETuple dst = dd.testTuple();

				EObject key = dst.elm(1);
				EObject value = dst.elm(2);
				if (key == X_ATOM) {
					current = current.setx(value.asInt(),
							type == Type.DOUBLE_TYPE ? EDOUBLE_TYPE : type);
				} else if (key == Y_ATOM) {
					current = current.sety(value.asInt(),
							type == Type.DOUBLE_TYPE ? EDOUBLE_TYPE : type);
				} else if (key == FR_ATOM) {
					current = current.setf(value.asInt(), type);
				} else {
					throw new Error("unknown " + dst);
				}

				return current;
			}

			private Type getType(TypeMap current, EObject src) {

				if (src instanceof ETuple2) {
					ETuple2 tup = (ETuple2) src;
					if (tup.elem1 == X_ATOM) {
						return current.getx(tup.elem2.asInt());
					} else if (tup.elem1 == Y_ATOM) {
						return current.gety(tup.elem2.asInt());
					} else if (tup.elem1 == FR_ATOM) {
						return current.getf(tup.elem2.asInt());
					} else if (tup.elem1 == ATOM_ATOM) {
						return EATOM_TYPE;
					} else if (tup.elem1 == LITERAL_ATOM) {
						return Type.getType(tup.elem2.getClass());
					} else if (tup.elem1 == INTEGER_ATOM) {
						if (tup.elem2.getClass() == ESmall.class) {
							return ESMALL_TYPE;
						} else if (tup.elem2.getClass() == EBig.class) {
							return EBIG_TYPE;
						} else {
							return Type.getType(tup.elem2.getClass());
						}
					} else if (tup.elem1 == FLOAT_ATOM) {
						return Type.DOUBLE_TYPE;
					} else if (tup.elem1 == FIELD_FLAGS_ATOM) {
						return Type.INT_TYPE;
					}

				} else if (src == NIL_ATOM) {
					return ENIL_TYPE;

				} else if (src instanceof ESmall) {
					return EINTEGER_TYPE;
				}

				throw new Error("unknown " + src);
			}

			@Override
			public BeamInstruction[] getInstructions() {

				BeamInstruction[] res = new BeamInstruction[insns.size()];
				for (int i = 0; i < insns.size(); i++) {
					res[i] = new BInsn(insns.get(i), this.map[i]);
				}

				return res;
			}

			@Override
			public int getLabel() {
				return this.block_label;
			}

			class BInsn implements BeamInstruction {

				private final ETuple insn;
				private final TypeMap current;

				public BInsn(ETuple insn, TypeMap current) {
					this.insn = insn;
					this.current = current;
				}

				@Override
				public BeamOpcode opcode() {
					return BeamOpcode.get(insn.elm(1).testAtom());
				}

				@Override
				public String toString() {
					return insn.toString();
				}
			}
		}

		@Override
		public int getArity() {
			return arity;
		}

		@Override
		public String getName() {
			return name.getName();
		}

		@Override
		public String getModuleName() {
			return moduleName.getName();
		}

		@Override
		public boolean isExported() {
			String externalName = getName() + "/" + getArity();
			return exports.contains(externalName);
		}

		@Override
		public int getFregCount() {
			return max_freg;
		}

		@Override
		public int getXregCount() {
			return max_xreg;
		}

		@Override
		public int getYregCount() {
			return max_stack;
		}

		@Override
		public BeamCodeBlock[] getCodeBlocks() {

			BeamCodeBlock[] blocks = this.lbs.values().toArray(
					new BeamCodeBlock[0]);

			return blocks;
		}

		@Override
		public int getEntryLabel() {
			return startLabel;
		}

	}

	public void visitModule(EAtom name) {
		this.moduleName = name;
		super.visitModule(name);
	}

	Set<String> exports = new HashSet<String>();

	/** list of {Fun,Arity,Entry} */
	public void visitExport(EAtom fun, int arity, int entry) {
		exports.add(fun.getName() + "/" + arity);
		super.visitExport(fun, arity, entry);
	}

	/** list of {Atom,Value} */
	public void visitAttribute(EAtom att, EObject value) {
		super.visitAttribute(att, value);
	}

	public String getModuleName() {
		return this.moduleName.getName();
	}

	public BeamFunction[] functions() {
		return functions.toArray(new BeamFunction[functions.size()]);
	}

}
