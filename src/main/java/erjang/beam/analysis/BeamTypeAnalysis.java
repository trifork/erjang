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
import java.util.Collection;
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
import erjang.EString;
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

import erjang.beam.repr.Insn;
import erjang.beam.repr.ExtFun;
import erjang.beam.repr.Operands;
import erjang.beam.repr.Operands.Int;
import erjang.beam.repr.Operands.XReg;
import static erjang.beam.repr.Operands.SourceOperand;
import static erjang.beam.repr.Operands.DestinationOperand;

import static erjang.beam.CodeAtoms.*;

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
	
	static final EAtom am_plus = EAtom.intern("+");
	static final EAtom am_minus = EAtom.intern("-");

	private static final ETuple X0_REG = ETuple.make(new EObject[] { X_ATOM,
			new ESmall(0) });
	private EAtom moduleName;

	private List<FV> functions = new ArrayList<FV>();

	@Override
	public void declareFunction(EAtom fun, int arity, int label) {
		/* ignore */
	}
	
	@Override
	public FunctionVisitor visitFunction(EAtom name, int arity, int startLabel) {
		FV f = new FV(super.visitFunction(name, arity, startLabel), name,
				arity, startLabel);
		functions.add(f);
		return f;
	}

	class FV extends FunctionAdapter implements BeamFunction, TypeMap.XRegMarker {

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

		public HashSet<Integer> all_xregs = new HashSet();

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
					if (lb.insns.get(0).opcode() == BeamOpcode.func_info) {
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
				((FunctionVisitor2) fv).visitMaxs((Collection<Integer>) this.all_xregs  ,
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
				res = res.setx(i, EOBJECT_TYPE, this);
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

		public void mark_xreg_as_used(int reg) {
			all_xregs.add(reg);
		}

		private void update_max_regs(TypeMap current) {
			max_stack = Math.max(max_stack, current.stacksize);
			max_freg  = Math.max(max_freg, current.max_freg());

			/* As for the X registers, we wish to track their usage
			 * individually (rather than just the number of the
			 * highest one used).
			 * This set could be updated here, but it is far more
			 * efficient to do it elsewhere - see TypeMap.setx().
			 */
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
				throw new erjang.NotImplemented();
				/*
				for (int i = 0; i < insns.size(); i++) {
					Insn insn = insns.get(i);
					BeamOpcode opcode = insn.opcode();
					vis.visitInsn(opcode, insn);
				}
				*/
			}

			private void accept_2(BlockVisitor2 vis, BeamExceptionHandler exh) {
				int tuple_pos = 0;
				Arg tuple_reg = null;

				vis.visitBegin(exh);

				for (int insn_idx = 0; insn_idx < insns.size(); insn_idx++) {
					Insn insn_ = insns.get(insn_idx);
					BeamOpcode opcode = insn_.opcode();
					TypeMap type_map = this.map[insn_idx];

					switch (opcode) {
					case func_info: {
						Insn.AAI insn = (Insn.AAI)insn_;
						// System.err.print("go: " + insn);
						vis.visitInsn(opcode, insn.getExtFun());
						break;
					}

					case fconv:
					case fmove:
					case move: {
						Insn.SD insn = (Insn.SD)insn_;
						Arg src  = src_arg(insn_idx, insn.src);
						Arg dest = dest_arg(insn_idx, insn.dest);

						if (dest.kind != Kind.F) {
							if (src.kind == Kind.F) {
								dest = new Arg(dest, EDOUBLE_TYPE);
							} else {
								dest = new Arg(dest, src.type);
							}
						} else {
							// arg2.kind == F
						}

						vis.visitInsn(opcode, src, dest);
						break;
					}

					case put_string: {
						Insn.ByD insn = (Insn.ByD)insn_;
						Arg src  = src_arg(insn_idx, insn.bin);
						Arg dest = dest_arg(insn_idx, insn.dest);
// 						Arg arg1 = decode_arg(insn_idx, insn.elm(3));
// 						Arg arg2 = decode_out_arg(insn_idx, insn.elm(4));
						dest = new Arg(dest, ESEQ_TYPE);
						vis.visitInsn(BeamOpcode.move, src, dest);
						break;
					}

					case fadd:
					case fsub:
					case fmul:
					case fdiv:
					{
						Insn.LSSD insn = (Insn.LSSD)insn_;
						EAtom name = opcode.symbol;
						int failLabel = decode_labelref(insn.label, type_map.exh);
						Arg[] in = new Arg[] {src_arg(insn_idx, insn.src1),
											  src_arg(insn_idx, insn.src2)};
						Type[] inTypes = new Type[] {in[0].type,
													 in[1].type};
						Arg out = dest_arg(insn_idx, insn.dest);

						BuiltInFunction bif = BIFUtil.getMethod("erlang", name.getName(), inTypes,
								failLabel != 0, true);

						vis.visitInsn(opcode, failLabel, in, out, bif);
						break;
					}

					case bif0:
					case bif1:
					case bif2:
					{
						Insn.Bif insn = (Insn.Bif)insn_;
						EAtom name = insn.ext_fun.fun;
						int failLabel = decode_labelref(insn.label, type_map.exh);
						SourceOperand[] srcs = insn.args;
						Arg[] in = src_args(insn_idx, srcs);
						Arg out  = dest_arg(insn_idx, insn.dest);

						BuiltInFunction bif = BIFUtil.getMethod("erlang", name.getName(),
								parmTypes(type_map, srcs),
								failLabel != 0, true);

						vis.visitInsn(opcode, failLabel, in, out, bif);
						break;
					}

 					case gc_bif1:
 					case gc_bif2:
					{
						Insn.GcBif insn = (Insn.GcBif)insn_;
						EAtom name = insn.ext_fun.fun;
						int failLabel = decode_labelref(insn.label, type_map.exh);
						SourceOperand[] srcs = insn.args;
						Arg[] in = src_args(insn_idx, srcs);
						Arg out  = dest_arg(insn_idx, insn.dest);

						// special case for X+1, 1+X, X-1.
						Int lop = null, rop = null;
						if (srcs.length==2 
								&& (((name==am_plus || name == am_minus) && (rop=srcs[1].testInt()) != null && rop.equals(1))
								 || (name==am_plus && (lop=srcs[0].testInt()) != null && lop.equals(1)))) 
						{
							if (name == am_plus) {
								Arg src = (lop == null) ? in[0] : in[1];
								
								vis.visitIncrement(src, out);
								break;
							} else if (name == am_minus) {
								Arg src = in[0];
								vis.visitDecrement(src, out);
								break;								
							}
						}
						
						BuiltInFunction bif = BIFUtil.getMethod("erlang", name.getName(),
								parmTypes(type_map, srcs),
								failLabel != 0, true);

						vis.visitInsn(opcode, failLabel, in, out, bif);
						break;
					}

					case is_tuple: {
						
						if (insn_idx+1 < insns.size()) {
						Insn next_insn = insns.get(insn_idx+1);
						if (next_insn.opcode() == BeamOpcode.test_arity) {
							
							int this_fail = decode_labelref(((Insn.L)insn_).label, this.map[insn_idx].exh);
							int next_fail = decode_labelref(((Insn.L)next_insn).label, this.map[insn_idx+1].exh);

							if (this_fail == next_fail) {

								Arg this_arg = src_arg(insn_idx, ((Insn.LD)insn_).dest);
								Arg next_arg = src_arg(insn_idx+1, ((Insn.LD)next_insn).dest);

								if (this_arg.equals(next_arg)) { 
									// SKIP THIS INSTRUCTION!									
									break;
								}
							}
						}
						}
					}
						
						// Tests:
						// LS:
					case is_integer:
					case is_float:
					case is_number:
					case is_atom:
					case is_pid:
					case is_reference:
					case is_port:
					case is_nil:
					case is_binary:
					case is_list:
					case is_nonempty_list:
					case is_function:
					case is_boolean:
					case is_bitstr:
						// LSI:
					case test_arity:
					case bs_test_tail2:
					case bs_test_unit:
						// LSS:
					case is_lt:
					case is_ge:
					case is_eq:
					case is_ne:
					case is_eq_exact:
					case is_ne_exact:
					case is_function2:
						// LSBi:
					case bs_match_string:
						// LSII:
					case bs_skip_utf8:
					case bs_skip_utf16:
					case bs_skip_utf32:
						// LSIID:
					case bs_start_match2:
					case bs_get_utf8:
					case bs_get_utf16:
					case bs_get_utf32:
						// LSSII:
					case bs_skip_bits2:
						// LSISIID:
					case bs_get_integer2:
					case bs_get_float2:
					case bs_get_binary2:
						accept_2_test(vis, (Insn.L)insn_, insn_idx);
						break;

					case K_return:
						vis.visitInsn(opcode, new Arg(Arg.Kind.X, 0,
								type_map.getx(0)));
						break;

					case allocate_heap_zero:
					case allocate_zero: {
						Insn.I insn = (Insn.I)insn_;
						int depth = type_map.stacksize;
						int count = insn.i1;
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
						
					case recv_mark:
					case recv_set:
						break;

					case call_ext_last:
					case call_ext_only:
						do_call(vis, insn_idx, (Insn.I)insn_, true, true);
						break;

					case call_ext:
						do_call(vis, insn_idx, (Insn.I)insn_, false, true);
						if (is_exceptional_call(insn_)) {
							vis.visitUnreachablePoint();
						}
						break;

					case call:
						do_call(vis, insn_idx, (Insn.I)insn_, false, false);
						break;

					case call_last:
					case call_only:
						do_call(vis, insn_idx, (Insn.I)insn_, true, false);
						break;

					case apply_last:
					case apply: {
						Insn.I insn = (Insn.I) insn_;
						Arg[] args = new Arg[2 + insn.i1];
						for (int i = 0; i < args.length; i++) {
							args[i] = new Arg(Arg.Kind.X, i, map[insn_idx]
									.getx(i));
						}
						vis.visitInsn(opcode, args);
						break;
					}

					case make_fun2: {
						Insn.F insn = (Insn.F) insn_;
						ExtFun efun = insn.anon_fun.asExtFun();
						int numfree = insn.anon_fun.free_vars;
						int index = insn.anon_fun.index;
						int old_index = insn.anon_fun.old_index;
						EBinary uniq = insn.anon_fun.mod_md5;
						int old_uniq = insn.anon_fun.old_uniq;
						
						Arg[] free = new Arg[numfree];
						for (int i = 0; i < numfree; i++) {
							free[i] = new Arg(Arg.Kind.X, i, map[insn_idx]
									.getx(i));
						}
						vis.visitInsn(opcode, efun, free, index, old_index, uniq, old_uniq);
						break;
					}

					case init: {
						Insn.D insn = (Insn.D) insn_;
						vis.visitInsn(opcode, dest_arg(insn_idx, insn.dest));
						break;
					}

					case put_list: {
						Insn.SSD insn = (Insn.SSD) insn_;
						Arg[] in = new Arg[] {
							src_arg(insn_idx, insn.src1),
							src_arg(insn_idx, insn.src2)
						};
						Arg out = dest_arg(insn_idx, insn.dest);

						vis.visitInsn(opcode, in, out);
						break;
					}

					case put_tuple: {
						Insn.ID insn = (Insn.ID) insn_;
						int arity = insn.i1;
						tuple_reg = new Arg(dest_arg(insn_idx, insn.dest),
											getTupleType(arity));
						vis.visitInsn(opcode, arity, tuple_reg);
						tuple_pos = 1;
						break;
					}

					case put: {
						Insn.S insn = (Insn.S) insn_;
						Arg val = src_arg(insn_idx, insn.src);
						vis.visitInsn(opcode, val, tuple_reg, tuple_pos++);
						break;
					}

					case set_tuple_element: {
						Insn.SDI insn = (Insn.SDI) insn_;
						Arg in  = src_arg(insn_idx, insn.src);
						Arg out = src_arg(insn_idx, insn.dest);
						int idx = insn.i;

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
						Insn.Select insn = (Insn.Select) insn_;
						int failLabel = decode_labelref(insn.defaultLabel,
														type_map.exh);
						Arg in = src_arg(insn_idx, insn.src);

						Operands.SelectList jumpTable = insn.jumpTable;
						int len = jumpTable.size();

						int[] arities = new int[len];
						int[] targets = new int[len];

						for (int i=0; i<len; i++) {
							Operands.Operand value = jumpTable.getValue(i);
							Operands.Label target = jumpTable.getLabel(i);

							arities[i] = value.asCodeInt().value;
							targets[i] = decode_labelref(target, type_map.exh);
						}

						vis.visitSelectTuple(in, failLabel, arities, targets);

						break;
					}

					case select_val: {
						Insn.Select insn = (Insn.Select) insn_;
						int failLabel = decode_labelref(insn.defaultLabel,
														type_map.exh);
						Arg in = src_arg(insn_idx, insn.src);

						Operands.SelectList jumpTable = insn.jumpTable;
						int len = jumpTable.size();

						Arg[] values = new Arg[len];
						int[] targets = new int[len];

						for (int i=0; i<len; i++) {
							Operands.Operand value = jumpTable.getValue(i);
							Operands.Label target = jumpTable.getLabel(i);

							values[i] = arg(value.asLiteral());
							targets[i] = decode_labelref(target, type_map.exh);
						}

						vis.visitSelectValue(in, failLabel, values, targets);

						break;
					}

					case get_tuple_element: {
						Insn.SID insn = (Insn.SID) insn_;

						Arg in  = src_arg(insn_idx, insn.src);
						int idx = insn.i;
						Arg out = dest_arg(insn_idx, insn.dest);

						vis.visitInsn(opcode, in, out, idx);
						break;
					}

					case jump: {
						Insn.L insn = (Insn.L) insn_;
						vis.visitJump(decode_labelref(insn.label, type_map.exh));
						break;
					}

					case on_load: // ignore
					case trim:
						break;

					case get_list: {
						Insn.SDD insn = (Insn.SDD) insn_;
						vis.visitInsn(opcode, new Arg[] {
								src_arg(insn_idx, insn.src),
								dest_arg(insn_idx, insn.dest1),
								dest_arg(insn_idx, insn.dest2) });
						break;
					}

					case try_case_end:
					case badmatch:
					case case_end: {
						Insn.S insn = (Insn.S) insn_;
						vis.visitInsn(opcode, src_arg(insn_idx, insn.src));
						break;
					}

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
						Insn.YL insn = (Insn.YL) insn_;
						TypeMap type_map_after = this.map[insn_idx+1];
						vis.visitCatchBlockStart(opcode,
												 decode_labelref(insn.label, type_map.exh),
												 src_arg(insn_idx, insn.y),
												 type_map_after.exh);
						break;
					}

					case raise: {
						Insn.SS insn = (Insn.SS) insn_;
						Arg[] in = {src_arg(insn_idx, insn.src1),
									src_arg(insn_idx, insn.src2) };
						Arg ex = new Arg(Arg.Kind.X, 0);
						int failLabel = 0;

						// Half of the args are constants...
						vis.visitInsn(opcode, failLabel, in, ex);
						break;
					}

					case try_end:
					case try_case:
					case catch_end: {
						Insn.Y insn = (Insn.Y) insn_;
						vis.visitCatchBlockEnd(opcode,
								       src_arg(insn_idx, insn.y),
								       type_map.exh);
						break;
					}

					case loop_rec: { /* loop receive */
						Insn.LD insn = (Insn.LD) insn_;
						vis.visitReceive(opcode,
										 decode_labelref(insn.label, type_map.exh),
										 dest_arg(insn_idx, insn.dest));
						break;
					}

					case remove_message:
					case timeout:
						vis.visitInsn(opcode);
						break;

					case loop_rec_end:
					case wait: {
						Insn.L insn = (Insn.L) insn_;
						vis.visitInsn(opcode, decode_labelref(insn.label, type_map.exh), null);
						break;
					}

					case wait_timeout: {
						Insn.LS insn = (Insn.LS) insn_;
						vis.visitInsn(opcode,
									  decode_labelref(insn.label, type_map.exh),
									  src_arg(insn_idx, insn.src));
						break;
					}

					case call_fun: {
						Insn.I insn = (Insn.I) insn_;
						int nargs = insn.i1;
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
						Insn.LSSID insn = (Insn.LSSID) insn_;
						vis.visitBSAdd(src_arg(insn_idx, insn.src1),
									   src_arg(insn_idx, insn.src2),
									   insn.i3,
									   dest_arg(insn_idx, insn.dest));
						break;
					}

					case bs_context_to_binary: {
						Insn.D insn = (Insn.D) insn_;
						vis.visitBS(opcode, dest_arg(insn_idx, insn.dest), null);
						// do nothing for now
						break;
					}

					case bs_restore2:
					case bs_save2: {
						Insn.DI insn = (Insn.DI) insn_;
						vis.visitBS(opcode, src_arg(insn_idx, insn.dest),
									//TODO: streamline - change API
									insn.i2 == -1
									? new Arg(EAtom.intern("start"))
									: new Arg(new ESmall(insn.i2))
							);
						break;
					}

					case bs_init_writable: {
						Arg size = src_arg(insn_idx, new Operands.XReg(0));
						Arg dest = dest_arg(new Operands.XReg(0));
						
						vis.visitInitWritable(size, dest);
						
						break;
					}
					
					case bs_init2:
					case bs_init_bits: {
						Insn.LSIIID insn = (Insn.LSIIID) insn_;
						Arg size  = src_arg(insn_idx, insn.src2);
						int flags = insn.i5;
						Arg out   = dest_arg(insn_idx, insn.dest);
						boolean unit_is_bits = (opcode == BeamOpcode.bs_init_bits);

						vis.visitInitBitString(size, flags, out,
											   unit_is_bits);
						break;
					}

					case bs_put_string: {
						Insn.By insn = (Insn.By) insn_;
						Arg str = arg(insn.bin);
						vis.visitBitStringPut(opcode, str, null,-1,-1);
						break;
					}

					case bs_put_binary:
					case bs_put_integer:
					case bs_put_float:
					{
						Insn.LSIIS insn = (Insn.LSIIS) insn_;
						Arg size = src_arg(insn_idx, insn.src2);
						int unit = insn.i3;
						int flags = insn.i4;
						Arg value = src_arg(insn_idx, insn.src5);
						vis.visitBitStringPut(opcode, value, size, unit, flags);
						break;
					}

					case bs_put_utf8:
					case bs_put_utf16:
					case bs_put_utf32:
					{
						Insn.LIS insn = (Insn.LIS) insn_;
						int flags = insn.i2;
						Arg value = src_arg(insn_idx, insn.src);
						//TODO: is the label always 0? (Op may fail on bad chars)
						vis.visitBitStringPut(opcode, value, null, -1, flags);
						break;
					}

					case bs_utf8_size:
					case bs_utf16_size: {
						Insn.LSD insn = (Insn.LSD) insn_;
						Arg value = src_arg(insn_idx, insn.src);
						Arg out   = dest_arg(insn_idx, insn.dest);
						//TODO: is the label always 0? (Op may fail)
						vis.visitBS(opcode, value, out);
						break;
					}

					case bs_private_append: {
						Insn.BSPrivateAppend insn = (Insn.BSPrivateAppend) insn_;
						// {bs_private_append,{f,0},{integer,8},1,{y,0},{field_flags,[]},{x,1}}.
						//  * tmp_arg1 = Number of bytes to build
						//  * tmp_arg2 = Source binary
						//  * Operands: Fail Unit Dst
						
						Arg extra_size = src_arg(insn_idx, insn.src2);
						Arg src = src_arg(insn_idx, insn.src4);
						int unit = insn.i3;
						int flags = insn.i5;
						Arg dst = dest_arg(insn_idx, insn.dest);
						
						vis.visitBitStringAppend(opcode, decode_labelref(insn.label, type_map.exh), extra_size, src, unit, flags, dst);
						break;
					}
					
					case bs_append: {
						Insn.BSAppend insn = (Insn.BSAppend) insn_;
						//     {bs_append,{f,0},{integer,32},0,3,8,{x,1},{field_flags,[]},{x,0}}.
						//   * tmp_arg1 = Number of bytes to build
						//   * tmp_arg2 = Source binary
						//   * Operands: Fail ExtraHeap Live Unit Dst

						Arg extra_size = src_arg(insn_idx, insn.src2);
						Arg src = src_arg(insn_idx, insn.src6);
						int unit = insn.i5;
						int flags = insn.i7;
						Arg dst = dest_arg(insn_idx, insn.dest8);

						vis.visitBitStringAppend(opcode, decode_labelref(insn.label, type_map.exh), extra_size, src, unit, flags, dst);
						break;
					}
					default:
						throw new Error("unhandled insn: " + insn_.toSymbolicTuple());
					}

				}
			}

			private void do_call(BlockVisitor2 vis, int insn_idx, Insn.I insn,
					boolean is_tail, boolean is_external) throws Error {
				int arg_count = insn.i1;
				Arg[] args = new Arg[arg_count];
				for (int i = 0; i < arg_count; i++) {
					args[i] = new Arg(Kind.X, i, this.map[insn_idx].getx(i));
				}

				ExtFun fun;
				if (insn instanceof Insn.IE) {
					fun = ((Insn.IE)insn).ext_fun;
				} else if (insn instanceof Insn.IL) {
					fun = ((Insn.IL)insn).functionAtLabel.asExtFun();
				} else
					throw new Error("unexpected in do_call: "+insn);

				vis.visitCall(fun, args, is_tail, is_external);
			}

			private void accept_2_test(BlockVisitor2 vis, Insn.L insn_, int insn_idx) {

				TypeMap typeMap = this.map[insn_idx];
				int failLabel = decode_labelref(insn_.label, typeMap.exh);
				BeamOpcode test = insn_.opcode();

				if (insn_ instanceof Insn.LD) { // Handle simple type tests:
					Insn.LD insn = (Insn.LD)insn_;
					Arg arg = src_arg(insn_idx, insn.dest);
					Type test_type = type_tested_for(insn);
					if (test_type != null) {
						if (insn_.opcode() == BeamOpcode.is_nonempty_list 
								|| !test_type.equals(arg.type))
							vis.visitTest(test, failLabel, arg, test_type);
						return;
					}
				}

				switch (test) {
				case is_function2: {
					Insn.LDS insn = (Insn.LDS) insn_;
					vis.visitTest(test, failLabel,
								  dest_arg(insn_idx, insn.dest),
								  src_arg(insn_idx,insn.src),
								  EFUN_TYPE);
					break;
				}

				case is_eq_exact:
					/* hack to convert types ... */

				case is_lt:
				case is_ge:
				case is_ne_exact:
				case is_ne:
				case is_eq: {
					Insn.LSS insn = (Insn.LSS) insn_;
					Arg[] args = new Arg[] {
						src_arg(insn_idx, insn.src1),
						src_arg(insn_idx, insn.src2) };

					Type outType = Type.VOID_TYPE;
					Type t1 = getType(typeMap, insn.src1);
					Type t2 = getType(typeMap, insn.src2);

					if (t1.equals(EOBJECT_TYPE) && !t2.equals(EOBJECT_TYPE)) {
						outType = t2;
					}
					
					if (t2.equals(EOBJECT_TYPE) && !t1.equals(EOBJECT_TYPE)) {
						outType = t1;
					}
					
					vis.visitTest(test, failLabel, args, outType);
										
					break;
				}

				case test_arity: {
					Insn.LDI insn = (Insn.LDI) insn_;
					int arity = insn.i;
					Arg reg = src_arg(insn_idx, insn.dest);
					vis.visitTest(test, failLabel, reg, arity,
							getTupleType(arity));
					break;
				}

				case bs_start_match2:
				case bs_get_utf8:
				case bs_get_utf16:
				case bs_get_utf32: {
					Insn.LDIID insn = (Insn.LDIID) insn_;
					vis.visitBitStringTest(test, failLabel,
										   src_arg(insn_idx, insn.dest),
										   insn.i4,
										   dest_arg(insn_idx, insn.dest5));
					break;
				}

				case bs_match_string: {
					Insn.LDBi insn = (Insn.LDBi) insn_;
					vis.visitBitStringTest(test, failLabel,
										   src_arg(insn_idx, insn.dest),
										   insn.bin.value);
					break;
				}

				case bs_get_integer2:
				case bs_get_float2:
				case bs_get_binary2: {
					Insn.LDISIID insn = (Insn.LDISIID) insn_;
					vis.visitBitStringTest(test, failLabel,
										   src_arg(insn_idx, insn.dest),
										   src_arg(insn_idx, insn.src4),
										   insn.i5,
										   insn.i6,
										   dest_arg(insn_idx, insn.dest7));
					break;
				}
				case bs_skip_bits2: {
					Insn.LDSII insn = (Insn.LDSII) insn_;
					vis.visitBitStringTest(test, failLabel,
										   src_arg(insn_idx, insn.dest),
										   src_arg(insn_idx, insn.src3),
										   insn.i4,
										   insn.i5);
					break;
				}
				case bs_test_unit:
				case bs_test_tail2: {
					Insn.LDI insn = (Insn.LDI) insn_;
					vis.visitBitStringTest(test, failLabel,
										   src_arg(insn_idx, insn.dest),
										   insn.i);
					break;
				}

				case bs_skip_utf8:
				case bs_skip_utf16:
				case bs_skip_utf32: {
					Insn.LDII insn = (Insn.LDII) insn_;
					vis.visitBitStringTest(test, failLabel,
										   src_arg(insn_idx, insn.dest),
										   insn.i4);
					break;
				}
				default:
					throw new Error("unhandled test: " + insn_.toSymbolic() +
									" at index " + insn_idx +
									" // " + test + ":" + test.ordinal());
				}//switch

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

			private Arg[] src_args(int insn_idx, SourceOperand[] args) {
				Arg[] res = new Arg[args.length];
				for (int i=0; i<args.length; i++) {
					res[i] = src_arg(insn_idx, args[i]);
				}
				return res;
			}

			private Arg src_arg(int insn_idx, SourceOperand src) {
				TypeMap current = this.map[insn_idx];

				if (src instanceof Operands.XReg)
					return src_arg((Operands.XReg) src, current);
				if (src instanceof Operands.YReg)
					return src_arg((Operands.YReg) src, current);
				if (src instanceof Operands.FReg)
					return arg((Operands.FReg) src);
				if (src instanceof Operands.Float)
					return arg((Operands.Float) src);
				if (src instanceof Operands.Literal)
					return arg((Operands.Literal) src);

				throw new Error("Unhandled src_arg: "+src.toSymbolic());
			}

			private Arg dest_arg(int insn_idx, DestinationOperand dest) {
				TypeMap current = this.map[insn_idx];

				if (dest instanceof Operands.XReg)
					return dest_arg((Operands.XReg) dest);
				if (dest instanceof Operands.YReg)
					return dest_arg((Operands.YReg) dest, current);
				if (dest instanceof Operands.FReg)
					return arg((Operands.FReg) dest);

				throw new Error("Unhandled dest_arg: "+dest.toSymbolic());
			}

			private Arg src_arg(Operands.XReg xreg, TypeMap tm) {
				return new Arg(Kind.X, xreg.nr, tm.getx(xreg.nr));
			}
			private Arg src_arg(Operands.YReg yreg, TypeMap tm) {
				return new Arg(Kind.Y, tm.get_ypos(yreg.nr), tm.gety(yreg.nr));
			}

			private Arg dest_arg(Operands.XReg xreg) {
				return new Arg(Kind.X, xreg.nr);
			}
			private Arg dest_arg(Operands.YReg yreg, TypeMap tm) {
				return new Arg(Kind.Y, tm.get_ypos(yreg.nr));
			}

			private Arg arg(Operands.FReg freg) {
				return new Arg(Kind.F, freg.nr);
			}
			private Arg arg(Operands.Float flt) {
				return new Arg(flt.literalValue(), Type.DOUBLE_TYPE);
			}
			private Arg arg(Operands.Literal lit) {
				return new Arg(lit.literalValue());
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

			private int decode_labelref(Operands.Label label, ExceptionHandler exh) {
				if (label == null) return 0;
				return extendedLabel(label.nr, exh);
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
				if (ERT.DEBUG2 == false) return;
				next_insn: for (int i = 0; i < insns.size(); i++) {
					System.err.println(name + "(" + block_label + "):" + i
							+ " :: " + (map == null ? "?" : map[i]));
					System.err.println("     >> " + insns.get(i));
				}
			}

			public void analyze0() {
				TypeMap current = initial;
				BeamOpcode last_opcode = BeamOpcode.NONE;
				Insn last_insn = null;

				map = new TypeMap[insns.size()];

				next_insn: for (int insn_idx = 0; insn_idx < insns.size(); insn_idx++) {

					update_max_regs(current);

					if (is_term(last_opcode)) {
						throw new Error("how did we get here then...? "
								+ this.block_label + ":" + insn_idx);
					}

					map[insn_idx] = current;
					Insn insn_ = insns.get(insn_idx);
					BeamOpcode code = insn_.opcode();
					last_opcode = code; last_insn = insn_;
					/*
					 * System.out.println(name + "(" + bb_label + "):" + i +
					 * " :: " + current + "" + insn);
					 */

					if (current.exh != null && may_terminate_exceptionally(code))
						addExceptionEdge(current);

					switch (code) {
					case fmove:
					case move: {
						Insn.SD insn = (Insn.SD) insn_;
						SourceOperand src = insn.src;
						DestinationOperand dst = insn.dest;

						Type srcType = getType(current, src);

						// Determine type after possible conversion:
						Type dstType = srcType;
						if (dst.testFReg() != null) {
							dstType = Type.DOUBLE_TYPE; // FRegs are always unboxed
						} else if (sizeof(current, src) > sizeof(current, dst)) { // Conversion needed
							if (srcType.equals(Type.DOUBLE_TYPE)) {
								dstType = EDOUBLE_TYPE; // Box
							} else {
								throw new Error("why?" + insn.toSymbolic()
										+ "; srcType="+getType(current,src));
							}
						}

						current = setType(current, dst, dstType);
						continue next_insn;
					}
					case put_string: {
						Insn.ByD insn = (Insn.ByD) insn_;
						DestinationOperand dst = insn.dest;
						current = setType(current, dst, ESEQ_TYPE);
						continue next_insn;
					}

					case jump: {
						Insn.L insn = (Insn.L) insn_;
						current = branch(current, insn.label, insn_idx);
						continue next_insn;

					}

					case send: {
						current.touchx(0, 2);
						current = current.setx(0, current.getx(1), FV.this);
						continue next_insn;
					}

					case fadd:
					case fsub:
					case fmul:
					case fdiv:
					{
						Insn.LSSD insn = (Insn.LSSD) insn_;
						EAtom name = insn.opcode().symbol;
						SourceOperand[] parms = new SourceOperand[] {
							insn.src1, insn.src2
						};
						Type type = getBifResult("erlang", name.getName(),
												 parmTypes(current, parms), false);
						current = setType(current, insn.dest, type);

						continue next_insn;
					}

 					case gc_bif1:
 					case gc_bif2:
					{
						// {gc_bif,BifName,F,Live,[A1,A2?],Reg};

						Insn.GcBif insn = (Insn.GcBif) insn_;
						boolean is_guard = (insn.label.nr != 0);

						current = branch(current, insn.label, insn_idx);

						EAtom name = insn.ext_fun.fun;
						SourceOperand[] parms = insn.argList();

						Type type = getBifResult("erlang", name.getName(),
												 parmTypes(current, parms), is_guard);

						current = setType(current, insn.dest, type);

						continue next_insn;
					}

					case bif0:
					case bif1:
					case bif2:
					{
						Insn.Bif insn = (Insn.Bif) insn_;
						current = branch(current, insn.label, insn_idx);

						EAtom name = insn.ext_fun.fun;
						SourceOperand[] parms = insn.argList();

						Type type = getBifResult("erlang", name.getName(),
												 parmTypes(current, parms), false);

						current = setType(current, insn.dest, type);

						continue next_insn;
					}


					case is_tuple: {
						
						if (insn_idx+1 < insns.size()) {
						Insn next_insn = insns.get(insn_idx+1);
						if (next_insn.opcode() == BeamOpcode.test_arity) {
							
							if (this.map[insn_idx+1] == null) {
								this.map[insn_idx+1] = this.map[insn_idx];
							}
							
							int this_fail = decode_labelref(((Insn.L)insn_).label, this.map[insn_idx].exh);
							int next_fail = decode_labelref(((Insn.L)next_insn).label, this.map[insn_idx+1].exh);

							if (this_fail == next_fail) {

								Arg this_arg = src_arg(insn_idx, ((Insn.LD)insn_).dest);
								Arg next_arg = src_arg(insn_idx+1, ((Insn.LD)next_insn).dest);

								if (this_arg.equals(next_arg)) { 
									// SKIP THIS INSTRUCTION!									
									continue next_insn;
								}
							}
						}
						}
						
					}
						

						// Tests:
						// LS:
					case is_integer:
					case is_float:
					case is_number:
					case is_atom:
					case is_pid:
					case is_reference:
					case is_port:
					case is_nil:
					case is_binary:
					case is_list:
					case is_nonempty_list:
					case is_function:
					case is_boolean:
					case is_bitstr:
						// LSI:
					case test_arity:
					case bs_test_tail2:
					case bs_test_unit:
						// LSS:
					case is_lt:
					case is_ge:
					case is_eq:
					case is_ne:
					case is_eq_exact:
					case is_ne_exact:
					case is_function2:
						// LSBi:
					case bs_match_string:
						// LSII:
					case bs_skip_utf8:
					case bs_skip_utf16:
					case bs_skip_utf32:
						// LSIID:
					case bs_start_match2:
					case bs_get_utf8:
					case bs_get_utf16:
					case bs_get_utf32:
						// LSSII:
					case bs_skip_bits2:
						// LSISIID:
					case bs_get_integer2:
					case bs_get_float2:
					case bs_get_binary2:
					{
						try {
							current = analyze_test(current, (Insn.L)insn_, insn_idx);
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
						Insn.SD insn = (Insn.SD) insn_;
						getType(current, insn.src);
						current = setType(current, insn.dest,
										  Type.DOUBLE_TYPE);
						continue next_insn;
					}

					case init: {
						Insn.D insn = (Insn.D) insn_;
						current = setType(current, insn.dest, ENIL_TYPE);
						continue next_insn;
					}

					case set_tuple_element: {
						Insn.SDI insn = (Insn.SDI) insn_;
						getType(current, insn.src);
						getType(current, insn.dest);
						continue next_insn;
					}

					case get_tuple_element: {
						Insn.SID insn = (Insn.SID) insn_;
						getType(current, insn.src);
						current = setType(current, insn.dest, EOBJECT_TYPE);
						continue next_insn;
					}

					case get_list: {
						Insn.SDD insn = (Insn.SDD) insn_;
						current = setType(current, insn.dest1, EOBJECT_TYPE);

						Type list_type = getType(current, insn.src);
						Type tail_type =
							(list_type == ELIST_TYPE || list_type == ESEQ_TYPE)
							? ESEQ_TYPE : EOBJECT_TYPE;
						current = setType(current, insn.dest2, tail_type);

						continue next_insn;
					}

					case put_list: {
						Insn.SSD insn = (Insn.SSD) insn_;

						Type head_type = getType(current, insn.src1);
						Type tail_type = getType(current, insn.src2);

						if (tail_type == null) {
							throw new Error("value: " + insn.src2.toSymbolic()
									+ " has no type");
						}

						Type list_type = (tail_type.equals(ENIL_TYPE)
										  || tail_type.equals(ESEQ_TYPE)
										  || tail_type.equals(ELIST_TYPE))
							? ELIST_TYPE : ECONS_TYPE;
						current = setType(current, insn.dest, list_type);

						continue next_insn;
					}

					case put_tuple: {
						Insn.ID insn = (Insn.ID) insn_;
						int arity = insn.i1;
						current = setType(current, insn.dest, getTupleType(arity));
						continue next_insn;
					}

					case K_try: {
						Insn.YL insn = (Insn.YL) insn_;
						current = setType(current, insn.y, EOBJECT_TYPE);
						current = installExceptionHandler(current, insn.label, insn_idx);
						continue next_insn;
					}

					case try_end: {
						current = current.popExceptionHandler();
						continue next_insn;
					}

					case try_case: {
						Insn.Y insn = (Insn.Y) insn_;
						getType(current, insn.y);
						current = current.popExceptionHandler();
						current = current.setx(0, EATOM_TYPE, FV.this); // exc.class
						current = current.setx(1, EOBJECT_TYPE, FV.this); // value/reason
						current = current.setx(2, EOBJECT_TYPE, FV.this); // exc.object
						continue next_insn;
					}

					case try_case_end:
						continue next_insn;

					case raise: {
						Insn.SS insn = (Insn.SS) insn_;

						checkArg(current, insn.src1);
						checkArg(current, insn.src2);
						current = setType(current, Operands.XReg.get(0), EOBJECT_TYPE);
						continue next_insn;
					}

					case K_catch: {
						Insn.YL insn = (Insn.YL) insn_;
						current = installExceptionHandler(current, insn.label, insn_idx);
						continue next_insn;
					}

					case catch_end: {
						current = current.popExceptionHandler();
						current = current.setx(0, EOBJECT_TYPE, FV.this); // value
						continue next_insn;
					}

					case make_fun2: {
						Insn.F insn = (Insn.F) insn_;
						current.touchx(0, insn.anon_fun.free_vars);
						current = current.setx(0, EFUN_TYPE, FV.this);
						continue next_insn;
					}

					case loop_rec: {
						Insn.LD insn = (Insn.LD) insn_;
						current = branch(current, insn.label, insn_idx);
						current = setType(current, insn.dest, EOBJECT_TYPE);
						continue next_insn;
					}

					case remove_message:
						// assume this insn overrides X0
						// current = current.setx(0, EOBJECT_TYPE, FV.this);
						continue next_insn;

					case loop_rec_end:
					case timeout: {
						// System.err.println(insn);
						continue next_insn;
					}

					case wait_timeout: {
						checkArg(current, ((Insn.LS)insn_).src);
					} // fall-through
					case wait: {
						Insn.L insn = (Insn.L) insn_;
						current = branch(current, insn.label, insn_idx);
						continue next_insn;
					}

					case deallocate:
					case trim: {
						Insn.I insn = (Insn.I) insn_;
						int howmuch = insn.i1;
						current = current.trim_y(howmuch);
						continue next_insn;
					}

						// this is really a no-op in jave
					case on_load:
					case test_heap: {
						continue next_insn;
					}

					case allocate_zero:
					case allocate_heap_zero: {
						Insn.I insn = (Insn.I) insn_;
						int slots = insn.i1;
						current = current.alloc_y(slots);
						for (int slot = 0; slot < slots; slot++) {
							current = current.sety(slot, ENIL_TYPE);
						}
						continue next_insn;
					}

					case allocate:
					case allocate_heap: {
						Insn.I insn = (Insn.I) insn_;
						current = current.alloc_y(insn.i1);
						continue next_insn;
					}

					case fcheckerror:
					case fclearerror:
						continue next_insn;
						
					case recv_mark:
					case recv_set:
						continue next_insn;

						
					case put: {
						Insn.S insn = (Insn.S) insn_;
						checkArg(current, insn.src);
						continue next_insn;
					}

					case select_tuple_arity: {
						Insn.Select insn = (Insn.Select) insn_;
						current = branch(current, insn.defaultLabel, insn_idx);

						checkArg(current, insn.src);

						DestinationOperand dest = insn.src.testDestination();
						Operands.SelectList jumpTable = insn.jumpTable;
						int len = jumpTable.size();
						for (int i=0; i<len; i++) {
							Operands.Operand value = jumpTable.getValue(i);
							Operands.Label target = jumpTable.getLabel(i);

							if (dest != null) {
								int arity = value.asCodeInt().value;
								current = setType(current, dest, getTupleType(arity));
							}
							current = branch(current, target, insn_idx);
						}

						continue next_insn;
					}

					case select_val: {
						Insn.Select insn = (Insn.Select) insn_;
						current = branch(current, insn.defaultLabel, insn_idx);

						checkArg(current, insn.src);

						Operands.SelectList jumpTable = insn.jumpTable;
						int len = jumpTable.size();
						for (int i=0; i<len; i++) {
							Operands.Label target = jumpTable.getLabel(i);
							//TODO: Set the known type of 'src'
							current = branch(current, target, insn_idx);
						}

						continue next_insn;
					}

						// we loose the type of the result
					case apply:
					case call:
					case call_ext: {
						Insn.I insn = (Insn.I) insn_;
						int argCount = insn.i1;
						current.touchx(0, argCount);
						current = current.setx(0, EOBJECT_TYPE, FV.this);
						continue next_insn;
					}

						// all these exit
					case K_return:
						getType(current, X0_REG);
						continue next_insn;

					case apply_last:
					case call_last:
					case call_only:
					case call_ext_last:
					case call_ext_only: {
						Insn.I insn = (Insn.I) insn_;
						int argCount = insn.i1;
						current.touchx(0, argCount);
						is_tail_recursive = true;
						continue next_insn;
					}

					case func_info:
						continue next_insn;

					case if_end:
					case badmatch:
					case case_end:
						continue next_insn;

					case bs_add: {
						Insn.LSSID insn = (Insn.LSSID) insn_;
						checkArg(current, insn.src1);
						checkArg(current, insn.src2);
						current = setType(current, insn.dest, Type.INT_TYPE);
						continue next_insn;
					}

					case bs_context_to_binary: {
						Insn.D insn = (Insn.D) insn_;
						checkArg(current, insn.dest);
						current = current.setx(0, EBINARY_TYPE, FV.this);
						continue next_insn;
					}

					case bs_save2: {
						continue next_insn;
					}

					case bs_restore2: {
						Insn.DI insn = (Insn.DI) insn_;
						current = setType(current, insn.dest, EMATCHSTATE_TYPE);
						continue next_insn;
					}

					case bs_init2: {
						Insn.LSIIID insn = (Insn.LSIIID) insn_;
						// int size = insn.elm(3).asInt();
						current = setType(current, insn.dest, EBINARY_TYPE);
						continue next_insn;
					}

					case bs_init_bits: {
						Insn.LSIIID insn = (Insn.LSIIID) insn_;
						// int size = insn.elm(3).asInt();
						current = setType(current, insn.dest, EBITSTRING_TYPE);
						continue next_insn;
					}

					case bs_init_writable: {
						XReg x0 = new Operands.XReg(0);
						checkArg(current, x0);
						current = setType(current, x0, EBITSTRING_TYPE);
						continue next_insn;
					}
					
					case bs_private_append: {
						Insn.BSPrivateAppend insn = (Insn.BSPrivateAppend)insn_;
						checkArg(current, insn.src2);
						checkArg(current, insn.src4);
						current = setType(current, insn.dest, EBITSTRING_TYPE);
						continue next_insn;	
					}
					
					case bs_append: {
						// {bs_append,{f,0},{integer,32},0,3,8,{x,1},{field_flags,[]},{x,0}}.
						Insn.BSAppend insn = (Insn.BSAppend)insn_;
						// Arg extra_size = decode_arg(insn_idx, insn.elm(3));
						// Arg src = decode_arg(insn_idx, insn.elm(7));
						// Arg flags = decode_arg(insn_idx, insn.elm(8));
						// Arg dst = decode_out_arg(insn_idx, insn.elm(9));

						checkArg(current, insn.src6);
						current = setType(current, insn.dest8, EBITSTRING_TYPE);
						continue next_insn;
					}

					case bs_put_string:
					case bs_put_binary:
					case bs_put_float:
					case bs_put_integer: {
						continue next_insn;
					}

					case bs_put_utf8:
					case bs_put_utf16:
					case bs_put_utf32: {
						Insn.LIS insn = (Insn.LIS)insn_;
						// make sure there is a source
						checkArg(current, insn.src);
						continue next_insn;
					}

					case bs_utf8_size:
					case bs_utf16_size: {
						// {bs_utf16_size,{f,0},src={x,0},dst={x,2}}
						Insn.LSD insn = (Insn.LSD)insn_;
						checkArg(current, insn.src);
						current = setType(current, insn.dest, ESMALL_TYPE);
						continue next_insn;
					}

					case call_fun: {
						Insn.I insn = (Insn.I)insn_;
						int nargs = insn.i1;
						for (int i = 0; i < nargs; i++) {
							if (current.getx(i) == null)
								throw new Error("uninitialized x" + i);
						}
						current = current.setx(0, EOBJECT_TYPE, FV.this);
						continue next_insn;
					}

					default: {
						ETuple insn = insn_.toSymbolicTuple();
						throw new Error("unhandled: " + insn + "::" + current);
					}
					}//switch
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

			boolean is_term(BeamOpcode code) {
				switch (code) {
				case K_return:
				case if_end:
				case badmatch:
				case case_end:
				case try_case_end:
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

			boolean is_exceptional_call(Insn insn) {
				BeamOpcode opcode = insn.opcode();
				if (opcode == BeamOpcode.call_ext) {
					Insn.IE spec_insn = (Insn.IE)insn;
					ExtFun ext_fun = spec_insn.ext_fun;

					if (ext_fun.mod == ERLANG_ATOM &&
					    (ext_fun.fun == ERROR_ATOM 
					    || ext_fun.fun == THROW_ATOM
					    || ext_fun.fun == EXIT_ATOM) &&
					    ext_fun.arity == 1) return true;
				}
				return false;
			}

			private int sizeof(TypeMap current, Operands.SourceOperand cell) {
				if (cell instanceof Operands.XReg ||
					cell instanceof Operands.YReg)
					return 32;
				if (cell instanceof Operands.FReg)
					return 64;

				Type t = getType(current, cell);
				if (t == Type.DOUBLE_TYPE) {
					return 64;
				} else {
					return 32;
				}
			}

			private Type getBifResult(String module, String name, Type[] parmTypes,
					boolean is_guard) {
				return BIFUtil.getBifResult(module, name, parmTypes, is_guard);
			}

			@Deprecated
			private Type[] parmTypes(TypeMap current, ESeq args) {
				ArrayList<Type> res = new ArrayList<Type>();

				while (args != ERT.NIL) {
					EObject arg = args.head();
					res.add(getType(current, arg));
					args = args.tail();
				}

				return res.toArray(new Type[res.size()]);
			}

			private Type[] parmTypes(TypeMap current, SourceOperand[] args) {
				Type[] res = new Type[args.length];

				for (int i=0; i<args.length; i++) {
					SourceOperand arg = args[i];
					Type argType = getType(current, arg);
					if (argType == null) {
						throw new Error("uninitialized " + arg);
					}
					res[i] = argType;
				}

				return res;
			}

			private void checkArg(TypeMap current, SourceOperand arg) {
				Type argType = getType(current, arg);
				if (argType == null) {
					throw new Error("uninitialized " + arg);
				}
			}

			private TypeMap analyze_test(TypeMap current, Insn.L insn_, int insn_idx) {
				current = branch(current, insn_.label, insn_idx);

				BeamOpcode opcode = insn_.opcode();
				switch (opcode) {
				case is_lt:
				case is_ge:
				case is_ne:
				case is_eq:
				case is_ne_exact: {
					Insn.LSS insn = (Insn.LSS) insn_;
					checkArg(current, insn.src1);
					checkArg(current, insn.src2);
					return current;
				}

				case is_eq_exact: {
					Insn.LSS insn = (Insn.LSS) insn_;
					checkArg(current, insn.src1);
					checkArg(current, insn.src2);

					Type t1 = getType(current, insn.src1);
					Type t2 = getType(current, insn.src2);

					if (!t1.equals(t2)) {
						//TODO: for reg-vs-reg, we should really use the GLB.
						DestinationOperand reg;
						if ((reg = insn.src1.testDestination()) != null) {
							current = setType(current, reg, t2);
						}
						if ((reg = insn.src2.testDestination()) != null) {
							current = setType(current, reg, t1);
						}
					}

					return current;
				}

				case bs_start_match2: {
					Insn.LDIID insn = (Insn.LDIID) insn_;
					checkArg(current, insn.dest);
					return setType(current, insn.dest5, EMATCHSTATE_TYPE);
				}

				case bs_get_integer2: {
					Insn.LDISIID insn = (Insn.LDISIID) insn_;
					if (!EMATCHSTATE_TYPE.equals(getType(current, insn.dest))) {
						throw new Error("matching without a state");
					}

					/* DISABLED because it triggers a get_test_bif()-related bug
					if (insn.i5 <= 32) {
						return setType(current, insn.dest7, Type.INT_TYPE);
					}
					*/

					return setType(current, insn.dest7, EINTEGER_TYPE);
				}

				case bs_get_binary2: {
					Insn.LDISIID insn = (Insn.LDISIID) insn_;
					if (!EMATCHSTATE_TYPE.equals(getType(current, insn.dest))) {
						throw new Error("matching without a state");
					}

					return setType(current, insn.dest7, EBINARY_TYPE);
				}


				case bs_get_float2: {
					Insn.LDISIID insn = (Insn.LDISIID) insn_;
					if (!EMATCHSTATE_TYPE.equals(getType(current, insn.dest))) {
						throw new Error("matching without a state");
					}

					return setType(current, insn.dest7, Type.DOUBLE_TYPE);
				}

				case bs_test_tail2:
				case bs_test_unit:
				case bs_skip_bits2:
				case bs_match_string:
				case bs_skip_utf8:
				case bs_skip_utf16:
				case bs_skip_utf32: {
					// These bit string matchers don't modify registers.
					Insn.LD insn = (Insn.LD) insn_;
					if (!EMATCHSTATE_TYPE.equals(getType(current, insn.dest))) {
						throw new Error("matching without a state");
					}

					return current;
				}

				case bs_get_utf8:
				case bs_get_utf16:
				case bs_get_utf32: {
					// These bit string matchers don't modify registers.
					Insn.LDIID insn = (Insn.LDIID) insn_;
					if (!EMATCHSTATE_TYPE.equals(getType(current, insn.dest))) {
						throw new Error("matching without a state");
					}
					return setType(current, insn.dest5, ESMALL_TYPE);
				}

				default: { // All type tests:
					Insn.LD insn = (Insn.LD) insn_;
					checkArg(current, insn.dest);
					switch (opcode) {
					case test_arity: {
						int arity = ((Insn.LDI)insn).i;
						return setType(current, insn.dest, getTupleType(arity));
					}

					case is_function2: {
						Insn.LDS insn2 = (Insn.LDS) insn;
						checkArg(current, insn2.src);
						// TODO: Use more specific type when arity is known?
						return setType(current, insn.dest, EFUN_TYPE);
					}

					default: {
						if (insn instanceof Insn.LD) {
							Type test_type = type_tested_for((Insn.LD)insn);
							if (test_type != null)
								return setType(current, insn.dest, test_type);
						}
						throw new Error("unhandled test: " + insn_.toSymbolic());
					}
					}//switch
				}
				}//switch
			}

			private Type type_tested_for(Insn.LD insn) {
				switch (insn.opcode()) {
				case is_nil:       return ENIL_TYPE;
				case is_binary:    return EBINARY_TYPE;
				case is_tuple:     return ETUPLE_TYPE;
				case is_integer:   return EINTEGER_TYPE;
				case is_bitstr:    return EBITSTRING_TYPE;
				case is_number:    return ENUMBER_TYPE;
				case is_pid:       return EPID_TYPE;
				case is_port:      return EPORT_TYPE;
				case is_reference: return EREFERENCE_TYPE;
				case is_float:     return EDOUBLE_TYPE;
				case is_function:  return EFUN_TYPE;

				case is_list:
				case is_nonempty_list:
					return ECONS_TYPE;

				case is_boolean:
				case is_atom:
					return EATOM_TYPE;

				default:
					return null;
				}//switch
			}

			private TypeMap branch(TypeMap current, Operands.Label target, int idx) {
				return branch(current, target==null? -1 : target.nr, idx);
			}

			private TypeMap branch(TypeMap current, int target, int idx) {
				if (target > 0) {
					get_lb(target, false).merge_from(current);
				}
				return current.clearLive(makeBasicBlock(block_label, idx + 1));
			}

			private TypeMap installExceptionHandler(TypeMap current, Operands.Label target, int idx) {
				 TypeMap afterPush = current.pushExceptionHandler(target.nr);

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

			public void merge_from(TypeMap typeMap) {
				if (initial == null) {
					initial = typeMap.clearLive(makeBasicBlock(
							this.block_label, 0));
					needs_analyze.add(this);

				} else {
					TypeMap new_types = initial.mergeFrom(typeMap);
					if (new_types == initial) {
						// ignore //
					} else if (new_types.equals(initial)) {
					    if (ERT.DEBUG) System.err.println("Missed TypeMap sharing opportunity");
					} else {

						// System.out.println("merge " + initial + "\n    | " +
						// typeMap + "\n   -> " + new_types);
						initial = new_types;
						needs_analyze.add(this);
					}
				}

				typeMap.add_succ(initial.bb);
			}

			List<Insn> insns = new ArrayList<Insn>();

			@Override
			public void visitEnd() {
			}

			@Override
			public void visitInsn(Insn insn) {
				insns.add(insn);
			}

			private TypeMap setType(TypeMap current, EObject dd, Type type) {
				ETuple dst = dd.testTuple();

				EObject key = dst.elm(1);
				EObject value = dst.elm(2);
				if (key == X_ATOM) {
					current = current.setx(value.asInt(),
							type == Type.DOUBLE_TYPE ? EDOUBLE_TYPE : type,
							       FV.this);
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

			private TypeMap setType(TypeMap current, DestinationOperand dst, Type type) {
				{
					Operands.FReg freg;
					if ((freg = dst.testFReg()) != null) {
						return current.setf(freg.nr, type);
					}
				}
				type = type == Type.DOUBLE_TYPE ? EDOUBLE_TYPE : type;
				{
					Operands.XReg xreg;
					if ((xreg = dst.testXReg()) != null) {
						return current.setx(xreg.nr, type, FV.this);
					}
				}
				{
					Operands.YReg yreg;
					if ((yreg = dst.testYReg()) != null) {
						return current.sety(yreg.nr, type);
					}
				}
				throw new Error("unknown " + dst);
			}

			@Deprecated
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

			private Type getType(TypeMap current, SourceOperand src) {
				{
					Operands.XReg xreg;
					if ((xreg = src.testXReg()) != null)
						return current.getx(xreg.nr);
				}
				{
					Operands.YReg yreg;
					if ((yreg = src.testYReg()) != null)
						return current.gety(yreg.nr);
				}
				{
					Operands.FReg freg;
					if ((freg = src.testFReg()) != null)
						return current.getf(freg.nr);
				}
				if ((src.testAtom()) != null)
					return EATOM_TYPE;
				else if ((src.testInt()) != null)
					return ESMALL_TYPE;
				else if ((src.testBigInt()) != null)
						return EBIG_TYPE;
				else if ((src.testFloat()) != null)
						return Type.DOUBLE_TYPE;
				else if (src instanceof Operands.Nil)
					return ENIL_TYPE;
				{
					Operands.TableLiteral lit;
					if ((lit = src.testTableLiteral()) != null)
						return Type.getType(lit.value.getClass());
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

				private final Insn insn;
				private final TypeMap current;

				public BInsn(Insn insn, TypeMap current) {
					this.insn = insn;
					this.current = current;
				}

				@Override
				public BeamOpcode opcode() {
					return insn.opcode();
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
		public Set<Integer> getXRegisters() {
			return all_xregs;
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
