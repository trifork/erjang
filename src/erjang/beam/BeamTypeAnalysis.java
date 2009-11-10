package org.erlang.beam;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import org.erlang.EAtom;
import org.erlang.ECons;
import org.erlang.EDouble;
import org.erlang.EFun;
import org.erlang.EInteger;
import org.erlang.EList;
import org.erlang.ENil;
import org.erlang.EObject;
import org.erlang.EPID;
import org.erlang.EPort;
import org.erlang.ESeq;
import org.erlang.ETerm;
import org.erlang.ETuple;
import org.erlang.ETuple2;
import org.erlang.beam.BeamFile.FunctionVisitor;
import org.erlang.beam.BeamFile.LabeledBlockVisitor;
import org.erlang.beam.BeamFile.ModuleVisitor;
import org.objectweb.asm.Type;

public class BeamTypeAnalysis extends ModuleVisitor {

	static final Type EINTEGER_TYPE = Type.getType(EInteger.class);
	static final Type EOBJECT_TYPE = Type.getType(EObject.class);
	static final Type EDOUBLE_TYPE = Type.getType(EDouble.class);
	static final Type ENIL_TYPE = Type.getType(ENil.class);
	static final Type EATOM_TYPE = Type.getType(EAtom.class);
	static final Type ETUPLE_TYPE = Type.getType(ETuple.class);
	static final Type ECONS_TYPE = Type.getType(ECons.class);
	static final Type ESEQ_TYPE = Type.getType(ESeq.class);
	static final Type ELIST_TYPE = Type.getType(EList.class);
	static final Type EFUN_TYPE = Type.getType(EFun.class);
	static final Type EPID_TYPE = Type.getType(EPID.class);
	static final Type EPORT_TYPE = Type.getType(EPort.class);

	static final ETerm X_ATOM = EAtom.intern("x");
	static final ETerm Y_ATOM = EAtom.intern("y");
	static final ETerm FR_ATOM = EAtom.intern("fr");
	static final ETerm NIL_ATOM = EAtom.intern("nil");
	static final ETerm INTEGER_ATOM = EAtom.intern("integer");
	static final ETerm FLOAT_ATOM = EAtom.intern("float");
	static final ETerm ATOM_ATOM = EAtom.intern("atom");
	static final ETerm LITERAL_ATOM = EAtom.intern("literal");
	static final ETerm NOFAIL_ATOM = EAtom.intern("nofail");
	static final ETerm F_ATOM = EAtom.intern("f");

	private static final ETuple X0_REG = ETuple.make(new ETerm[] { X_ATOM,
			new EInteger(0) });

	@Override
	FunctionVisitor visitFunction(EAtom name, int arity, int startLabel) {
		return new FV(name, arity, startLabel);
	}

	class FV extends FunctionVisitor {

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

		void live_analysis() {

			Integer[] all = bbs.keySet().toArray(new Integer[bbs.size()]);
			Arrays.sort(all);

			boolean change = false;

			do {
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

		public FV(EAtom name, int arity, int startLabel) {
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

			SortedSet<Integer> labels = new TreeSet<Integer>();
			labels.addAll(lbs.keySet());

			boolean has_unreachable_code = false;
			for (int i : labels) {
				lb = lbs.get(i);
				if (lb.isUnreached()) {
					if (BeamOpcode.get(lb.insns.get(0).nth(1).asAtom()) == BeamOpcode.func_info) {
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
		}

		private void dump() {

			System.err.println("DUMPING " + name + "/" + arity);

			for (Map.Entry<Integer, LabeledBlock> ent : lbs.entrySet()) {
				ent.getValue().dump();
			}

		}

		@Override
		public LabeledBlockVisitor visitLabeledBlock(int label) {
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
				throw new Error("no such bb: " + label);
			}
		}

		class LabeledBlock extends LabeledBlockVisitor {

			private final int block_label;
			TypeMap initial;
			boolean last = false;
			TypeMap[] map;

			public LabeledBlock(int label) {
				this.block_label = label;
				initial = null;
			}

			public boolean isUnreached() {
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
				next_insn: for (int i = 0; i < insns.size(); i++) {
					System.err.println(name + "(" + block_label + "):" + i
							+ " :: " + (map == null ? "?" : map[i]));
					System.err.println("     >> " + insns.get(i));
				}
			}

			public void analyze0() {
				TypeMap current = initial;
				BeamOpcode last_opcode = BeamOpcode.NONE;

				map = new TypeMap[insns.size()];

				next_insn: for (int insn_idx = 0; insn_idx < insns.size(); insn_idx++) {
					
					if (is_term(last_opcode)) {
						throw new Error("how did we get here then...? "+this.block_label+":"+insn_idx);
					}
					
					map[insn_idx] = current;
					ETuple insn = insns.get(insn_idx);
					BeamOpcode code = BeamOpcode.get(insn.nth(1).asAtom());
					last_opcode = code;
					/*
					 * System.out.println(name + "(" + bb_label + "):" + i +
					 * " :: " + current + "" + insn);
					 */
					switch (code) {
					case fmove:
					case move: {
						EObject src = insn.nth(2);
						EObject dst = insn.nth(3);
						
						Type srcType = getType(current, src);

						if (sizeof(current, src) > sizeof(current, dst)) {
							System.err.println(insn);
							if (getType(current, src) == Type.INT_TYPE) {
								current = setType(current, dst, EINTEGER_TYPE);
							} else if (getType(current, src) == Type.DOUBLE_TYPE) {
								current = setType(current, dst, EDOUBLE_TYPE);
							} else {
								throw new Error("why?" + insn);
							}
						}

						current = setType(current, (ETuple2) dst, srcType);
						continue next_insn;
					}

					case jump: {
						current = branch(current, insn.nth(2), insn_idx);
						continue next_insn;

					}

					case send: {
						current.touchx(0, 2);
						current = current.setx(0, current.getx(1));
						continue next_insn;
					}

					case arithfbif: {
						System.err.println(insn);
						EAtom name = insn.nth(2).asAtom();
						ESeq parms = insn.nth(4).asSeq();

						checkArgs(current, parms, insn);

						Type type = getBifResult(name.getName(), parmTypes(
								current, parms), false);

						current = setType(current, insn.nth(5), type);

						continue next_insn;
					}

					case gc_bif: {
						// {gc_bif,BifName,F,Live,[A1,A2?],Reg};

						boolean is_guard = false;
						if (insn.nth(3).asTuple().nth(2).asInt() != 0) {
							is_guard = true;
						}

						// System.err.println(insn);
						current = branch(current, insn.nth(3), insn_idx);
						
						
						EAtom name = insn.nth(2).asAtom();
						ESeq parms = insn.nth(5).asSeq();

						checkArgs(current, parms, insn);

						Type type = getBifResult(name.getName(), parmTypes(
								current, parms), is_guard);

						current = setType(current, insn.nth(6), type);

						// dump();
						continue next_insn;
					}

					case bif: {
						// System.err.println(insn);
						current = branch(current, insn.nth(3), insn_idx);

						EAtom name = insn.nth(2).asAtom();
						ESeq parms = insn.nth(4).asSeq();

						checkArgs(current, parms, insn);

						Type type = getBifResult(name.getName(), parmTypes(
								current, parms), false);

						current = setType(current, insn.nth(5), type);

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
						getType(current, insn.nth(2));
						current = setType(current, insn.nth(3),
								Type.DOUBLE_TYPE);
						continue next_insn;
					}

					case init: {
						current = setType(current, insn.nth(2), null);
						continue next_insn;
					}

					case set_tuple_element: {
						getType(current, insn.nth(2));
						getType(current, insn.nth(3));
						continue next_insn;
					}

					case get_tuple_element: {
						EObject src = insn.nth(2);
						int idx = insn.nth(3).asInt();
						EObject dst = insn.nth(4);
						getType(current, src);
						current = setType(current, dst, EOBJECT_TYPE);
						continue next_insn;
					}

					case get_list: {
						EObject from = insn.nth(2);
						EObject head_into = insn.nth(3);
						EObject tail_into = insn.nth(4);

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
						
						Type head_type = getType(current, insn.nth(2));
						Type tail_type = getType(current, insn.nth(3));

						if (tail_type == null) {
							throw new Error("value: " + insn.nth(3)
									+ " has no type");
						}

						if (tail_type.equals(ENIL_TYPE)
								|| tail_type.equals(ESEQ_TYPE)
								|| tail_type.equals(ELIST_TYPE)) {
							current = setType(current, insn.nth(4), ELIST_TYPE);
						} else {
							current = setType(current, insn.nth(4), ECONS_TYPE);
						}
						continue next_insn;
					}

					case put_tuple: {
						int arity = insn.nth(2).asInt();
						ETuple reg = insn.nth(3).asTuple();

						current = setType(current, reg, getTupleType(arity));
						continue next_insn;
					}

					case K_catch:
						current = branch(current, insn.nth(3), insn_idx);
						continue next_insn;

					case catch_end:
						continue next_insn;

					case make_fun2: {
						current.touchx(0, insn.nth(5).asInt());
						current = current.setx(0, EFUN_TYPE);
						continue next_insn;
					}

					case loop_rec: {
						current = branch(current, insn.nth(2), insn_idx);
						current = setType(current, insn.nth(3), EOBJECT_TYPE);
						continue next_insn;
					}

					case remove_message: 
						// assume this insn overrides X0
						// current = current.setx(0, EOBJECT_TYPE);
						continue next_insn;

					case loop_rec_end:
					case timeout:
					{
						// System.err.println(insn);
						continue next_insn;
					}

					case wait_timeout: {
						Type timeout_type = getType(current, insn.nth(3));
					}
					case wait: {
						current = branch(current, insn.nth(2), insn_idx);
						continue next_insn;
					}

					case deallocate:
					case trim: {
						int howmuch = insn.nth(2).asInt();
						current = current.trim_y(howmuch);
						continue next_insn;
					}

						// this is really a no-op in jave
					case test_heap: {
						continue next_insn;
					}

					case allocate_zero:
					case allocate_heap_zero: {
						int slots = insn.nth(2).asInt();
						current = current.alloc_y(slots);
						for (int slot = 0; slot < slots; slot++) {
							current = current.sety(slot, ENIL_TYPE);
						}
						continue next_insn;
					}

					case allocate:
					case allocate_heap: {
						current = current.alloc_y(insn.nth(2).asInt());
						continue next_insn;
					}

					case fcheckerror:
					case fclearerror:
						continue next_insn;

					case put:
						getType(current, insn.nth(2));
						continue next_insn;

					case select_tuple_arity: {
						current = branch(current, insn.nth(3), insn_idx);

						// touch select reg
						getType(current, insn.nth(2));

						ESeq cases = insn.nth(4).asTuple().nth(2).asSeq();
						while (cases != ESeq.EMPTY) {
							EObject value = cases.head();
							EObject target = cases.tail().head();

							current = branch(current, target, insn_idx);

							cases = cases.tail().tail();
						}

						continue next_insn;
					}

					case select_val: {
						current = branch(current, insn.nth(3), insn_idx);

						// touch select reg
						getType(current, insn.nth(2));

						ESeq cases = insn.nth(4).asTuple().nth(2).asSeq();
						while (cases != ESeq.EMPTY) {
							EObject value = cases.head();
							EObject target = cases.tail().head();

							current = branch(current, target, insn_idx);

							cases = cases.tail().tail();
						}

						continue next_insn;
					}

						// we loose the type of the result

					case apply:
					case call:
					case call_ext:
					{
						int argCount = insn.nth(2).asInt();
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
						int argCount = insn.nth(2).asInt();
						current.touchx(0, argCount);
						continue next_insn;
						
					case func_info:
						continue next_insn;

					case if_end:
					case badmatch:
					case case_end:
						continue next_insn;

					default:
						throw new Error("unhandled: " + insn + "::" + current);
					}
				}
				
				

				if (is_term(last_opcode) == false) {
					LabeledBlock lbv = get_lb(this.block_label + 1, false);
					try {
						lbv.merge_from(current);
					} catch (Error e) {

						System.out.println("merge " + current + "\n    | "
								+ lbv.initial + "\n FAILED");

					}
				}
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
					
				case wait:
				case select_tuple_arity:
				case select_val:
					
				case jump:
					return true;
				default:
					return false;
				}
			}

			private int sizeof(TypeMap current, EObject cell) {
				
				ETuple at;
				if ((at=cell.asTuple()) != null) {
					if (at.arity() == 2) {
						if (at.nth(1) == X_ATOM) return 32;
						if (at.nth(1) == Y_ATOM) return 32;
						if (at.nth(1) == FR_ATOM) return 64;
					}
				}
				
				Type t = getType(current, cell);
				if (t == Type.DOUBLE_TYPE) {
					return 64;
				} else {
					return 32;
				}
			}

			private Type getBifResult(String name, Type[] parmTypes, boolean is_guard) {
				return BIFUtil.getBifResult(name, parmTypes, is_guard);
			}

			private Type[] parmTypes(TypeMap current, ESeq args) {
				ArrayList<Type> res = new ArrayList<Type>();

				while (args != ESeq.EMPTY) {
					EObject arg = args.head();
					res.add(getType(current, arg));
					args = args.tail();
				}

				return res.toArray(new Type[res.size()]);
			}

			private void checkArgs(TypeMap current, EObject eTerm, ETuple insn) {
				ESeq args = eTerm.asSeq();

				while (args != ESeq.EMPTY) {
					EObject arg = args.head();

					if (getType(current, arg) == null) {
						throw new Error("uninitialized " + arg + " in " + insn);
					}

					args = args.tail();
				}
			}

			private TypeMap analyze_test(TypeMap current, ETuple insn,
					int insn_idx) {

				current = branch(current, insn.nth(3), insn_idx);

				checkArgs(current, insn.nth(4), insn);
				
				EObject[] args = insn.nth(4).asSeq().toArray();
				EObject arg1 = args[0];
				EObject arg2 = (args.length > 1) ? args[1] : null;

				BeamOpcode test = BeamOpcode.get(insn.nth(2).asAtom());
				switch (test) {
				case is_nil: {
					return setType(current, arg1.asTuple(), ENIL_TYPE);
				}

				case is_list:
				case is_nonempty_list: {
					return setType(current, arg1.asTuple(), ECONS_TYPE);
				}

				case is_tuple: {
					return setType(current, arg1.asTuple(), ETUPLE_TYPE);
				}

				case test_arity: {
					return setType(current, arg1.asTuple(), getTupleType(arg2
							.asInt()));
				}

				case is_boolean:
				case is_atom: {
					return setType(current, arg1.asTuple(), EATOM_TYPE);
				}

				case is_integer: {
					return setType(current, arg1, EINTEGER_TYPE);
				}

				case is_pid: {
					return setType(current, arg1, EPID_TYPE);
				}

				case is_port: {
					return setType(current, arg1, EPORT_TYPE);
				}

				case is_float: {
					return setType(current, arg1, EDOUBLE_TYPE);
				}

				case is_ge:
				case is_ne:
				case is_ne_exact:
					return current;

				case is_eq:
				case is_eq_exact: {
					Type t1 = getType(current, arg1);
					Type t2 = getType(current, arg2);

					if (!t1.equals(t2)) {
						if (isReg(arg1)) {
							current = setType(current, arg1.asTuple(), t2);
						} else if (isReg(arg2)) {
							current = setType(current, arg2.asTuple(), t1);
						}
					}

					return current;
				}

				case is_lt:

					return current;

				default:
					throw new Error("unhandled test: " + insn);
				}

			}

			private TypeMap branch(TypeMap current, EObject nth, int idx) {
				if (nth != NOFAIL_ATOM) {

					ETuple tuple = nth.asTuple();
					if (tuple.nth(1) != F_ATOM)
						throw new Error("not a branch target: " + nth);

					int target = tuple.nth(2).asInt();
					if (target != 0) {
						get_lb(target, false).merge_from(current);
					}
				}

				return current.clearLive(makeBasicBlock(block_label, idx + 1));
			}

			private Type getTupleType(int arity) {
				String tt = "L" + ETUPLE_TYPE.getInternalName() + arity + ";";
				return Type.getType(tt);
			}

			private boolean isReg(EObject arg2) {
				ETuple et = arg2.asTuple();
				return (et != null && et.nth(1) == X_ATOM
						|| et.nth(1) == Y_ATOM || et.nth(1) == FR_ATOM);
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
				ETuple dst = dd.asTuple();

				EObject key = dst.nth(1);
				EObject value = dst.nth(2);
				if (key == X_ATOM) {
					current = current.setx(value.asInt(), type);
				} else if (key == Y_ATOM) {
					current = current.sety(value.asInt(), type);
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
						if (tup.elem2.getClass() == EInteger.class) {
							return Type.INT_TYPE;
						} else {
							return Type.getType(tup.elem2.getClass());
						}
					} else if (tup.elem1 == FLOAT_ATOM) {
						return Type.DOUBLE_TYPE;
					}

				} else if (src == NIL_ATOM) {
					return ENIL_TYPE;

				} else if (src instanceof EInteger) {
					return EINTEGER_TYPE;
				}

				throw new Error("unknown " + src);
			}

		}

	}

}
