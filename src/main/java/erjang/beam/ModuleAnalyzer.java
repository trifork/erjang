package erjang.beam;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import erjang.EAtom;
import erjang.EObject;
import erjang.ERT;
import erjang.FunID;
import erjang.beam.repr.Insn;
import erjang.beam.repr.Insn.IE;
import erjang.beam.repr.Insn.IL;
import erjang.beam.repr.Insn.ILI;
import erjang.beam.repr.Operands.Label;
import erjang.beam.repr.Operands.SourceOperand;

public class ModuleAnalyzer implements ModuleVisitor {

	public static final boolean DEBUG_ANALYZE = false;
	Map<Label, FunInfo> info = new HashMap<Label, FunInfo>();

	static class FunInfo {
		Set<FunInfo> callers = new HashSet<FunInfo>();
		Set<FunInfo> tail_callers = new HashSet<FunInfo>();
		FunID name;
		boolean may_return_tail_marker, is_pausable, call_is_pausable;
		public boolean exported;
		protected boolean is_called_locally_in_tail_position;
		protected boolean is_anon_fun;
		protected boolean is_called_locally_in_nontail_position;

		boolean mustHaveFun() {
			return exported 
				|| is_called_locally_in_tail_position
				|| is_anon_fun;
		}
		
		@Override
		public String toString() {
			return (may_return_tail_marker ? "T" : "-") + (is_pausable ? "P" : "-")
					+ " " + name;

		}

		void addCaller(FunInfo caller) {
			callers.add(caller);
		}

		void addTailCaller(FunInfo caller) {
			tail_callers.add(caller);
		}

		@Override
		public int hashCode() {
			return name.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			if (obj instanceof FunInfo) {
				FunInfo fi = (FunInfo) obj;
				return name.equals(fi.name);
			}
			return false;
		}
	}

	void propagate() {
		while (propagate_one())
			;
	}

	/** */
	boolean propagate_one() {
		boolean effect = false;

		for (FunInfo fun : info.values()) {
			if (fun.is_pausable || fun.call_is_pausable) {
				for (FunInfo caller : fun.callers) {
					
					if (!caller.is_pausable) {
						effect = caller.is_pausable = true;
						
						if (DEBUG_ANALYZE) {
							System.err.println("propagate " +fun+ " -> " + caller);
						}
					}
				}

				for (FunInfo caller : fun.tail_callers) {
					
					if (!caller.call_is_pausable) {
						effect = caller.call_is_pausable = true;
						
						if (DEBUG_ANALYZE) {
							System.err.println("propagate " +fun+ " -> " + caller);
						}
					}
				}
			
			}
		}

		return effect;
	}

	FunInfo get(Label label) {
		FunInfo fi = info.get(label);
		if (fi == null) {
			fi = new FunInfo();
			info.put(label, fi);
		}

		return fi;
	}

	FunInfo get(EAtom fun, int arity, Label label) {
		FunInfo fi = get(label);
		fi.name = new FunID(name, fun, arity);
		return fi;
	}

	private EAtom name;

	@Override
	public void visitAttribute(EAtom att, EObject value) {
	}

	@Override
	public void visitExport(EAtom fun, int arity, int entry) {
		Label label = new Label(entry);
		FunInfo fi = get(label);
		fi.exported = true;
	}


	@Override
	public void visitEnd() {
		propagate();

		if (ERT.DEBUG2) {
		for (Map.Entry<Label, FunInfo> e : info.entrySet()) {
			System.err.println(e.getValue());
		}
		}

	}
	
	@Override
	public void declareFunction(EAtom fun, int arity, int startLabel) {
		Label label = new Label(startLabel);
		get(fun,arity,label);
	}

	@Override
	public FunctionVisitor visitFunction(EAtom name, int arity,
			final int startLabel) {
		
		if (DEBUG_ANALYZE) {
			System.err.println("== analyzing "+ModuleAnalyzer.this.name+":"+name+"/"+arity);
		}

		Label start = new Label(startLabel);
		final FunInfo self = get(name, arity, start);

		return new FunctionVisitor() {

			@Override
			public BlockVisitor visitLabeledBlock(int label) {

				return new BlockVisitor() {

					@Override
					public void visitInsn(Insn insn) {

						BeamOpcode op;
						switch (op = insn.opcode()) {

						case send: {
							if (DEBUG_ANALYZE && !self.is_pausable) {
								System.err.println("pausable: send");
							}
							
							self.is_pausable = true;
							break;
						}
						
						case call: {
							Insn.IL cl = (Insn.IL) insn;
							FunInfo target = get(cl.label);
							
							BuiltInFunction bif = 
								BIFUtil.getMethod(target.name.module,
									target.name.function, target.name.arity, false, false);

							if (bif == null) {							
								target.addCaller(self);
								target.is_called_locally_in_nontail_position = true;
							}
							break;
						}

						case call_last: {
							ILI cl = (Insn.ILI) insn;
							boolean is_self_call = cl.label.nr == startLabel;	
							self.may_return_tail_marker |= !is_self_call;
							FunInfo target = get(cl.label);
							
							BuiltInFunction bif = 
								BIFUtil.getMethod(target.name.module,
									target.name.function, target.name.arity, false, false);

							if (bif == null) {							
								target.addTailCaller(self);
								target.is_called_locally_in_tail_position |= !is_self_call;
							}
							break;
						}

						case call_only: {
							IL cl = (Insn.IL) insn;
							boolean is_self_call = cl.label.nr == startLabel;
							self.may_return_tail_marker |= !is_self_call;
							FunInfo target = get(cl.label);
							BuiltInFunction bif = 								
								BIFUtil.getMethod(target.name.module,
									target.name.function, target.name.arity, false, false);

							if (bif == null) {							
								target.addTailCaller(self);
								target.is_called_locally_in_tail_position |= !is_self_call;
							}
							break;
						}
						
						case make_fun2: {
							Insn.F fi = (Insn.F) insn;
							Label anon = new Label(fi.anon_fun.label);
							get(anon).is_anon_fun = true;
							break;
						}

						case apply_last:
							if (DEBUG_ANALYZE && !self.call_is_pausable) {
								System.err.println("call_pausable: " + op);
							}

							self.may_return_tail_marker = true;
							self.call_is_pausable = true;
							break;

						case call_ext_last:
						case call_ext_only: 
						{
							Insn.IE e = (IE) insn;

							String mod = e.ext_fun.mod.getName();
							String fun = e.ext_fun.fun.getName();
							int arity = e.ext_fun.arity;
							
							if (mod.equals("erlang")
								&& arity == 1
								&& (fun.equals("throw") || fun.equals("error") || fun.equals("exit"))) {
								break;
							}
						}							
							
							if (DEBUG_ANALYZE && !self.call_is_pausable) {
								System.err.println("call_pausable: " + op);
							}

							self.may_return_tail_marker = true;
							self.call_is_pausable = true;
							/* FALL THRU */

						case call_ext:
							if (self.is_pausable) {
								break;
							}
							
							Insn.IE e = (IE) insn;

							String mod = e.ext_fun.mod.getName();
							String fun = e.ext_fun.fun.getName();
							int arity = e.ext_fun.arity;
							BuiltInFunction bif = BIFUtil.getMethod(mod,
									fun, arity, false, false);
							
							if (bif != null) {
								
								if (DEBUG_ANALYZE && !self.is_pausable && bif.isPausable()) {
									System.err.println("pausable: calls " + bif.javaMethod);
								}

								
								self.is_pausable |= bif.isPausable();
								

							} else if (op == BeamOpcode.call_ext) {

								if (DEBUG_ANALYZE && !self.is_pausable) {
									System.err.println("pausable: calls "+mod+":"+fun+"/"+arity);
								}

								self.is_pausable = true;
							}

							break;

						case apply:
						case call_fun:
						case wait:
						case wait_timeout:
							if (DEBUG_ANALYZE && !self.is_pausable) {
								System.err.println("pausable: "+op);
							}
							self.is_pausable = true;
							
							break;

						case bif:
						case bif0:
						case bif1:
						case bif2: {
							// no reason to go through this pain, if we're
							// already pausable
							if (self.is_pausable)
								break;

							Insn.Bif bi = (Insn.Bif) insn;
							EAtom name = bi.ext_fun.fun;
							SourceOperand[] srcs = bi.args;

							bif = BIFUtil.getMethod("erlang",
									name.getName(), srcs.length,
									false, true);
							
							if (bif == null) {
								throw new Error("missing bif: "+bi.ext_fun);
							}

							self.is_pausable |= bif.isPausable();

							if (DEBUG_ANALYZE && self.is_pausable) {
								System.err.println("pausable: calls "+bif.javaMethod);
							}

						}
						default:
							break;
						}

					}

					@Override
					public void visitEnd() {
					}
				};

			}

			@Override
			public void visitEnd() {
			}
		};

	}

	@Override
	public void visitModule(EAtom name) {
		this.name = name;
	}

	public Map<FunID,FunInfo> getFunInfos() {
		
		
		
		Map<FunID, FunInfo> res = new HashMap<FunID, FunInfo>();
		
		for (FunInfo fi : info.values()) {
			res.put(fi.name, fi);
		}
		
		return res;

		
	}

}
