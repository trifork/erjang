package erjang.beam;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.objectweb.asm.Type;

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

	Map<Label, FunInfo> result = new HashMap<Label, FunInfo>();

	static class FunInfo {
		Set<FunInfo> callers = new HashSet<FunInfo>();
		FunID name;
		boolean is_tail_recursive, is_pausable;

		@Override
		public String toString() {
			return (is_tail_recursive ? "T" : "-") + (is_pausable ? "P" : "-")
					+ " " + name;

		}

		void addCaller(FunInfo caller) {
			callers.add(caller);
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

		for (FunInfo fi : result.values()) {
			if (fi.is_pausable) {
				for (FunInfo ffi : fi.callers) {
					if (!ffi.is_pausable) {
						effect = ffi.is_pausable = true;
					}
				}
			}
		}

		return effect;
	}

	FunInfo get(Label label) {
		FunInfo fi = result.get(label);
		if (fi == null) {
			fi = new FunInfo();
			result.put(label, fi);
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
	public void visitEnd() {
		propagate();

		if (ERT.DEBUG2) {
		for (Map.Entry<Label, FunInfo> e : result.entrySet()) {
			System.err.println(e.getValue());
		}
		}

	}

	@Override
	public void visitExport(EAtom fun, int arity, int entry) {
	}

	@Override
	public FunctionVisitor visitFunction(EAtom name, int arity,
			final int startLabel) {

		Label start = new Label(startLabel);
		final FunInfo self = get(name, arity, start);

		return new FunctionVisitor() {

			@Override
			public BlockVisitor visitLabeledBlock(int label) {

				return new BlockVisitor() {

					@Override
					public void visitInsn(Insn insn) {

						switch (insn.opcode()) {

						case send: {
							self.is_pausable = true;
							break;
						}
						
						case call: {
							Insn.IL cl = (Insn.IL) insn;
							get(cl.label).addCaller(self);
							break;
						}

						case call_last: {
							ILI cl = (Insn.ILI) insn;
							self.is_tail_recursive |= (cl.label.nr != startLabel);
							get(cl.label).addCaller(self);
							break;
						}

						case call_only: {
							IL cl = (Insn.IL) insn;
							self.is_tail_recursive |= (cl.label.nr != startLabel);
							get(cl.label).addCaller(self);
							break;
						}

						case apply_last:
							self.is_tail_recursive = true;
							self.is_pausable = true;
							break;

						case call_ext_last:
						case call_ext_only:
							self.is_tail_recursive = true;
							/* FALL THRU */

						case call_ext:
							Insn.IE e = (IE) insn;

							String mod = e.ext_fun.mod.getName();
							String fun = e.ext_fun.fun.getName();
							int arity = e.ext_fun.arity;
							BuiltInFunction bif = BIFUtil.getMethod(mod,
									fun, arity, false, false);
							
							if (bif != null) {
								self.is_pausable |= bif.isPausable();
								
								if (ERT.DEBUG2 && !self.is_pausable) {
									System.err.println("! "+mod+":"+fun+"/"+e.ext_fun.arity);
								}
								
							} else {
								self.is_pausable = true;
							}

							break;

						case apply:
						case call_fun:
						case wait:
						case wait_timeout:
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
		
		for (FunInfo fi : result.values()) {
			res.put(fi.name, fi);
		}
		
		return res;

		
	}

}
