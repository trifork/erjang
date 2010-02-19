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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;

import kilim.Pausable;
import kilim.analysis.ClassInfo;
import kilim.analysis.ClassWeaver;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.Method;

import erjang.EAtom;
import erjang.EBinMatchState;
import erjang.EBinary;
import erjang.EBitString;
import erjang.EBitStringBuilder;
import erjang.ECons;
import erjang.EDouble;
import erjang.EFun;
import erjang.EInteger;
import erjang.EList;
import erjang.EModule;
import erjang.ENil;
import erjang.ENumber;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.EProc;
import erjang.ERT;
import erjang.ERef;
import erjang.ESeq;
import erjang.ESmall;
import erjang.EString;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ErlFun;
import erjang.ErlangException;
import erjang.Export;
import erjang.Import;
import erjang.Module;
import erjang.NotImplemented;
import erjang.beam.Arg.Kind;

import static erjang.beam.CodeAtoms.*;

/**
 * 
 */
public class CompilerVisitor implements ModuleVisitor, Opcodes {

	ECons atts = ERT.NIL;
	private Set<String> exported = new HashSet<String>();

	private final ClassVisitor cv;
	private EAtom module_name;
	private Type self_type;

	private static final EObject ATOM_field_flags = EAtom.intern("field_flags");
	private static final EObject ATOM_start = EAtom.intern("start");

	static final String[] PAUSABLE_EX = new String[] { Type.getType(
			Pausable.class).getInternalName() };
	static final Type EBINMATCHSTATE_TYPE = Type.getType(EBinMatchState.class);
	static final Type EBINSTRINGBUILDER_TYPE = Type
			.getType(EBitStringBuilder.class);
	static final Type ERLANG_EXCEPTION_TYPE = Type
			.getType(ErlangException.class);
	static final Type ERT_TYPE = Type.getType(ERT.class);
	static final Type EINTEGER_TYPE = Type.getType(EInteger.class);
	static final Type ESTRING_TYPE = Type.getType(EString.class);
	static final Type EMODULE_TYPE = Type.getType(EModule.class);
	/**
	 * 
	 */
	static final String EMODULE_NAME = EMODULE_TYPE.getInternalName();
	static final Type ENUMBER_TYPE = Type.getType(ENumber.class);
	static final Type EOBJECT_TYPE = Type.getType(EObject.class);
	static final String EOBJECT_DESC = EOBJECT_TYPE.getDescriptor();
	static final Type EPROC_TYPE = Type.getType(EProc.class);
	static final String EPROC_NAME = EPROC_TYPE.getInternalName();
	static final String EPROC_DESC = EPROC_TYPE.getDescriptor();

	static final Type ESMALL_TYPE = Type.getType(ESmall.class);
	static final String ESMALL_NAME = ESMALL_TYPE.getInternalName();

	static final Type EEXCEPTION_TYPE = Type.getType(ErlangException.class);
	static final String EEXCEPTION_DESC = EEXCEPTION_TYPE.getDescriptor();

	/**/

	static final String GO_DESC = "(" + EPROC_TYPE.getDescriptor() + ")"
			+ EOBJECT_DESC;
	static final Type EDOUBLE_TYPE = Type.getType(EDouble.class);
	static final Type ENIL_TYPE = Type.getType(ENil.class);
	static final Type EATOM_TYPE = Type.getType(EAtom.class);
	static final Type ETUPLE_TYPE = Type.getType(ETuple.class);
	static final Type EBINARY_TYPE = Type.getType(EBinary.class);
	static final Type EBITSTRING_TYPE = Type.getType(EBitString.class);
	static final Type EBITSTRINGBUILDER_TYPE = Type
			.getType(EBitStringBuilder.class);
	static final Type ECONS_TYPE = Type.getType(ECons.class);
	static final Type ESEQ_TYPE = Type.getType(ESeq.class);
	static final Type ELIST_TYPE = Type.getType(EList.class);
	static final Type EFUN_TYPE = Type.getType(EFun.class);
	/**
	 * 
	 */
	static final String EFUN_NAME = EFUN_TYPE.getInternalName();
	static final String EOBJECT_NAME = EOBJECT_TYPE.getInternalName();
	static final String ETUPLE_NAME = ETUPLE_TYPE.getInternalName();
	static final String ERT_NAME = ERT_TYPE.getInternalName();
	static final String EDOUBLE_NAME = EDOUBLE_TYPE.getInternalName();
	static final String EINTEGER_NAME = EINTEGER_TYPE.getInternalName();
	static final String ENIL_NAME = ENIL_TYPE.getInternalName();
	static final String ESEQ_NAME = ESEQ_TYPE.getInternalName();

	static final String ETUPLE_DESC = ETUPLE_TYPE.getDescriptor();
	static final String EATOM_DESC = EATOM_TYPE.getDescriptor();
	static final String ECONS_DESC = ECONS_TYPE.getDescriptor();
	static final String ESEQ_DESC = ESEQ_TYPE.getDescriptor();

	/**
	 * 
	 */
	static final String EFUN_DESCRIPTOR = EFUN_TYPE.getDescriptor();
	static final Type EPID_TYPE = Type.getType(EPID.class);
	static final Type EPORT_TYPE = Type.getType(EPort.class);
	static final Type EMATCHSTATE_TYPE = Type.getType(EBinMatchState.class);

	static final Type MODULE_ANN_TYPE = Type.getType(Module.class);
	static final Type ERLFUN_ANN_TYPE = Type.getType(ErlFun.class);
	static final Type IMPORT_ANN_TYPE = Type.getType(Import.class);
	static final Type EXPORT_ANN_TYPE = Type.getType(Export.class);
	private final ClassRepo classRepo;

	/**
	 * @param classRepo
	 * 
	 */
	public CompilerVisitor(ClassVisitor cv, ClassRepo classRepo) {
		this.cv = cv;
		this.classRepo = classRepo;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.beam.ModuleVisitor#visitModule(erjang.EAtom)
	 */
	@Override
	public void visitModule(EAtom name) {

		this.module_name = name;

		this.self_type = Type.getType("L" + getInternalClassName() + ";");

		cv.visit(V1_6, ACC_PUBLIC, self_type.getInternalName(), null,
				EMODULE_NAME, null);

		add_module_annotation(cv);

	}

	private void add_module_annotation(ClassVisitor cv) {

		AnnotationVisitor av = cv.visitAnnotation(MODULE_ANN_TYPE
				.getDescriptor(), true);
		av.visit("value", getModuleName());

		av.visitEnd();
	}

	public String getInternalClassName() {

		String moduleName = getModuleName();
		return Compiler.moduleClassName(moduleName);
	}

	/**
	 * @return
	 */
	private String getModuleName() {
		return module_name.getName();
	}

	Map<EObject, String> constants = new HashMap<EObject, String>();

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.beam.ModuleVisitor#visitAttribute(erjang.EAtom,
	 * erjang.EObject)
	 */
	@Override
	public void visitAttribute(EAtom att, EObject value) {
		atts = atts.cons(ETuple2.make(att, value));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.beam.ModuleVisitor#visitEnd()
	 */
	@Override
	public void visitEnd() {

		// wow, this is where we generate <clinit>

		for (Map.Entry<String, ExtFunc> ent : imported.entrySet()) {
			String field_name = ent.getKey();
			ExtFunc f = ent.getValue();

			FieldVisitor fv = cv.visitField(ACC_STATIC, ent.getKey(), "L"
					+ EFUN_NAME + f.no + ";", null, null);
			EFun.ensure(f.no);
			AnnotationVisitor av = fv.visitAnnotation(IMPORT_ANN_TYPE
					.getDescriptor(), true);
			av.visit("module", f.mod.getName());
			av.visit("fun", f.fun.getName());
			av.visit("arity", f.no);
			av.visitEnd();
			fv.visitEnd();
		}

		generate_classinit();

		cv.visitEnd();
	}

	/**
	 * 
	 */
	private void generate_classinit() {
		MethodVisitor mv = cv.visitMethod(ACC_STATIC | ACC_PRIVATE, "<clinit>",
				"()V", null, null);
		mv.visitCode();

		for (Map.Entry<String, String> ent : funs.entrySet()) {

			String field = ent.getKey();
			String clazz = ent.getValue();

			mv.visitTypeInsn(NEW, clazz);
			mv.visitInsn(DUP);
			mv.visitMethodInsn(INVOKESPECIAL, clazz, "<init>", "()V");

			mv.visitFieldInsn(PUTSTATIC, self_type.getInternalName(), field,
					"L" + funt.get(field) + ";");

		}

		for (Map.Entry<EObject, String> ent : constants.entrySet()) {

			EObject term = ent.getKey();
			term.emit_const(mv);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, self_type.getInternalName(),
					ent.getValue(), Type.getType(term.getClass())
							.getDescriptor());
		}

		cv.visitField(ACC_STATIC|ACC_PRIVATE, 
				"attributes", ESEQ_TYPE.getDescriptor(), null, null);
		
		atts.emit_const(mv);
		mv.visitFieldInsn(Opcodes.PUTSTATIC, self_type.getInternalName(),
				"attributes", ESEQ_TYPE.getDescriptor());
		
		
		mv.visitInsn(RETURN);
		mv.visitMaxs(200, 10);
		mv.visitEnd();

		// make the method module_name
		mv = cv.visitMethod(ACC_PROTECTED, "module_name",
				"()Ljava/lang/String;", null, null);
		mv.visitCode();
		mv.visitLdcInsn(this.module_name.getName());
		mv.visitInsn(ARETURN);
		mv.visitMaxs(1, 1);
		mv.visitEnd();

		// make the method module_name
		mv = cv.visitMethod(ACC_PROTECTED, "attributes",
				"()" + ESEQ_TYPE.getDescriptor(), null, null);
		mv.visitCode();
		mv.visitFieldInsn(Opcodes.GETSTATIC, self_type.getInternalName(), "attributes", 
				ESEQ_TYPE.getDescriptor());
		mv.visitInsn(ARETURN);
		mv.visitMaxs(1, 1);
		mv.visitEnd();

		// make default constructor
		mv = cv.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
		mv.visitCode();
		mv.visitVarInsn(ALOAD, 0);
		mv.visitMethodInsn(INVOKESPECIAL, EMODULE_NAME, "<init>", "()V");
		mv.visitInsn(RETURN);
		mv.visitMaxs(1, 1);
		mv.visitEnd();

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.beam.ModuleVisitor#visitExport(erjang.EAtom, int, int)
	 */
	@Override
	public void visitExport(EAtom name, int arity, int entry) {
		exported.add(EUtil.getJavaName(name, arity));
	}

	boolean isExported(EAtom name, int arity) {
		return exported.contains(EUtil.getJavaName(name, arity));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.beam.ModuleVisitor#visitFunction(erjang.EAtom, int, int)
	 */
	@Override
	public FunctionVisitor visitFunction(EAtom name, int arity, int startLabel) {
		return new ASMFunctionAdapter(name, arity, startLabel);
	}

	Map<String, Integer> lambdas_xx = new TreeMap<String, Integer>();
	Map<String, String> funs = new HashMap<String, String>();
	Map<String, String> funt = new HashMap<String, String>();

	class ASMFunctionAdapter implements FunctionVisitor2 {
		private final EAtom fun_name;
		private final int arity;
		private final int startLabel;

		Map<Integer, Label> labels = new TreeMap<Integer, Label>();
		Map<Integer, Label> ex_handler_labels = new TreeMap<Integer, Label>();
		Set<Integer> label_inserted = new TreeSet<Integer>();
		List<EXHandler> ex_handlers = new ArrayList<EXHandler>();
		EXHandler activeExceptionHandler;
		BeamExceptionHandler active_beam_exh;

		private boolean isTailRecursive;
		private MethodVisitor mv;
		private int[] xregs;
		private int[] yregs;
		private int[] fpregs;
		private Label start;
		private Label end;
		private int scratch_reg;

		private int bit_string_builder;

		private int bit_string_matcher;

		private int bit_string_save;

		Label getLabel(int i) {
			if (i <= 0)
				throw new Error();
			Label l = labels.get(i);
			if (l == null) {
				labels.put(i, l = new Label());
			}
			return l;
		}

		Label getExceptionHandlerLabel(BeamExceptionHandler exh) {
			int i = exh.getHandlerLabel();
			Label l = ex_handler_labels.get(i);
			if (l == null) {
				ex_handler_labels.put(i, l = new Label());
			}
			return l;
		}

		/**
		 * @param name
		 * @param arity
		 * @param startLabel
		 * @param isTailRecursive
		 */
		public ASMFunctionAdapter(EAtom name, int arity, int startLabel) {
			this.fun_name = name;
			this.arity = arity;
			this.startLabel = startLabel;
		}

		@Override
		public void visitMaxs(int x_count, int y_count, int fp_count,
				boolean isTailRecursive) {

			this.isTailRecursive = isTailRecursive;
			String javaName = EUtil.getJavaName(fun_name, arity);
			String signature = EUtil.getSignature(arity, true);
			mv = cv.visitMethod(ACC_STATIC | ACC_PUBLIC, javaName, signature,
					null, PAUSABLE_EX);

			add_erlfun_annotation(mv);

			this.start = new Label();
			this.end = new Label();

			mv.visitCode();
			allocate_regs_to_locals(x_count, y_count, fp_count);

			mv.visitLabel(start);

			mv.visitJumpInsn(GOTO, getLabel(startLabel));
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see erjang.beam.FunctionVisitor#visitEnd()
		 */
		@Override
		public void visitEnd() {
			adjust_exception_handlers(null, false);
			mv.visitLabel(end);

			for (EXHandler h : ex_handlers) {
				if (! label_inserted.contains(h.handler_beam_label))
					throw new InternalError("Exception handler not inserted: "+h.handler_beam_label);
				mv.visitTryCatchBlock(h.begin, h.end, h.target,
						      Type.getType(ErlangException.class).getInternalName());
			}

			mv.visitMaxs(20, scratch_reg + 3);
			mv.visitEnd();

			String mname = EUtil.getJavaName(fun_name, arity);
			String outer_name = self_type.getInternalName();
			String inner_name = "FN_" + mname;
			String full_inner_name = outer_name + "$" + inner_name;

			int freevars = get_lambda(fun_name, arity);
			if (freevars == -1) {

				generate_invoke_call_self();
				generate_tail_call_self();

				freevars = 0;

				FieldVisitor fv = cv.visitField(ACC_STATIC | ACC_FINAL, mname,
						"L" + EFUN_NAME + arity + ";", null, null);
				EFun.ensure(arity);

				if (isExported(fun_name, arity)) {
					if (ERT.DEBUG2)
						System.err.println("export " + module_name + ":"
								+ fun_name + "/" + arity);
					AnnotationVisitor an = fv.visitAnnotation(EXPORT_ANN_TYPE
							.getDescriptor(), true);
					an.visit("module", module_name.getName());
					an.visit("fun", fun_name.getName());
					an.visit("arity", new Integer(arity));
					an.visitEnd();
				}

				fv.visitEnd();

				funs.put(mname, full_inner_name);
				funt.put(mname, EFUN_NAME + arity);
				EFun.ensure(arity);
			}

			cv.visitInnerClass(full_inner_name, outer_name, inner_name,
					ACC_STATIC);

			byte[] data = CompilerVisitor.make_invoker(self_type, mname, mname,
					arity, true, freevars, EOBJECT_TYPE);

			ClassWeaver w = new ClassWeaver(data, new Compiler.ErjangDetector(
					self_type.getInternalName()));
			for (ClassInfo ci : w.getClassInfos()) {
				try {
					// System.out.println("> storing "+ci.className);
					classRepo.store(ci.className, ci.bytes);
				} catch (IOException e) {
					e.printStackTrace();
				}
			}

		}

		private void ensure_exception_handler_in_place() {
			adjust_exception_handlers(active_beam_exh, false);
		}

		private void adjust_exception_handlers(BeamExceptionHandler exh, boolean expectChange) {
			int desiredLabel = exh==null ? -1
				: exh.getHandlerLabel();
			int actualLabel = activeExceptionHandler==null ? -1
				: activeExceptionHandler.handler_beam_label;
			if (expectChange) assert(actualLabel != desiredLabel);
			if (actualLabel != desiredLabel) {
				// Terminate the old handler block:
				if (activeExceptionHandler != null) {
					mv.visitLabel(activeExceptionHandler.end);
					activeExceptionHandler = null;

				}
				//...and begin a new if necessary:
				if (exh != null) {
					EXHandler h = new EXHandler();
					h.begin = new Label();
					h.end = new Label();
					h.target = getExceptionHandlerLabel(exh);
					h.handler_beam_label = exh.getHandlerLabel();
					h.beam_exh = exh;

					ex_handlers.add(h);
					mv.visitLabel(h.begin);
					mv.visitInsn(NOP); // To avoid potentially-empty exception block; Kilim can't handle those.
					activeExceptionHandler = h;
				}
			}
		}

		/**
		 * 
		 */
		private void generate_invoke_call_self() {

			String javaName = EUtil.getJavaName(fun_name, arity);
			String signature = EUtil.getSignature(arity, true);
			mv = cv.visitMethod(ACC_STATIC, javaName + "$call", signature,
					null, PAUSABLE_EX);
			mv.visitCode();

			// if (isTailRecursive) {

			mv.visitVarInsn(ALOAD, 0);
			for (int i = 0; i < arity; i++) {
				mv.visitVarInsn(ALOAD, i + 1);
			}
			mv.visitMethodInsn(INVOKESTATIC, self_type.getInternalName(),
					javaName, EUtil.getSignature(arity, true));
			mv.visitVarInsn(ASTORE, arity + 1);

			Label done = new Label();
			Label loop = new Label();
			mv.visitLabel(loop);
			mv.visitVarInsn(ALOAD, arity + 1);
			mv.visitFieldInsn(GETSTATIC, EPROC_NAME, "TAIL_MARKER",
					EOBJECT_DESC);
			mv.visitJumpInsn(IF_ACMPNE, done);

			// load proc
			mv.visitVarInsn(ALOAD, 0);
			mv.visitFieldInsn(GETFIELD, EPROC_NAME, "tail", EFUN_DESCRIPTOR);
			mv.visitVarInsn(ALOAD, 0);

			mv.visitMethodInsn(INVOKEVIRTUAL, EFUN_NAME, "go", GO_DESC);
			mv.visitVarInsn(ASTORE, arity + 1);

			mv.visitJumpInsn(GOTO, loop);

			mv.visitLabel(done);
			mv.visitVarInsn(ALOAD, arity + 1);

			/*
			 * } else {
			 * 
			 * mv.visitVarInsn(ALOAD, 0); for (int i = 0; i < arity; i++) {
			 * mv.visitVarInsn(ALOAD, i + 1); } mv.visitMethodInsn(INVOKESTATIC,
			 * self_type.getInternalName(), javaName, EUtil.getSignature(arity,
			 * true));
			 * 
			 * }
			 */

			mv.visitInsn(ARETURN);
			mv.visitMaxs(arity + 2, arity + 2);
			mv.visitEnd();

		}

		/**
		 * 
		 */
		private void generate_tail_call_self() {

			String javaName = EUtil.getJavaName(fun_name, arity);
			String signature = EUtil.getSignature(arity, true);
			mv = cv.visitMethod(ACC_STATIC, javaName + "$tail", signature,
					null, null);
			mv.visitCode();

			// if (isTailRecursive) {

			for (int i = 0; i < arity; i++) {
				mv.visitVarInsn(ALOAD, 0);
				mv.visitVarInsn(ALOAD, i + 1);
				mv
						.visitFieldInsn(PUTFIELD, EPROC_NAME, "arg" + i,
								EOBJECT_DESC);
			}

			mv.visitVarInsn(ALOAD, 0);
			mv.visitFieldInsn(GETSTATIC, self_type.getInternalName(), javaName,
					"L" + EFUN_NAME + arity + ";");
			mv.visitFieldInsn(PUTFIELD, EPROC_NAME, "tail", EFUN_DESCRIPTOR);
			mv.visitFieldInsn(GETSTATIC, EPROC_NAME, "TAIL_MARKER",
					EOBJECT_DESC);

			/*
			 * } else { for (int i = 0; i < arity + 1; i++) {
			 * mv.visitVarInsn(ALOAD, i); }
			 * 
			 * mv.visitMethodInsn(INVOKESTATIC, self_type.getInternalName(),
			 * javaName, signature); }
			 */
			mv.visitInsn(ARETURN);
			mv.visitMaxs(arity + 2, arity + 2);
			mv.visitEnd();
		}

		/**
		 * @param xCount
		 * @param yCount
		 * @param fpCount
		 */
		private void allocate_regs_to_locals(int xCount, int yCount, int fpCount) {

			int max_x = xCount;
			int max_y = yCount;
			int max_f = fpCount;

			int local = 1;

			xregs = new int[max_x];
			for (int i = 0; i < max_x; i++) {
				// mv.visitLocalVariable("X" + i, EOBJECT_DESCRIPTOR,
				// null, start, end, local);
				xregs[i] = local;
				local += 1;

			}

			yregs = new int[max_y];
			for (int i = 0; i < max_y; i++) {
				// mv.visitLocalVariable("Y" + i, EOBJECT_DESCRIPTOR,
				// null, start, end, local);
				yregs[i] = local;
				local += 1;
			}

			fpregs = new int[max_f];
			for (int i = 0; i < max_f; i++) {
				// mv.visitLocalVariable("F" + i,
				// Type.DOUBLE_TYPE.getDescriptor(), null, start, end,
				// local);
				fpregs[i] = local;
				local += 2;
			}

			this.bit_string_builder = local++;
			this.bit_string_matcher = local++;
			this.bit_string_save = local++;
			this.scratch_reg = local;

		}

		private void add_erlfun_annotation(MethodVisitor mv) {
			AnnotationVisitor ann = mv.visitAnnotation(ERLFUN_ANN_TYPE
					.getDescriptor(), true);
			ann.visit("module", module_name.getName());
			ann.visit("name", fun_name.getName());
			ann.visit("arity", new Integer(this.arity));
			ann.visit("export", new Boolean(isExported(fun_name, arity)));
			ann.visitEnd();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see erjang.beam.FunctionVisitor#visitLabeledBlock(int)
		 */
		@Override
		public BlockVisitor visitLabeledBlock(int label) {

			Label blockLabel = getLabel(label);

			mv.visitLabel(blockLabel);
			label_inserted.add(label);
			return new ASMBlockVisitor(label);
		}

		class ASMBlockVisitor implements BlockVisitor2 {
			final int beam_label;
			public ASMBlockVisitor(int beam_label) {this.beam_label=beam_label;}

			@Override
			public void visitBegin(BeamExceptionHandler exh) {
				active_beam_exh = exh;
				if (exh==null || (exh.getHandlerLabel() != beam_label))
					adjust_exception_handlers(exh, false);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitBSAdd(erjang.beam.Arg[],
			 * erjang.beam.Arg)
			 */
			@Override
			public void visitBSAdd(Arg[] in, Arg out) {
				push(in[0], Type.INT_TYPE);
				push(in[1], Type.INT_TYPE);
				mv.visitInsn(IADD);
				pop(out, Type.INT_TYPE);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitBS(erjang.beam.BeamOpcode,
			 * erjang.beam.Arg)
			 */
			@Override
			public void visitBS(BeamOpcode opcode, Arg arg, Arg imm) {
				switch (opcode) {
				case bs_save2:
				case bs_restore2:
					push(arg, EOBJECT_TYPE);
					if (imm.value == ATOM_start) {
					    String methName =
						(opcode == BeamOpcode.bs_restore2)
						? "bs_restore2_start" : "bs_save2_start";
					    mv.visitMethodInsn(INVOKESTATIC, EBINMATCHSTATE_TYPE.getInternalName(), methName, "("+EOBJECT_DESC+")V");
					} else {
						push(imm, Type.INT_TYPE);
						mv.visitMethodInsn(INVOKESTATIC, EBINMATCHSTATE_TYPE.getInternalName(), opcode.name(), "("+EOBJECT_DESC+"I)V");
					}
					
					return;

				case bs_context_to_binary:
					push(arg, EOBJECT_TYPE);
					mv.visitMethodInsn(INVOKESTATIC, EBINMATCHSTATE_TYPE
							.getInternalName(), "bs_context_to_binary", "(" +EOBJECT_DESC + ")" + EBITSTRING_TYPE.getDescriptor());
					pop(arg, EBITSTRING_TYPE);
					return;
				}

				throw new Error("unhandled: " + opcode);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInitBitString(erjang.EObject,
			 * erjang.beam.Arg, erjang.beam.Arg)
			 */
			@Override
			public void visitInitBitString(Arg size, Arg flags, Arg out, boolean unit_is_bits) {

				ETuple tflags = flags.value.testTuple();
				if (tflags.elm(1) != ATOM_field_flags)
					throw new Error("unhandled flags: " + flags.value);
				int iflags = tflags.elm(2).asInt();

				push(size, Type.INT_TYPE);
				mv.visitLdcInsn(new Integer(iflags));
				String methodName = unit_is_bits? "bs_initBits" : "bs_init";
				mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, methodName, "(II)"
						+ EBITSTRINGBUILDER_TYPE.getDescriptor());

				mv.visitInsn(DUP);
				mv.visitVarInsn(ASTORE, bit_string_builder);

				mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
						.getInternalName(), "bitstring", "()"
						+ EBITSTRING_TYPE.getDescriptor());

				pop(out, EBITSTRING_TYPE);

				return;

			}

			@Override
			public void visitBitStringAppend(BeamOpcode opcode, Arg extra_size, Arg src, Arg flags, Arg dst) {

				push(src, EOBJECT_TYPE);
				push(extra_size, Type.INT_TYPE);
				push(flags, Type.INT_TYPE);
				mv.visitMethodInsn(INVOKESTATIC, EBITSTRINGBUILDER_TYPE.getInternalName(),
							"bs_append", "("+ EOBJECT_DESC +"II)"+EBITSTRINGBUILDER_TYPE.getDescriptor());
				
				mv.visitInsn(DUP);
				mv.visitVarInsn(ASTORE, bit_string_builder);

				mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
						.getInternalName(), "bitstring", "()"
						+ EBITSTRING_TYPE.getDescriptor());

				pop(dst, EBITSTRING_TYPE);
				return;
			}
			
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * erjang.beam.BlockVisitor2#visitBitStringPut(erjang.beam.BeamOpcode
			 * , erjang.EObject)
			 */
			@Override
			public void visitBitStringPut(BeamOpcode opcode, Arg arg, Arg size,
										  int unit, int flags)
			{
				switch (opcode) {
				case bs_put_string:
					mv.visitVarInsn(ALOAD, bit_string_builder);
					push(arg, ESTRING_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
							.getInternalName(), "put_string", "("
							+ ESTRING_TYPE.getDescriptor() + ")V");
					return;

				case bs_put_integer:
					mv.visitVarInsn(ALOAD, bit_string_builder);
					push(arg, EINTEGER_TYPE);
					push_scaled(size, unit);
					push_int(flags);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
							.getInternalName(), "put_integer", "("
							+ EOBJECT_DESC + "II)V");
					return;

				case bs_put_float:
					mv.visitVarInsn(ALOAD, bit_string_builder);
					push(arg, Type.DOUBLE_TYPE);
					push_scaled(size, unit);
					push_int(flags);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
									   .getInternalName(), "put_float", "("+EOBJECT_DESC+"II)V");
					return;

				case bs_put_binary:
					assert(unit==8);
					mv.visitVarInsn(ALOAD, bit_string_builder);
					push(arg, EBITSTRING_TYPE);
					push(size, EATOM_TYPE);
					push_int(flags);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
							.getInternalName(), "put_bitstring", "("
							+ EOBJECT_TYPE.getDescriptor()
							+ EATOM_TYPE.getDescriptor() + "I)V");
					return;

				}

				throw new Error("unhandled: " + opcode);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * erjang.beam.BlockVisitor2#visitBitStringTest(erjang.beam.BeamOpcode
			 * , int, erjang.beam.Arg[])
			 */
			@Override
			public void visitBitStringTest(BeamOpcode test, int failLabel,
					Arg[] args) {

				switch (test) {
				case bs_start_match2: {
					Arg in = args[0];
					Arg out = args[3];

					push(in, EOBJECT_TYPE);
					push(args[2], Type.INT_TYPE);
					mv.visitMethodInsn(INVOKESTATIC, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(" + EOBJECT_DESC
							+ "I)" + EMATCHSTATE_TYPE.getDescriptor());

					mv.visitInsn(DUP);
					mv.visitVarInsn(ASTORE, bit_string_matcher);

					mv.visitJumpInsn(IFNULL, getLabel(failLabel));

					mv.visitVarInsn(ALOAD, bit_string_matcher);

					pop(out, EBINMATCHSTATE_TYPE);
					return;
				}

					// {test,bs_test_unit,{f,41},[{x,2},8]}
				case bs_test_unit:
					push(args[0], EBINMATCHSTATE_TYPE);
					push(args[1], Type.INT_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(I)"
							+ EOBJECT_DESC);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
					return;

				case bs_match_string:
					push(args[0], EBINMATCHSTATE_TYPE);
					push(args[1], Type.INT_TYPE);
					push(args[2], EBITSTRING_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(I"
							+ EBITSTRING_TYPE.getDescriptor() + ")"
							+ EBITSTRING_TYPE);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
					return;

					// {test,bs_skip_bits2, {f,39},
					// [{x,1},{x,0},8,{field_flags,0}]}
				case bs_skip_bits2:
					push(args[0], EBINMATCHSTATE_TYPE);
					push(args[1], EINTEGER_TYPE);
					push(args[2], Type.INT_TYPE);
					push_immediate(args[3].value.testTuple().elm(2),
							Type.INT_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(" + EOBJECT_DESC
							+ "II)" + EOBJECT_DESC);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
					return;

					// {test,bs_get_binary2,{f,348},[{x,3},5,{atom,all},8,{field_flags,0},{x,3}]}
				case bs_get_binary2:

					push(args[0], EBINMATCHSTATE_TYPE);
					push(args[2], EOBJECT_TYPE);
					push_immediate(args[4].value.testTuple().elm(2),
							Type.INT_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(" + EOBJECT_DESC
							+ "I)" + EBITSTRING_TYPE);

					mv.visitInsn(DUP);
					mv.visitVarInsn(ASTORE, scratch_reg);

					mv.visitJumpInsn(IFNULL, getLabel(failLabel));

					mv.visitVarInsn(ALOAD, scratch_reg);

					pop(args[5], EBITSTRING_TYPE);
					return;

					// {test,bs_get_integer2,{f,348},[{x,3},4,{integer,32},1,{field_flags,0},{x,4}]}
				case bs_get_integer2:

					push(args[0], EBINMATCHSTATE_TYPE);
					push(args[2], Type.INT_TYPE);
					push(args[4], Type.INT_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(II)"
							+ EINTEGER_TYPE.getDescriptor());
					mv.visitInsn(DUP);
					mv.visitVarInsn(ASTORE, scratch_reg);

					mv.visitJumpInsn(IFNULL, getLabel(failLabel));

					mv.visitVarInsn(ALOAD, scratch_reg);

					pop(args[5], EOBJECT_TYPE);
					return;

				/*{test,bs_skip_utf8,{f,275},[{x,6},7,{field_flags,0}]} */
				case bs_skip_utf8:
				case bs_skip_utf16:
				case bs_skip_utf32:
					push(args[0], EBINMATCHSTATE_TYPE);
					push_immediate(args[2].value.testTuple().elm(2),
							Type.INT_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(I)Z");
					mv.visitJumpInsn(IFEQ, getLabel(failLabel));
					return;

				/* {test,bs_get_utf8,{f,6},[{x,0},1,
				   {field_flags,[...,unsigned,big]},{x,1}]}. */
				case bs_get_utf8:
				case bs_get_utf16:
				case bs_get_utf32:
					push(args[0], EBINMATCHSTATE_TYPE);
					push(args[2], Type.INT_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(I)I");
					mv.visitInsn(DUP);
					mv.visitVarInsn(ISTORE, scratch_reg);

					mv.visitJumpInsn(IFLT, getLabel(failLabel));
					mv.visitVarInsn(ILOAD, scratch_reg);

					pop(args[3], Type.INT_TYPE);
					return;

				case bs_test_tail2:
					push(args[0], EBINMATCHSTATE_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "()"
							+ EOBJECT_DESC);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
					return;

				}

				throw new Error("unhandled bit string test: " + test);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInsn(erjang.beam.BeamOpcode,
			 * int, erjang.beam.Arg[], erjang.beam.Arg)
			 */
			@Override
			public void visitInsn(BeamOpcode opcode, int failLabel, Arg[] in,
					Arg ex) {
				if (opcode == BeamOpcode.raise) {

					push(ex, EATOM_TYPE);
					push(in[1], EATOM_TYPE);
					push(in[0], EOBJECT_TYPE);

					// raise will actually throw
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "raise", "("
							+ EOBJECT_DESC + EOBJECT_DESC + EOBJECT_DESC + ")"
							+ EEXCEPTION_DESC);
					mv.visitInsn(ATHROW);

					return;
				}

				throw new Error("unhandled: " + opcode);

			}

			/**
			 * 
			 */
			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInsn(erjang.beam.BeamOpcode,
			 * int, erjang.beam.Arg[], erjang.beam.Arg,
			 * java.lang.reflect.Method)
			 */
			@Override
			public void visitInsn(BeamOpcode opcode, int failLabel, Arg[] in,
					Arg out, BuiltInFunction bif) {

				ensure_exception_handler_in_place();

				switch (opcode) {
				case gc_bif:
				case bif:
				case arithfbif:
					Type[] parameterTypes = bif.getArgumentTypes();
					// assert (in.length == parameterTypes.length);
					push(in, parameterTypes);

					mv.visitMethodInsn(INVOKESTATIC, bif.owner
							.getInternalName(), bif.getName(), bif
							.getDescriptor());

					if (failLabel != 0) {
						// guard

						// dup result for test
						if (out != null) {

							pop(out, bif.getReturnType());
							push(out, bif.getReturnType());
						}

						if (bif.getReturnType().getSort() == Type.BOOLEAN) {
							mv.visitJumpInsn(IFEQ, getLabel(failLabel));
						} else {
							if (bif.getReturnType().getSort() != Type.OBJECT)
								throw new Error(
										"guards must return object type - "
												+ bif);
							mv.visitJumpInsn(IFNULL, getLabel(failLabel));
						}
					} else {
						pop(out, bif.getReturnType());
					}

					return;

				}

				throw new Error();
			}

			public void visitUnreachablePoint() {
				mv.visitLdcInsn("Reached unreachable point.");
				mv.visitInsn(DUP);
				mv.visitMethodInsn(INVOKESPECIAL, "Ljava/lang/RuntimeException;", "<init>", "(Ljava/lang/String;)");
				mv.visitInsn(ATHROW);
			}

			public void visitCatchBlockStart(BeamOpcode opcode, int label, Arg out, BeamExceptionHandler exh) {
				switch (opcode) {
				case K_try:
				case K_catch: {
					active_beam_exh = exh;
					return;
				}
				}
			}

			public void visitCatchBlockEnd(BeamOpcode opcode, Arg out, BeamExceptionHandler exh) {
				active_beam_exh = exh.getParent();
 				adjust_exception_handlers(active_beam_exh, false);
				switch (opcode) {
				case try_end: {
				} break;

				case catch_end: {
					// Insert exception decoding sequence:
					Label after = new Label();
					mv.visitJumpInsn(GOTO, after);
					mv.visitLabel(getExceptionHandlerLabel(exh));

					// Remember the exception value:
					mv.visitInsn(DUP);
					mv.visitVarInsn(ALOAD, 0);
					mv.visitInsn(SWAP);
					mv.visitFieldInsn(PUTFIELD, EPROC_NAME,
							  "last_exception", EEXCEPTION_DESC);

					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME,
							   "decode_exception2", "("
							   + ERLANG_EXCEPTION_TYPE.getDescriptor()
							   + ")" + EOBJECT_DESC);

					mv.visitVarInsn(ASTORE, xregs[0]);
					mv.visitLabel(after);
				} break;

				case try_case: {
					mv.visitLabel(getExceptionHandlerLabel(exh));

					// Remember the exception value:
					mv.visitInsn(DUP);
					mv.visitVarInsn(ALOAD, 0);
					mv.visitInsn(SWAP);
					mv.visitFieldInsn(PUTFIELD, EPROC_NAME,
							  "last_exception", EEXCEPTION_DESC);

					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME,
							"decode_exception3", "("
									+ ERLANG_EXCEPTION_TYPE.getDescriptor()
									+ ")" + getTubleType(3).getDescriptor());

					mv.visitInsn(DUP);
					mv.visitFieldInsn(GETFIELD, ETUPLE_NAME + 3, "elem1",
							EOBJECT_DESC);
					mv.visitVarInsn(ASTORE, xregs[0]);

					mv.visitInsn(DUP);
					mv.visitFieldInsn(GETFIELD, ETUPLE_NAME + 3, "elem2",
							EOBJECT_DESC);
					mv.visitVarInsn(ASTORE, xregs[1]);

					mv.visitFieldInsn(GETFIELD, ETUPLE_NAME + 3, "elem3",
							EOBJECT_DESC);
					mv.visitVarInsn(ASTORE, xregs[2]);

				} break;
				}
			}


			/**
			 * @param out
			 * @return
			 */
			private int var_index(Arg out) {

				switch (out.kind) {
				case X:
					return xregs[out.no];
				case Y:
					return yregs[out.no];
				case F:
					return fpregs[out.no];
				}

				throw new Error();
			}

			/**
			 * @param out
			 * @param stack_type
			 */
			private void pop(Arg out, Type stack_type) {
				if (out == null) {
					if (stack_type != null
							&& Type.DOUBLE_TYPE.equals(stack_type)) {
						mv.visitInsn(POP2);
					} else {
						mv.visitInsn(POP);
					}
					return;
				}

				if (stack_type == Type.DOUBLE_TYPE
						&& (out.kind == Kind.X || out.kind == Kind.Y)) {
					emit_convert(
							stack_type,
							stack_type == Type.DOUBLE_TYPE ? EDOUBLE_TYPE
									: (stack_type == Type.INT_TYPE ? EINTEGER_TYPE
											: EOBJECT_TYPE));
				}

				if (out.kind == Kind.X || out.kind == Kind.Y) {
					if (out.type == Type.INT_TYPE
							|| out.type == Type.BOOLEAN_TYPE
							|| (out.type == null && stack_type == Type.INT_TYPE)
							|| (out.type == null && stack_type == Type.BOOLEAN_TYPE)) {
						mv.visitVarInsn(ISTORE, var_index(out));
					} else {
						mv.visitVarInsn(ASTORE, var_index(out));
					}
				} else if (out.kind == Kind.F) {

					if (!stack_type.equals(Type.DOUBLE_TYPE)) {
						emit_convert(stack_type, Type.DOUBLE_TYPE);
					}

					mv.visitVarInsn(DSTORE, var_index(out));
				} else {
					throw new Error();
				}

			}

			/**
			 * @param type
			 * @param stackType
			 */
			private void emit_convert(Type from_type, Type to_type) {
				if (from_type.equals(to_type))
					return;
				if (from_type.getSort() == Type.OBJECT
						&& to_type.getSort() == Type.OBJECT) {
					return;
				}

				if (to_type.getSort() == Type.OBJECT) {
					emit_box(from_type, to_type);
				} else {
					emit_unbox(from_type, to_type);
				}

			}

			/**
			 * @param fromType
			 * @param toType
			 */
			private void emit_unbox(Type fromType, Type toType) {
				if (toType.equals(Type.INT_TYPE)) {
					if (fromType.equals(ESMALL_TYPE)) {
						mv.visitFieldInsn(GETFIELD, ESMALL_NAME, "value", "I");
						return;
					}
				}

				if (toType.equals(Type.DOUBLE_TYPE)) {
					if (fromType.equals(EDOUBLE_TYPE)) {
						mv.visitFieldInsn(GETFIELD, EDOUBLE_NAME, "value", "D");
						return;
					}
				}

				mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "unboxTo"
						+ primTypeName(toType), "(" + fromType.getDescriptor()
						+ ")" + toType.getDescriptor());

			}

			/**
			 * @param toType
			 * @return
			 */
			private String primTypeName(Type typ) {
				switch (typ.getSort()) {
				case Type.DOUBLE:
					return "Double";
				case Type.INT:
					return "Int";
				case Type.BOOLEAN:
					return "Atom";
				default:
					throw new Error();
				}
			}

			/**
			 * @param fromType
			 * @param toType
			 */
			private void emit_box(Type fromType, Type toType) {
				if (fromType.equals(Type.INT_TYPE)
						&& toType.getSort() == Type.OBJECT) {
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "box", "(I)"
							+ ESMALL_TYPE.getDescriptor());
				} else if (fromType.equals(Type.DOUBLE_TYPE)
						&& toType.getSort() == Type.OBJECT) {
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "box", "(D)"
							+ EDOUBLE_TYPE.getDescriptor());
				} else if (fromType.equals(Type.BOOLEAN_TYPE)
						&& toType.getSort() == Type.OBJECT) {
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "box", "(Z)"
							+ EATOM_TYPE.getDescriptor());
				} else {
					throw new Error("cannot box " + fromType + " -> " + toType);
				}

			}

			/**
			 * @param in
			 * @param parameterTypes
			 */
			private void push(Arg[] in, Type[] parameterTypes) {
				if (in.length == parameterTypes.length - 1
						&& EPROC_TYPE.equals(parameterTypes[0])) {
					mv.visitVarInsn(ALOAD, 0);
				}

				for (int i = 0; i < in.length; i++) {
					push(in[i], parameterTypes[i]);
				}
			}

			private void push_scaled(Arg value, int factor) {
				if (value.kind == Kind.IMMEDIATE &&
					value.value instanceof ESmall)
				{
					ESmall sm = (ESmall)value.value;
					mv.visitLdcInsn(new Integer(factor * sm.intValue()));
				} else {
					push(value, Type.INT_TYPE);
 					push_int(factor);
					mv.visitInsn(IMUL);
				}
			}

			/**
			 * @param arg
			 * @param class1
			 */
			private void push(Arg value, Type stack_type) {
				Type t = value.type;
				if (value.kind == Kind.X || value.kind == Kind.Y) {
					if (value.type == Type.INT_TYPE
							|| value.type == Type.BOOLEAN_TYPE) {
						// throw new Error("should not happen");
						mv.visitVarInsn(ILOAD, var_index(value));
					} else {
						mv.visitVarInsn(ALOAD, var_index(value));
					}
				} else if (value.kind == Kind.F) {
					mv.visitVarInsn(DLOAD, var_index(value));
				} else if (value.kind == Kind.IMMEDIATE) {
					t = push_immediate(value.value, stack_type);
				} else {
					throw new Error();
				}

				if (t != null && !t.equals(stack_type)) {
					emit_convert(t, stack_type);
				}
			}

			/**
			 * @param value
			 * @param stack_type
			 */
			private Type push_immediate(EObject value, Type stack_type) {

				if (value == ERT.NIL) {
					mv.visitFieldInsn(GETSTATIC, ERT_NAME, "NIL", ENIL_TYPE
							.getDescriptor());
					return ENIL_TYPE;
				}

				if (value == ERT.TRUE) {
					mv.visitFieldInsn(GETSTATIC, ERT_NAME, "TRUE", EATOM_DESC);
					return EATOM_TYPE;
				}

				if (value == ERT.FALSE) {
					mv.visitFieldInsn(GETSTATIC, ERT_NAME, "FALSE", EATOM_DESC);
					return EATOM_TYPE;
				}

				if (stack_type.getSort() != Type.OBJECT) {
					if (value instanceof ESmall) {
						mv.visitLdcInsn(new Integer(value.asInt()));
						return Type.INT_TYPE;
					} else if (value instanceof EDouble) {
						mv.visitLdcInsn(new Double(((EDouble) value).value));
						return Type.DOUBLE_TYPE;
					} else if (value == TRUE_ATOM) {
						mv.visitInsn(ICONST_1);
						return Type.BOOLEAN_TYPE;
					} else if (value == FALSE_ATOM) {
						mv.visitInsn(ICONST_0);
						return Type.BOOLEAN_TYPE;
					}
					if (value instanceof ETuple2) {
						ETuple2 t2 = (ETuple2) value;
						if (t2.elm(1) == ATOM_field_flags) {
							push_int(t2.elem2.asInt());
							return Type.INT_TYPE;
						}
					}
					throw new Error("cannot convert " + value + " as "
							+ stack_type);
				}

				String known = constants.get(value);

				if (known == null) {
					Type type = Type.getType(value.getClass());

					String cn = getConstantName(value, constants.size());

					constants.put(value, known = cn);
					cv.visitField(ACC_STATIC, known, type.getDescriptor(),
							null, null);

					// System.err.println(constants);
				}

				if (known != null) {
					Type type = Type.getType(value.getClass());
					mv.visitFieldInsn(GETSTATIC, self_type.getInternalName(),
							known, type.getDescriptor());
					return type;
				}

				throw new Error("cannot push " + value + " as " + stack_type);

				// return Type.getType(value.getClass());
			}

			/**
			 * @param value
			 * @param size
			 * @return
			 */
			private String getConstantName(EObject value, int size) {
				if (value instanceof EAtom)
					return "atom_" + EUtil.toJavaIdentifier((EAtom) value);
				if (value instanceof ENumber)
					return EUtil.toJavaIdentifier("num_"
							+ value.toString().replace('.', '_').replace('-',
									'_'));
				if (value instanceof EString)
					return "str_" + size;
				else
					return "cst_" + size;
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * erjang.beam.BlockVisitor2#visitReceive(erjang.beam.BeamOpcode,
			 * int, erjang.beam.Arg)
			 */
			@Override
			public void visitReceive(BeamOpcode opcode, int blockLabel, Arg out) {
				switch (opcode) {
				case loop_rec:
					ensure_exception_handler_in_place();
					
					mv.visitVarInsn(ALOAD, 0);
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "receive_peek",
							"(" + EPROC_TYPE.getDescriptor() + ")"
									+ EOBJECT_DESC);
					mv.visitInsn(DUP);
					pop(out, EOBJECT_TYPE);

					mv.visitJumpInsn(IFNULL, getLabel(blockLabel));

					return;

				}

				throw new Error();
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInsn(erjang.beam.BeamOpcode,
			 * erjang.beam.Arg[], erjang.beam.Arg)
			 */
			@Override
			public void visitInsn(BeamOpcode opcode, Arg[] in, Arg out) {
				switch (opcode) {
				case put_list:
					push(in[0], EOBJECT_TYPE);
					push(in[1], EOBJECT_TYPE);
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "cons", "("
							+ EOBJECT_DESC + EOBJECT_DESC + ")"
							+ ECONS_TYPE.getDescriptor());
					pop(out, ECONS_TYPE);
					return;
				case call_fun: {
					ensure_exception_handler_in_place();
					
					int nargs = in.length - 1;
					push(in[nargs], EOBJECT_TYPE);

					String funtype = EFUN_NAME + nargs;

					mv.visitMethodInsn(INVOKESTATIC, funtype, "cast", "("
							+ EOBJECT_DESC + ")L" + funtype + ";");

					mv.visitVarInsn(ALOAD, 0); // load proc
					for (int i = 0; i < nargs; i++) {
						push(in[i], EOBJECT_TYPE);
					}

					mv.visitMethodInsn(INVOKEVIRTUAL, funtype, "invoke", EUtil
							.getSignature(nargs, true));
					pop(out, EOBJECT_TYPE);
					return;
				}
				}

				throw new Error();
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInsn(erjang.beam.BeamOpcode)
			 */
			@Override
			public void visitInsn(BeamOpcode insn) {
				switch (insn) {
				case if_end:
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "if_end", EUtil
							.getSignature(0, false));
					mv.visitInsn(ARETURN);
					return;

				case timeout:
					mv
							.visitMethodInsn(INVOKESTATIC, ERT_NAME, "timeout",
									"()V");
					return;

				case remove_message:
					mv.visitVarInsn(ALOAD, 0);
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME,
							"remove_message", "(" + EPROC_TYPE.getDescriptor()
									+ ")V");
					return;

				}

				throw new Error();
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInsn(erjang.beam.BeamOpcode,
			 * int, erjang.beam.Arg)
			 */
			@Override
			public void visitInsn(BeamOpcode opcode, int val, Arg out) {
				switch (opcode) {
				case put_tuple:
					mv.visitTypeInsn(NEW, out.type.getInternalName());
					mv.visitInsn(DUP);
					mv.visitMethodInsn(INVOKESPECIAL, out.type
							.getInternalName(), "<init>", "()V");
					pop(out, out.type);

					return;

				case wait_timeout: {
					mv.visitVarInsn(ALOAD, 0);
					push(out, EOBJECT_TYPE);
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "wait_timeout",
							"(" + EPROC_TYPE.getDescriptor()
									+ EOBJECT_TYPE.getDescriptor() + ")Z");

					mv.visitJumpInsn(IFNE, getLabel(val));
					return;
				}

				case loop_rec_end:
					mv.visitVarInsn(ALOAD, 0);
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "loop_rec_end",
							"(" + EPROC_TYPE.getDescriptor() + ")V");
					mv.visitJumpInsn(GOTO, getLabel(val));
					return;


				case wait: {
					mv.visitVarInsn(ALOAD, 0);
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "wait_forever",
							"(" + EPROC_TYPE.getDescriptor() + ")V");

					mv.visitJumpInsn(GOTO, getLabel(val));
					return;
				}
				}
				throw new Error("unhandled: " + opcode);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInsn(erjang.beam.BeamOpcode,
			 * erjang.beam.Arg, erjang.beam.Arg, int)
			 */
			@Override
			public void visitInsn(BeamOpcode opcode, Arg val, Arg out, int pos) {
				if (opcode == BeamOpcode.put) {
					push(out, out.type);
					push(val, EOBJECT_TYPE);
					mv.visitFieldInsn(PUTFIELD, out.type.getInternalName(),
							"elem" + pos, EOBJECT_DESC);
					return;

				} else if (opcode == BeamOpcode.get_tuple_element) {
					push(val, val.type);
					mv.visitFieldInsn(GETFIELD, val.type.getInternalName(),
							"elem" + (pos + 1), EOBJECT_DESC);
					pop(out, EOBJECT_TYPE);
					return;
				} else if (opcode == BeamOpcode.set_tuple_element) {

					push(out, out.type);
					mv.visitTypeInsn(CHECKCAST, ETUPLE_NAME);
					push_int(pos + 1);
					push(val, val.type);
					mv.visitMethodInsn(INVOKEVIRTUAL, ETUPLE_NAME, "set", "(I"
							+ EOBJECT_DESC + ")V");
					return;
				}

				throw new Error();
			}

			/**
			 * @param pos
			 */
			private void push_int(int pos) {
				if (pos >= -1 && pos <= 5) {
					mv.visitInsn(ICONST_0 + pos);
				} else {
					mv.visitLdcInsn(new Integer(pos));
				}
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitTest(erjang.beam.BeamOpcode,
			 * int, erjang.beam.Arg, java.lang.reflect.Method)
			 */
			@Override
			public void visitInsn(BeamOpcode test, int failLabel, Arg arg1,
					org.objectweb.asm.commons.Method bif) {
				throw new Error();
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor#visitEnd()
			 */
			@Override
			public void visitEnd() {
				// skip //
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor#visitInsn(erjang.beam.BeamOpcode,
			 * erjang.ETuple)
			 */
			@Override
			public void visitInsn(BeamOpcode opcode, ETuple et) {
				throw new Error();
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInsn(erjang.beam.BeamOpcode,
			 * erjang.beam.Arg)
			 */
			@Override
			public void visitInsn(BeamOpcode opcode, Arg arg) {
				switch (opcode) {
				case K_return:
					push(arg, EOBJECT_TYPE);
					mv.visitInsn(ARETURN);
					return;

				case func_info:
					ExtFunc f = (ExtFunc) arg;
					push_immediate(f.mod, EATOM_TYPE);
					push_immediate(f.fun, EATOM_TYPE);

					push_immediate(ERT.NIL, ENIL_TYPE);
					for (int i=arg.no-1; i>=0; i--) {
						push(new Arg(Kind.X, i), EOBJECT_TYPE);
						mv.visitMethodInsn(INVOKEVIRTUAL, ESEQ_NAME, "cons", "("
										   + EOBJECT_DESC + ")"
										   + ESEQ_DESC);
					}

					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "func_info", "("
							+ EATOM_DESC + EATOM_DESC + ESEQ_DESC +")" + EOBJECT_DESC);
					mv.visitInsn(ARETURN);
					return;

				case init:
					push_immediate(ERT.NIL, ENIL_TYPE);
					pop(arg, ENIL_TYPE);
					return;

				case try_case_end:
					push(arg, EOBJECT_TYPE);
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "try_case_end",
							EUtil.getSignature(1, false));
					mv.visitInsn(ARETURN);
					return;

				case case_end:
					push(arg, EOBJECT_TYPE);
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "case_end",
							EUtil.getSignature(1, false));
					mv.visitInsn(ARETURN);
					return;

				case badmatch:
					push(arg, EOBJECT_TYPE);
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "badmatch",
							EUtil.getSignature(1, false));
					mv.visitInsn(ARETURN);
					return;

				}

				throw new Error("unhandled " + opcode);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitTest(erjang.beam.BeamOpcode,
			 * int, erjang.beam.Arg)
			 */
			@Override
			public void visitTest(BeamOpcode test, int failLabel, Arg arg1,
					Type out) {
				Method test_bif = get_test_bif(test, arg1.type);

				// System.err.println(test_bif);

				push(arg1, arg1.type);

				mv.visitMethodInsn(INVOKEVIRTUAL, EOBJECT_NAME, test_bif
						.getName(), test_bif.getDescriptor());

				Type returnType = test_bif.getReturnType();
				if (failLabel != 0) {
					// guard
					if (returnType.getSort() != Type.OBJECT)
						throw new Error("guards must return object type: "
								+ test_bif);

					// dup result for test
					if (arg1 != null) {
						mv.visitInsn(DUP);
						mv.visitVarInsn(ASTORE, scratch_reg);
					}
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
					if (arg1 != null) {
						mv.visitVarInsn(ALOAD, scratch_reg);
						pop(arg1, returnType);
					}
				} else {
					pop(arg1, returnType);
				}

			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitTest(erjang.beam.BeamOpcode,
			 * int, erjang.beam.Arg, int, org.objectweb.asm.Type)
			 */
			@Override
			public void visitTest(BeamOpcode test, int failLabel, Arg arg,
					int arity, Type tupleType) {

				switch (test) {
				case test_arity: {
					Type tt = getTubleType(arity);
					push(arg, EOBJECT_TYPE);
					mv.visitMethodInsn(INVOKESTATIC, tt.getInternalName(),
							"cast", "(" + arg.type.getDescriptor() + ")"
									+ tt.getDescriptor());
					mv.visitInsn(DUP);

					mv.visitVarInsn(ASTORE, scratch_reg);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));

					mv.visitVarInsn(ALOAD, scratch_reg);
					pop(arg, getTubleType(arity));
					return;
				}

				case is_function2:

					// push object to test
					push(arg, EOBJECT_TYPE);
					// push arity
					push_int(arity);
					// call object.testFunction2(nargs)
					mv.visitMethodInsn(INVOKEVIRTUAL, EOBJECT_NAME,
							"testFunction2", "(I)" + EFUN_DESCRIPTOR);

					mv.visitInsn(DUP);

					mv.visitVarInsn(ASTORE, scratch_reg);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));

					mv.visitVarInsn(ALOAD, scratch_reg);
					pop(arg, EFUN_TYPE);
					return;
				}
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitTest(erjang.beam.BeamOpcode,
			 * int, erjang.beam.Arg[], erjang.beam.Arg, org.objectweb.asm.Type)
			 */
			@Override
			public void visitTest(BeamOpcode test, int failLabel, Arg[] args,
					Arg out, Type outType) {

				if (test == BeamOpcode.is_eq_exact
						&& (args[0].type.equals(EATOM_TYPE) || args[1].type
								.equals(EATOM_TYPE))) {
					push(args[0], EOBJECT_TYPE);
					push(args[1], EOBJECT_TYPE);
					mv.visitJumpInsn(IF_ACMPNE, getLabel(failLabel));
					return;
				}

				BuiltInFunction bif = BIFUtil.getMethod(test2name(test), args,
						true);

				if (bif.getArgumentTypes().length > 0
						&& EPROC_TYPE.equals(bif.getArgumentTypes()[0])) {
					mv.visitVarInsn(ALOAD, 0);
				}

				for (int i = 0; i < args.length; i++) {
					push(args[i], bif.getArgumentTypes()[i]);
				}

				mv.visitMethodInsn(INVOKESTATIC, bif.owner.getInternalName(),
						bif.getName(), bif.getDescriptor());

				if (failLabel != 0) {
					// guard
					switch (bif.getReturnType().getSort()) {

					case Type.OBJECT:

						if (out != null) {
							pop(out, bif.getReturnType());
							push(out, bif.getReturnType());
						}

						mv.visitJumpInsn(IFNULL, getLabel(failLabel));
						break;
					default:
						throw new Error("guards must return object type: "
								+ bif);
					}

				} else {
					pop(out, bif.getReturnType());
				}

				return;

			}

			/**
			 * @param test
			 * @return
			 */
			private String test2name(BeamOpcode test) {
				switch (test) {
				case is_eq:
					return "==";
				case is_eq_exact:
					return "=:=";
				case is_ne:
					return "/=";
				case is_ne_exact:
					return "=/=";
				case is_ge:
					return ">=";
				case is_lt:
					return "<";
				}
				return test.name();
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInsn(erjang.beam.BeamOpcode,
			 * erjang.beam.Arg, erjang.beam.Arg)
			 */
			@Override
			public void visitInsn(BeamOpcode opcode, Arg arg1, Arg arg2) {

				switch (opcode) {
				case fconv:
				case move:
				case fmove:
					if (arg1.kind == Kind.F) {
						push(arg1, Type.DOUBLE_TYPE);

						if (arg2.kind == Kind.F) {
							pop(arg2, Type.DOUBLE_TYPE);
						} else {
							emit_convert(Type.DOUBLE_TYPE, EDOUBLE_TYPE);
							pop(arg2, EDOUBLE_TYPE);
						}
					} else {
						if (arg2.kind == Kind.F) {
							push(arg1, Type.DOUBLE_TYPE);
							pop(arg2, Type.DOUBLE_TYPE);
						} else {
							push(arg1, arg1.type);
							pop(arg2, arg1.type);
						}
					}
					break;

				default:
					throw new Error("unhandled: " + opcode);
				}

			}

			/**
			 * @param test
			 * @param args
			 * @return
			 */
			private Method get_test_bif(BeamOpcode test, Type type) {

				if (!type.getInternalName().startsWith("erjang/E")) {
					throw new Error("expecting EObject");
				}

				switch (test) {
				case is_nonempty_list:
					return IS_NONEMPTY_LIST_TEST;
				case is_nil:
					return IS_NIL_TEST;
				case is_boolean:
					return IS_BOOLEAN_TEST;
				case is_number:
					return IS_NUMBER_TEST;
				case is_float:
					return IS_FLOAT_TEST;
				case is_atom:
					return IS_ATOM_TEST;
				case is_list:
					return IS_LIST_TEST;
				case is_tuple:
					return IS_TUPLE_TEST;
				case is_integer:
					return IS_INTEGER_TEST;
				case is_binary:
					return IS_BINARY_TEST;
				case is_bitstr:
					return IS_BITSTRING_TEST;
				case is_pid:
					return IS_PID_TEST;
				case is_port:
					return IS_PORT_TEST;
				case is_reference:
					return IS_REFERENCE_TEST;
				case is_function:
					return IS_FUNCTION_TEST;
				case is_function2:
					return IS_FUNCTION2_TEST;
				}

				throw new Error("unhandled " + test);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInsn(erjang.beam.BeamOpcode,
			 * erjang.beam.Arg[])
			 */
			@Override
			public void visitInsn(BeamOpcode opcode, Arg[] ys) {

				if (opcode == BeamOpcode.allocate_zero
						|| opcode == BeamOpcode.allocate_heap_zero) {

					mv.visitFieldInsn(GETSTATIC, ERT_NAME, "NIL", ENIL_TYPE
							.getDescriptor());
					for (int i = 0; i < ys.length; i++) {
						if (i != (ys.length - 1))
							mv.visitInsn(DUP);
						mv.visitInsn(NOP);
						pop(ys[i], ENIL_TYPE);
					}

					return;
				} else if (opcode == BeamOpcode.get_list) {

					push(ys[0], ECONS_TYPE);
					mv.visitInsn(DUP);
					mv.visitMethodInsn(INVOKEVIRTUAL, ECONS_TYPE
							.getInternalName(), "head", "()" + EOBJECT_DESC);
					pop(ys[1], EOBJECT_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, ECONS_TYPE
							.getInternalName(), "tail", "()" + EOBJECT_DESC);
					pop(ys[2], EOBJECT_TYPE);
					return;

				} 

				ensure_exception_handler_in_place();

				if (opcode == BeamOpcode.apply || opcode == BeamOpcode.apply_last) {

					int arity = ys.length-2;
					push(ys[ys.length-2], EOBJECT_TYPE); // push mod
					push(ys[ys.length-1], EOBJECT_TYPE); // push fun
					push_int(arity); // push arity

					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "resolve_fun", "("+EOBJECT_DESC+EOBJECT_DESC+"I)"+EFUN_DESCRIPTOR);
					
					String funtype = EFUN_NAME + arity;

					mv.visitMethodInsn(INVOKESTATIC, funtype, "cast", "("
							+ EOBJECT_DESC + ")L" + funtype + ";");
					
					mv.visitVarInsn(ALOAD, 0); // push eproc
					int loops = 0;
					for (int i = 0; i <= ys.length-3; i++) {
						push(ys[i], EOBJECT_TYPE);
						loops += 1;
					}
					
					if (loops != arity) {
						// invariant for above complicated loop logic.
						throw new InternalError("bad args here");
					}
					
					boolean is_tail = opcode == BeamOpcode.apply_last;
					if (is_tail) {
						mv.visitMethodInsn(INVOKEVIRTUAL, funtype, "invoke_tail", EUtil
									.getSignature(arity, true));
						mv.visitInsn(ARETURN);						
					} else {
						mv.visitMethodInsn(INVOKEVIRTUAL, funtype,
								"invoke", EUtil
										.getSignature(arity, true));
											
						mv.visitVarInsn(ASTORE, xregs[0]);
					}
						
					return;


				} else if (opcode == BeamOpcode.send) {
					mv.visitVarInsn(ALOAD, 0);
					for (int i = 0; i < ys.length; i++) {
						push(ys[i], EOBJECT_TYPE);
					}
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "send", EUtil
							.getSignature(ys.length, true));
					mv.visitVarInsn(ASTORE, xregs[0]);

					return;
				}

				throw new Error("unhandled:" + opcode);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInsn(erjang.beam.BeamOpcode,
			 * erjang.beam.ExtFunc, int)
			 */
			@Override
			public void visitInsn(BeamOpcode opcode, ExtFunc efun,
					Arg[] freevars) {
				ensure_exception_handler_in_place();

				if (opcode == BeamOpcode.make_fun2) {

					CompilerVisitor.this.register_lambda(efun.fun, efun.no,
							freevars.length);

					String inner = EUtil.getFunClassName(self_type, efun);

					mv.visitTypeInsn(NEW, inner);
					mv.visitInsn(DUP);

					// String funtype = EFUN_NAME + efun.no;
					for (int i = 0; i < freevars.length; i++) {
						push(freevars[i], EOBJECT_TYPE);
					}

					StringBuilder sb = new StringBuilder("(");
					for (int i = 0; i < freevars.length; i++) {
						sb.append(EOBJECT_DESC);
					}
					sb.append(")V");
					// sb.append("L").append(funtype).append(";");
					String gen_fun_desc = sb.toString();

					mv.visitMethodInsn(INVOKESPECIAL, inner, "<init>",
							gen_fun_desc);

					mv.visitVarInsn(ASTORE, xregs[0]);

					return;
				}

				throw new Error();
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitSelectValue(erjang.beam.Arg,
			 * int, erjang.beam.Arg[], int[])
			 */
			@Override
			public void visitSelectValue(Arg in, int failLabel, Arg[] values,
					int[] targets) {

				boolean all_small_ints = true;
				for (int i = 0; i < values.length; i++) {
					if (!(values[i].value instanceof ESmall)) {
						all_small_ints = false;
						break;
					}
				}

				if (all_small_ints) {
					int[] ivals = new int[values.length];
					Label[] label = new Label[values.length];

					for (int i = 0; i < values.length; i++) {
						ivals[i] = values[i].value.asInt();
						label[i] = getLabel(targets[i]);
					}
					push(in, Type.INT_TYPE);

					sort(ivals, label);

					mv.visitLookupSwitchInsn(getLabel(failLabel), ivals, label);
					return;
				}

				class Case implements Comparable<Case> {

					final Arg arg;
					final Label label;

					/**
					 * @param arg
					 * @param label2
					 */
					public Case(Arg arg, Label label) {
						this.arg = arg;
						this.label = label;
					}

					EObject value() {
						return arg.value;
					}

					/*
					 * (non-Javadoc)
					 * 
					 * @see java.lang.Comparable#compareTo(java.lang.Object)
					 */
					@Override
					public int compareTo(Case o) {
						int h = hashCode();
						int ho = o.hashCode();
						if (h < ho)
							return -1;
						if (h > ho)
							return 1;
						return value().compareTo(o.value());
					}

				}

				Map<Integer, List<Case>> cases = new TreeMap<Integer, List<Case>>();

				for (int i = 0; i < values.length; i++) {
					int hash = values[i].value.hashCode();
					List<Case> c = cases.get(hash);
					if (c == null) {
						cases.put(hash, c = new ArrayList<Case>());
					}
					c.add(new Case(values[i], getLabel(targets[i])));
				}

				int[] hashes = new int[cases.size()];
				Label[] tests = new Label[cases.size()];
				List<Case>[] idx_cases = new List[cases.size()];

				int idx = 0;
				for (Map.Entry<Integer, List<Case>> c : cases.entrySet()) {
					hashes[idx] = c.getKey();
					idx_cases[idx] = c.getValue();
					tests[idx++] = new Label();
				}

				push(in, EOBJECT_TYPE);
				mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object",
						"hashCode", "()I");
				mv.visitLookupSwitchInsn(getLabel(failLabel), hashes, tests);

				for (int i = 0; i < idx_cases.length; i++) {

					mv.visitLabel(tests[i]);

					for (Case c : idx_cases[i]) {
						Arg val_j = c.arg;
						Label target_j = c.label;

						if (val_j.type.equals(EATOM_TYPE)) {

							push(in, in.type);
							push(val_j, val_j.type);
							mv.visitJumpInsn(IF_ACMPEQ, target_j);

						} else {

							if (in.type == val_j.type) {
								push(in, in.type);
								push(val_j, val_j.type);

								mv.visitMethodInsn(INVOKESTATIC, ERT_NAME,
										"eq", "(" + in.type.getDescriptor()
												+ in.type.getDescriptor()
												+ ")Z");
							} else {
								push(in, EOBJECT_TYPE);
								push(val_j, EOBJECT_TYPE);
								mv.visitMethodInsn(INVOKEVIRTUAL,
										"java/lang/Object", "equals",
										"(Ljava/lang/Object;)Z");
							}
							mv.visitJumpInsn(IFNE, target_j);
						}
					}

					mv.visitJumpInsn(GOTO, getLabel(failLabel));
				}

			}

			/**
			 * @param ivals
			 * @param label
			 */
			private int[] sort(int[] ivals, Label[] label) {

				Label[] orig_labels = label.clone();
				int[] orig_ivals = ivals.clone();
				int[] res = new int[ivals.length];

				Arrays.sort(ivals);

				next_val: for (int i = 0; i < ivals.length; i++) {

					int find = ivals[i];

					for (int p = 0; p < orig_ivals.length; p++) {

						int was = orig_ivals[p];

						if (find == was) {
							res[p] = i;
							label[i] = orig_labels[p];
							continue next_val;
						}

					}

				}

				return res;

			}
			
			/** class that we use to sort the labels for select_arity switch */
			class TupleArityLabel implements Comparable<TupleArityLabel> {
				Label cast_label = new Label();
				Label target;
				int arity;
				
				public TupleArityLabel(int arity, Label target) {
					this.arity = arity;
					this.target = target;					
				}
				
				@Override
				public int compareTo(TupleArityLabel o) {
					if (this.arity < o.arity)
						return -1;
					if (this.arity == o.arity)
						return 0;
					return 1;
				}
			}

			/** Switch based on arity of incoming <code>in</code> tuple value.  */
			@Override
			public void visitSelectTuple(Arg in, int failLabel, int[] arities,
					int[] targets) {

				push(in, ETUPLE_TYPE);

				mv.visitMethodInsn(INVOKEVIRTUAL, ETUPLE_NAME, "arity", "()I");
				
				TupleArityLabel[] cases = new TupleArityLabel[targets.length];
				for (int i = 0; i < targets.length; i++) {
					cases[i] = new TupleArityLabel(arities[i], getLabel(targets[i]));
				}

				Arrays.sort(cases);
				
				Label[] casts = new Label[cases.length];
				int[] values = new int[cases.length];
				
				for (int i = 0; i < cases.length; i++) {
					values[i] = cases[i].arity;
					casts[i] = cases[i].cast_label;
				}

				mv.visitLookupSwitchInsn(getLabel(failLabel), values, casts);
				for (int i = 0; i < cases.length; i++) {
					mv.visitLabel(cases[i].cast_label);
					
					// NOTE: unfortunately, we have to use a cast here.  There
					// is no real way around it, except maybe make some 
					// special ETuple methods for small tuple sizes?
					// that is an option for future optimization of pattern match.
					push(in, ETUPLE_TYPE);
					mv.visitTypeInsn(CHECKCAST, getTubleType(cases[i].arity).getInternalName());
					pop(in, getTubleType(cases[i].arity));
					mv.visitJumpInsn(GOTO, cases[i].target);
				}

			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitJump(int)
			 */
			@Override
			public void visitJump(int label) {
				mv.visitJumpInsn(GOTO, getLabel(label));
			}

			/**
			 * @param i
			 * @return
			 */
			private Type getTubleType(int i) {
				return Type.getType("L" + ETUPLE_NAME + i + ";");
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitCall(erjang.beam.ExtFunc,
			 * erjang.beam.Arg[], boolean, boolean)
			 */
			@Override
			public void visitCall(ExtFunc fun, Arg[] args, boolean is_tail,
					boolean isExternal) {

				ensure_exception_handler_in_place();

				if (false /* fun.mod == ERLANG_ATOM */) {

					System.err.println(fun);

					// functions in module "erlang" compile to direct calls

					BuiltInFunction m = BIFUtil.getMethod(fun.fun.getName(),
							args, false);

					if (m == null) {
						throw new Error("did not find " + fun);
					}

					if (m.getArgumentTypes().length > 0
							&& EPROC_TYPE.equals(m.getArgumentTypes()[0])) {
						mv.visitVarInsn(ALOAD, 0);

						assert (args.length + 1 == m.getArgumentTypes().length);
					} else {
						assert (args.length == m.getArgumentTypes().length);
					}

					push(args, m.getArgumentTypes());

					mv.visitMethodInsn(INVOKESTATIC, m.owner.getInternalName(),
							m.getName(), m.getDescriptor());

					if (is_tail) {
						emit_convert(m.getReturnType(), EOBJECT_TYPE);
						mv.visitInsn(ARETURN);
					} else {
						// emit_convert(m.getReturnType(), EOBJECT_TYPE);
						pop(new Arg(Arg.Kind.X, 0), m.getReturnType());
					}

				} else if (isExternal) {

					BuiltInFunction bif = null;

					try {
						bif = BIFUtil.getMethod(fun.toString(), args, false);
					} catch (Error e) {
						// ignore //
					}

					if (bif == null) {

						String field = CompilerVisitor.this
								.getExternalFunction(fun);

						String funTypeName = EFUN_NAME + args.length;
						EFun.ensure(args.length);
						mv.visitFieldInsn(GETSTATIC, self_type
								.getInternalName(), field, "L" + funTypeName
								+ ";");
						mv.visitVarInsn(ALOAD, 0);
						for (int i = 0; i < args.length; i++) {
							push(args[i], EOBJECT_TYPE);
						}

						mv.visitMethodInsn(INVOKEVIRTUAL, funTypeName,
								is_tail ? "invoke_tail" : "invoke", EUtil
										.getSignature(args.length, true));

					} else {

						// System.err.println("DIRECT "+bif);
						
						int off = 0;
						if (bif.getArgumentTypes().length > 0
								&& bif.getArgumentTypes()[0].equals(EPROC_TYPE)) {

							mv.visitVarInsn(ALOAD, 0);
							off = 1;
						}
						for (int i = 0; i < args.length; i++) {
							push(args[i], bif.getArgumentTypes()[off + i]);
						}

						mv.visitMethodInsn(INVOKESTATIC, bif.owner
								.getInternalName(), bif.getName(), bif
								.getDescriptor());

					}

					if (is_tail || isExitFunc(fun)) {
						mv.visitInsn(ARETURN);
					} else {
						mv.visitVarInsn(ASTORE, xregs[0]);
					}

				} else {

					// are we self-recursive?
					if (is_tail && fun.no == ASMFunctionAdapter.this.arity
							&& fun.fun == ASMFunctionAdapter.this.fun_name) {

						mv.visitVarInsn(ALOAD, 0);
						mv.visitMethodInsn(INVOKEVIRTUAL, EPROC_NAME,
								"check_exit", "()V");

						// System.out.println("self-recursive in " + fun);
						mv.visitJumpInsn(GOTO,
								getLabel(ASMFunctionAdapter.this.startLabel));
						return;

					}

					mv.visitVarInsn(ALOAD, 0);
					for (int i = 0; i < args.length; i++) {
						push(args[i], EOBJECT_TYPE);
					}

					mv.visitMethodInsn(INVOKESTATIC, self_type
							.getInternalName(), EUtil.getJavaName(fun.fun,
							fun.no)
							+ (is_tail ? "$tail" : "$call"), EUtil
							.getSignature(args.length, true));

					if (is_tail) {
						mv.visitInsn(ARETURN);
					} else {
						mv.visitVarInsn(ASTORE, xregs[0]);
					}

				}
			}

			/**
			 * @param fun
			 * @return
			 */
			private boolean isExitFunc(ExtFunc fun) {
				if (fun.mod == ERLANG_ATOM) {
					if (fun.fun.getName().equals("exit"))
						return true;
					if (fun.fun.getName().equals("error"))
						return true;
					if (fun.fun.getName().equals("throw"))
						return true;
				}

				return false;
			}
		}

	}

	static Method IS_NONEMPTY_LIST_TEST = Method
			.getMethod("erjang.ECons testNonEmptyList()");
	static Method IS_LIST_TEST = Method.getMethod("erjang.ECons testCons()");
	static Method IS_TUPLE_TEST = Method.getMethod("erjang.ETuple testTuple()");
	static Method IS_INTEGER_TEST = Method
			.getMethod("erjang.EInteger testInteger()");
	static Method IS_ATOM_TEST = Method.getMethod("erjang.EAtom testAtom()");
	static Method IS_FLOAT_TEST = Method
			.getMethod("erjang.EDouble testFloat()");
	static Method IS_NIL_TEST = Method.getMethod("erjang.ENil testNil()");
	static Method IS_BOOLEAN_TEST = Method
			.getMethod("erjang.EAtom testBoolean()");
	static Method IS_NUMBER_TEST = Method
			.getMethod("erjang.ENumber testNumber()");
	static Method IS_BINARY_TEST = Method
	.getMethod("erjang.EBinary testBinary()");
	static Method IS_BITSTRING_TEST = Method
	.getMethod("erjang.EBitString testBitString()");
	static Method IS_PID_TEST = Method.getMethod("erjang.EPID testPID()");
	static Method IS_PORT_TEST = Method.getMethod("erjang.EPort testPort()");
	static Method IS_REFERENCE_TEST = Method.getMethod(ERef.class.getName()
			+ " testReference()");
	static Method IS_FUNCTION_TEST = Method
			.getMethod("erjang.EFun testFunction()");
	static Method IS_FUNCTION2_TEST = Method
			.getMethod("erjang.EFun testFunction(int nargs)");

	Map<String, ExtFunc> imported = new HashMap<String, ExtFunc>();

	/**
	 * @param fun
	 * @return
	 */
	public String getExternalFunction(ExtFunc fun) {

		String name = EUtil.getJavaName(fun);
		if (!imported.containsKey(name)) {
			imported.put(name, fun);
		}

		return name;
	}

	/**
	 * @param fun
	 * @param arity
	 * @param length
	 */
	public void register_lambda(EAtom fun, int arity, int length) {
		lambdas_xx.put(EUtil.getJavaName(fun, arity), length);
	}

	public int get_lambda(EAtom fun, int arity) {
		Integer val = lambdas_xx.get(EUtil.getJavaName(fun, arity));
		if (val == null)
			return -1;
		return val;
	}

	static public byte[] make_invoker(Type self_type, String mname,
			String fname, int arity, boolean proc, int freevars,
			Type return_type) {

		String outer_name = self_type.getInternalName();
		String inner_name = "FN_" + mname;
		String full_inner_name = outer_name + "$" + inner_name;

		ClassWriter cw = new ClassWriter(true);
		String super_class_name = EFUN_NAME + (arity - freevars);
		EFun.ensure(arity - freevars);
		cw.visit(V1_6, ACC_FINAL | ACC_PUBLIC, full_inner_name, null,
				super_class_name, null);

		StringBuilder sb = new StringBuilder("(");
		// create the free vars
		for (int i = 0; i < freevars; i++) {
			cw.visitField(ACC_PRIVATE | ACC_FINAL, "fv" + i, EOBJECT_DESC,
					null, null);
			sb.append(EOBJECT_DESC);
		}
		sb.append(")V");

		MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "<init>", sb.toString(),
				null, null);
		mv.visitCode();

		mv.visitVarInsn(ALOAD, 0);
		mv.visitMethodInsn(INVOKESPECIAL, super_class_name, "<init>", "()V");

		for (int i = 0; i < freevars; i++) {
			mv.visitVarInsn(ALOAD, 0);
			mv.visitVarInsn(ALOAD, i + 1);
			mv
					.visitFieldInsn(PUTFIELD, full_inner_name, "fv" + i,
							EOBJECT_DESC);
		}

		mv.visitInsn(RETURN);
		mv.visitMaxs(3, 3);
		mv.visitEnd();

		make_invoke_method(cw, outer_name, fname, arity, proc, freevars,
				return_type);
		make_invoketail_method(cw, full_inner_name, arity, freevars);
		make_go_method(cw, outer_name, fname, full_inner_name, arity, proc,
				freevars, return_type);

		return cw.toByteArray();

	}

	private static void make_invoke_method(ClassWriter cw, String outer_name,
			String mname, int arity, boolean proc, int freevars, Type returnType) {
		MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "invoke", EUtil
				.getSignature(arity - freevars, true), null, PAUSABLE_EX);
		mv.visitCode();
		if (proc) {
			mv.visitVarInsn(ALOAD, 1);
		}
		for (int i = 0; i < arity - freevars; i++) {
			mv.visitVarInsn(ALOAD, i + 2);
		}
		for (int i = 0; i < freevars; i++) {
			mv.visitVarInsn(ALOAD, 0);
			mv.visitFieldInsn(GETFIELD, outer_name + "$FN_" + mname, "fv" + i,
					EOBJECT_DESC);
		}
		mv.visitMethodInsn(INVOKESTATIC, outer_name, mname, EUtil.getSignature(
				arity, proc, returnType));
		mv.visitVarInsn(ASTORE, arity + 2);

		Label done = new Label();
		Label loop = new Label();
		mv.visitLabel(loop);
		mv.visitVarInsn(ALOAD, arity + 2);
		mv.visitFieldInsn(GETSTATIC, EPROC_NAME, "TAIL_MARKER", EOBJECT_DESC);
		mv.visitJumpInsn(IF_ACMPNE, done);

		// load proc
		mv.visitVarInsn(ALOAD, 1);
		mv.visitFieldInsn(GETFIELD, EPROC_NAME, "tail", EFUN_DESCRIPTOR);
		mv.visitVarInsn(ALOAD, 1);

		mv.visitMethodInsn(INVOKEVIRTUAL, EFUN_NAME, "go", GO_DESC);
		mv.visitVarInsn(ASTORE, arity + 2);

		mv.visitJumpInsn(GOTO, loop);

		mv.visitLabel(done);
		mv.visitVarInsn(ALOAD, arity + 2);
		mv.visitInsn(ARETURN);
		mv.visitMaxs(arity + 2, arity + 2);
		mv.visitEnd();
	}

	public static void make_invoketail_method(ClassWriter cw,
			String full_inner, int arity, int freevars) {
		MethodVisitor mv;
		mv = cw.visitMethod(ACC_PUBLIC, "invoke_tail", EUtil.getSignature(arity
				- freevars, true), null, null);
		mv.visitCode();
		for (int i = 0; i < arity - freevars; i++) {
			mv.visitVarInsn(ALOAD, 1);
			mv.visitVarInsn(ALOAD, i + 2);
			mv.visitFieldInsn(PUTFIELD, EPROC_NAME, "arg" + i, EOBJECT_DESC);
		}

		mv.visitVarInsn(ALOAD, 1);
		mv.visitVarInsn(ALOAD, 0);
		mv.visitFieldInsn(PUTFIELD, EPROC_NAME, "tail", EFUN_DESCRIPTOR);
		mv.visitFieldInsn(GETSTATIC, EPROC_NAME, "TAIL_MARKER", EOBJECT_DESC);
		mv.visitInsn(ARETURN);
		mv.visitMaxs(arity + 2, arity + 2);
		mv.visitEnd();
	}

	private static void make_go_method(ClassWriter cw, String outer_name,
			String mname, String full_inner, int arity, boolean proc,
			int freevars, Type returnType) {
		MethodVisitor mv;
		mv = cw.visitMethod(ACC_PUBLIC, "go", GO_DESC, null, PAUSABLE_EX);
		mv.visitCode();
		for (int i = 0; i < arity - freevars; i++) {
			mv.visitVarInsn(ALOAD, 1);
			mv.visitFieldInsn(GETFIELD, EPROC_NAME, "arg" + i, EOBJECT_DESC);
			mv.visitVarInsn(ASTORE, i + 2);
		}
		for (int i = 0; i < arity - freevars; i++) {
			mv.visitVarInsn(ALOAD, 1);
			mv.visitInsn(ACONST_NULL);
			mv.visitFieldInsn(PUTFIELD, EPROC_NAME, "arg" + i, EOBJECT_DESC);
		}
		if (proc)
			mv.visitVarInsn(ALOAD, 1);

		for (int i = 0; i < arity - freevars; i++) {
			mv.visitVarInsn(ALOAD, i + 2);
		}

		for (int i = 0; i < freevars; i++) {
			mv.visitVarInsn(ALOAD, 0);
			mv.visitFieldInsn(GETFIELD, full_inner, "fv" + i, EOBJECT_DESC);
		}

		mv.visitMethodInsn(INVOKESTATIC, outer_name, mname, EUtil.getSignature(
				arity, proc, returnType));
		mv.visitInsn(ARETURN);
		mv.visitMaxs(arity + 2, arity + 2);
		mv.visitEnd();

		cw.visitEnd();
	}

}

/** Active exception handler */
class EXHandler {
	int handler_beam_label;
	Label begin, end, target;
	BeamExceptionHandler beam_exh;
}
