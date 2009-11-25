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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.TreeMap;

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
import erjang.ESmall;
import erjang.EList;
import erjang.EModule;
import erjang.ENil;
import erjang.ENumber;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.EString;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ErlFun;
import erjang.ErlangException;
import erjang.Module;
import erjang.beam.Arg.Kind;

/**
 * 
 */
public class CompilerVisitor implements ModuleVisitor, Opcodes {

	ECons atts = EList.NIL;
	private Set<String> exported = new HashSet<String>();

	private final ClassVisitor cv;
	private EAtom module_name;
	private Type self_type;

	private static final EObject ATOM_field_flags = EAtom.intern("field_flags");

	static final Type EBINMATCHSTATE_TYPE = Type.getType(EBinMatchState.class);
	static final Type EBINSTRINGBUILDER_TYPE = Type
			.getType(EBitStringBuilder.class);
	static final Type ERLANG_EXCEPTION_TYPE = Type
			.getType(ErlangException.class);
	static final Type ERT_TYPE = Type.getType(ERT.class);
	static final Type EINTEGER_TYPE = Type.getType(ESmall.class);
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

	static final String ETUPLE_DESC = ETUPLE_TYPE.getDescriptor();
	static final String EATOM_DESC = EATOM_TYPE.getDescriptor();

	/**
	 * 
	 */
	static final String EFUN_DESCRIPTOR = EFUN_TYPE.getDescriptor();
	static final Type EPID_TYPE = Type.getType(EPID.class);
	static final Type EPORT_TYPE = Type.getType(EPort.class);
	static final Type EMATCHSTATE_TYPE = Type.getType(EBinMatchState.class);

	static final EObject TRUE_ATOM = EAtom.intern("true");
	static final EObject FALSE_ATOM = EAtom.intern("false");
	static final EObject X_ATOM = EAtom.intern("x");
	static final EObject Y_ATOM = EAtom.intern("y");
	static final EObject FR_ATOM = EAtom.intern("fr");
	static final EObject NIL_ATOM = EAtom.intern("nil");
	static final EObject INTEGER_ATOM = EAtom.intern("integer");
	static final EObject FLOAT_ATOM = EAtom.intern("float");
	static final EObject ATOM_ATOM = EAtom.intern("atom");
	static final EObject LITERAL_ATOM = EAtom.intern("literal");
	static final EObject NOFAIL_ATOM = EAtom.intern("nofail");
	static final EObject F_ATOM = EAtom.intern("f");
	static final EObject FIELD_FLAGS_ATOM = EAtom.intern("field_flags");
	static final EObject ERLANG_ATOM = EAtom.intern("erlang");

	static final Type MODULE_ANN_TYPE = Type.getType(Module.class);
	static final Type ERLFUN_ANN_TYPE = Type.getType(ErlFun.class);
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

		this.self_type = Type.getObjectType(getInternalClassName());

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
					"L" + clazz + ";");

		}

		for (Map.Entry<EObject, String> ent : constants.entrySet()) {

			EObject term = ent.getKey();
			term.emit_const(mv);
			mv.visitFieldInsn(Opcodes.PUTSTATIC, self_type.getInternalName(),
					ent.getValue(), Type.getType(term.getClass())
							.getDescriptor());
		}

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

	class ASMFunctionAdapter implements FunctionVisitor2 {

		Stack<EXHandler> ex_handlers = new Stack<EXHandler>();

		private final EAtom fun_name;
		private final int arity;
		private final int startLabel;

		Map<Integer, Label> labels = new TreeMap<Integer, Label>();
		private boolean isTailRecursive;
		private MethodVisitor mv;
		private int[] xregs;
		private int[] yregs;
		private int[] fpregs;
		private Label start;
		private Label end;
		private int scratch_reg;

		private int bit_string_builder;

		Label getLabel(int i) {
			if (i <= 0)
				throw new Error();
			Label l = labels.get(i);
			if (l == null) {
				labels.put(i, l = new Label());
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
					null, null);

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
			mv.visitLabel(end);
			mv.visitMaxs(20, scratch_reg + 3);
			mv.visitEnd();

			String mname = EUtil.getJavaName(fun_name, arity);
			String outer_name = self_type.getInternalName();
			String inner_name = mname;
			String full_inner_name = outer_name + "$" + inner_name;

			int freevars = get_lambda(fun_name, arity);
			if (freevars == -1) {

				generate_invoke_call_self();
				generate_tail_call_self();

				freevars = 0;

				FieldVisitor fv = cv.visitField(ACC_STATIC | ACC_FINAL,
						inner_name, "L" + full_inner_name + ";", null, null);
				fv.visitEnd();

				funs.put(inner_name, full_inner_name);

			}

			cv.visitInnerClass(full_inner_name, outer_name, inner_name,
					ACC_STATIC);

			byte[] data = CompilerVisitor.make_invoker(self_type, mname, arity,
					true, freevars);

			try {
				classRepo.store(full_inner_name, data);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		/**
		 * 
		 */
		private void generate_invoke_call_self() {

			String javaName = EUtil.getJavaName(fun_name, arity);
			String signature = EUtil.getSignature(arity, true);
			mv = cv.visitMethod(ACC_STATIC, javaName + "$call", signature,
					null, null);
			mv.visitCode();

			if (isTailRecursive) {

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
				mv
						.visitFieldInsn(GETFIELD, EPROC_NAME, "tail",
								EFUN_DESCRIPTOR);
				mv.visitVarInsn(ALOAD, 0);

				mv.visitMethodInsn(INVOKEVIRTUAL, EFUN_NAME, "go", GO_DESC);
				mv.visitVarInsn(ASTORE, arity + 1);

				mv.visitJumpInsn(GOTO, loop);

				mv.visitLabel(done);
				mv.visitVarInsn(ALOAD, arity + 1);

			} else {

				mv.visitVarInsn(ALOAD, 0);
				for (int i = 0; i < arity; i++) {
					mv.visitVarInsn(ALOAD, i + 1);
				}
				mv.visitMethodInsn(INVOKESTATIC, self_type.getInternalName(),
						javaName, EUtil.getSignature(arity, true));

			}

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

			if (isTailRecursive) {

				for (int i = 0; i < arity; i++) {
					mv.visitVarInsn(ALOAD, 0);
					mv.visitVarInsn(ALOAD, i + 1);
					mv.visitFieldInsn(PUTFIELD, EPROC_NAME, "arg" + i,
							EOBJECT_DESC);
				}

				mv.visitVarInsn(ALOAD, 0);
				mv.visitFieldInsn(GETSTATIC, self_type.getInternalName(),
						javaName, "L" + EFUN_NAME + arity + ";");
				mv
						.visitFieldInsn(PUTFIELD, EPROC_NAME, "tail",
								EFUN_DESCRIPTOR);
				mv.visitFieldInsn(GETSTATIC, EPROC_NAME, "TAIL_MARKER",
						EOBJECT_DESC);

			} else {
				for (int i = 0; i < arity + 1; i++) {
					mv.visitVarInsn(ALOAD, i);
				}

				mv.visitMethodInsn(INVOKESTATIC, self_type.getInternalName(),
						javaName, signature);
			}

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
			this.scratch_reg = local;

		}

		private void add_erlfun_annotation(MethodVisitor mv) {
			AnnotationVisitor ann = mv.visitAnnotation(ERLFUN_ANN_TYPE
					.getDescriptor(), true);
			ann.visit("module", module_name.getName());
			ann.visit("name", fun_name.getName());
			ann.visit("arity", String.valueOf(this.arity));
			ann.visit("export", String.valueOf(isExported(fun_name, arity)));
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
			return new ASMBlockVisitor();
		}

		class ASMBlockVisitor implements BlockVisitor2 {

			/* (non-Javadoc)
			 * @see erjang.beam.BlockVisitor2#visitBSAdd(erjang.beam.Arg[], erjang.beam.Arg)
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
			public void visitBS(BeamOpcode opcode, Arg arg) {
				switch (opcode) {
				case bs_save2:
					push(arg, EBINMATCHSTATE_TYPE);
					mv.visitVarInsn(ASTORE, bit_string_builder + 1);
					return;

				case bs_restore2:
					mv.visitVarInsn(ALOAD, bit_string_builder + 1);
					pop(arg, EBINMATCHSTATE_TYPE);
					return;

				case bs_context_to_binary:
					push(arg, EBINMATCHSTATE_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), "binary", "()"
							+ EBITSTRING_TYPE.getDescriptor());
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
			public void visitInitBitString(Arg size, Arg flags, Arg out) {

				ETuple tflags = flags.value.testTuple();
				if (tflags.elm(1) != ATOM_field_flags)
					throw new Error("unhandled flags: " + flags.value);
				int iflags = tflags.elm(2).asInt();

				push(size, Type.INT_TYPE);
				mv.visitLdcInsn(new Integer(iflags));
				mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "bs_init", "(II)"
						+ EBITSTRINGBUILDER_TYPE.getDescriptor());

				mv.visitInsn(DUP);
				mv.visitVarInsn(ASTORE, bit_string_builder);

				mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
						.getInternalName(), "bitstring", "()"
						+ EBITSTRING_TYPE.getDescriptor());

				pop(out, EBITSTRING_TYPE);

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
					Arg flags) {

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
					push(size, Type.INT_TYPE);
					push(flags, Type.INT_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
							.getInternalName(), "put_integer", "("
							+ EINTEGER_TYPE.getDescriptor() + "II)V");
					return;

				case bs_put_float:
					mv.visitVarInsn(ALOAD, bit_string_builder);
					push(arg, Type.DOUBLE_TYPE);
					push(size, Type.INT_TYPE);
					push(flags, Type.INT_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
							.getInternalName(), "put_integer", "(DII)V");
					return;

				case bs_put_binary:
					mv.visitVarInsn(ALOAD, bit_string_builder);
					push(arg, EBITSTRING_TYPE);
					push(size, EATOM_TYPE);
					push(flags, Type.INT_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
							.getInternalName(), "put_bitstring", "("
							+ EBITSTRING_TYPE.getDescriptor() 
							+ EATOM_TYPE.getDescriptor() 
							+ "I)V");
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
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, test.name(), "("
							+ EOBJECT_DESC + ")"
							+ EMATCHSTATE_TYPE.getDescriptor());

					mv.visitInsn(DUP);
					mv.visitVarInsn(ASTORE, scratch_reg);

					mv.visitJumpInsn(IFNULL, getLabel(failLabel));

					mv.visitVarInsn(ALOAD, scratch_reg);

					pop(out, EBINMATCHSTATE_TYPE);
					return;
				}

					// {test,bs_test_unit,{f,41},[{x,2},8]}
				case bs_test_unit:
					push(args[0], EBINMATCHSTATE_TYPE);
					push(args[1], Type.INT_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, ERT_NAME, test.name(),
							"(I)" + EOBJECT_DESC);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
					return;

				case bs_match_string:
					push(args[0], EBINMATCHSTATE_TYPE);
					push(args[1], Type.INT_TYPE);
					push(args[2], EBITSTRING_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, ERT_NAME, test.name(),
							"(I" + EBITSTRING_TYPE.getDescriptor() + ")"
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
					mv.visitMethodInsn(INVOKEVIRTUAL, ERT_NAME, test.name(),
							"(" + EINTEGER_TYPE.getDescriptor() + "II)"
									+ EOBJECT_DESC);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
					return;

					// {test,bs_get_binary2,{f,348},[{x,3},5,{atom,all},8,{field_flags,0},{x,3}]}
				case bs_get_binary2:

					push(args[0], EBINMATCHSTATE_TYPE);
					push(args[2], EOBJECT_TYPE);
					push_immediate(args[4].value.testTuple().elm(2),
							Type.INT_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, ERT_NAME, test.name(),
							"(" + EOBJECT_DESC + "I)" + EBITSTRING_TYPE);

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
					mv.visitMethodInsn(INVOKEVIRTUAL, ERT_NAME, test.name(),
							"(II)" + EOBJECT_DESC);
					mv.visitInsn(DUP);
					mv.visitVarInsn(ASTORE, scratch_reg);

					mv.visitJumpInsn(IFNULL, getLabel(failLabel));

					mv.visitVarInsn(ALOAD, scratch_reg);

					pop(args[5], EOBJECT_TYPE);
					return;

				case bs_test_tail2:
					push(args[0], EBINMATCHSTATE_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, ERT_NAME, test.name(),
							"()" + EOBJECT_DESC);
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
							+ EATOM_DESC + EATOM_DESC + EOBJECT_DESC + ")"
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

				test_ex_start();

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
					if (fromType.equals(EINTEGER_TYPE)) {
						mv
								.visitFieldInsn(GETFIELD, EINTEGER_NAME,
										"value", "I");
						return;
					}
				}

				if (toType.equals(Type.DOUBLE_TYPE)) {
					if (fromType.equals(EDOUBLE_TYPE)) {
						mv.visitFieldInsn(GETFIELD, EDOUBLE_NAME, "value", "D");
						return;
					}
				}

				mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "unbox"
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
					return "Integer";
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
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "box", "("
							+ fromType.getDescriptor() + ")"
							+ toType.getDescriptor());
				} else if (fromType.equals(Type.DOUBLE_TYPE)
						&& toType.getSort() == Type.OBJECT) {
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "box", "("
							+ fromType.getDescriptor() + ")"
							+ toType.getDescriptor());
				} else if (fromType.equals(Type.BOOLEAN_TYPE)
						&& toType.getSort() == Type.OBJECT) {
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "box", "("
							+ fromType.getDescriptor() + ")"
							+ toType.getDescriptor());
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

			/**
			 * @param arg
			 * @param class1
			 */
			private void push(Arg value, Type stack_type) {
				Type t = value.type;
				if (value.kind == Kind.X || value.kind == Kind.Y) {
					if (value.type == Type.INT_TYPE
							|| value.type == Type.BOOLEAN_TYPE) {
						//throw new Error("should not happen");
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
			 *            TODO
			 */
			private Type push_immediate(EObject value, Type stack_type) {

				if (value == ENil.NIL) {
					mv.visitFieldInsn(GETSTATIC, ENIL_NAME, "NIL", ENIL_TYPE
							.getDescriptor());
					return ENIL_TYPE;
				}

				if (value == ERT.TRUE) {
					mv.visitFieldInsn(GETSTATIC, ERT_NAME, "ATOM_TRUE",
							EATOM_DESC);
					return EATOM_TYPE;
				}

				if (value == ERT.FALSE) {
					mv.visitFieldInsn(GETSTATIC, ERT_NAME, "ATOM_FALSE",
							EATOM_DESC);
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
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "timeout", EUtil
							.getSignature(0, false));
					mv.visitInsn(ARETURN);
					return;

				case remove_message:
					mv.visitVarInsn(ALOAD, 0);
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME,
							"remove_message", "(" + EPROC_TYPE.getDescriptor()
									+ ")V");
					return;

				case loop_rec_end:
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "loop_rec_end",
							EUtil.getSignature(0, false));
					mv.visitInsn(ARETURN);
					return;

				}

				throw new Error();
			}

			void test_ex_start() {
				if (ex_handlers.size() > 0) {
					EXHandler ex = ex_handlers.peek();
					if (!ex.is_start_visited) {
						mv.visitLabel(ex.begin);
						ex.is_start_visited = true;
					}
				}
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

				case K_try:
				case K_catch: {
					EXHandler h = new EXHandler();
					h.begin = new Label();
					h.target = new Label();
					h.end = new Label();
					h.reg = out;

					mv.visitTryCatchBlock(h.begin, h.end, h.target, Type
							.getType(ErlangException.class).getInternalName());

					ex_handlers.push(h);
					return;
				}

				case try_end:
					test_ex_start();
					EXHandler ex = ex_handlers.peek();
					mv.visitLabel(ex.end);
					return;

				case try_case: {

					test_ex_start();

					EXHandler h = ex_handlers.pop();

					mv.visitLabel(h.target);

					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME,
							"decode_exception", "("
									+ ERLANG_EXCEPTION_TYPE.getDescriptor()
									+ ")" + getTubleType(3).getDescriptor());

					mv.visitInsn(DUP);
					mv.visitFieldInsn(GETFIELD, ETUPLE_NAME + 3, "elm0",
							EOBJECT_DESC);
					mv.visitVarInsn(ASTORE, xregs[0]);

					mv.visitInsn(DUP);
					mv.visitFieldInsn(GETFIELD, ETUPLE_NAME + 3, "elm1",
							EOBJECT_DESC);
					mv.visitVarInsn(ASTORE, xregs[1]);

					mv.visitFieldInsn(GETFIELD, ETUPLE_NAME + 3, "elm2",
							EOBJECT_DESC);
					mv.visitVarInsn(ASTORE, xregs[2]);

					return;
				}

				case catch_end: {

					test_ex_start();

					EXHandler h = ex_handlers.pop();

					Label after = new Label();
					mv.visitJumpInsn(GOTO, after);

					mv.visitLabel(h.end);
					mv.visitLabel(h.target);

					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME,
							"decode_exception", "("
									+ ERLANG_EXCEPTION_TYPE.getDescriptor()
									+ ")" + getTubleType(3).getDescriptor());

					mv.visitVarInsn(ASTORE, xregs[0]);

					mv.visitLabel(after);

					return;
				}

				case wait_timeout: {
					mv.visitVarInsn(ALOAD, 0);
					push(out, EOBJECT_TYPE);
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "wait_timeout",
							"(" + EPROC_TYPE.getDescriptor()
									+ EINTEGER_TYPE.getDescriptor() + ")Z");

					mv.visitJumpInsn(IFNE, getLabel(val));
					return;
				}

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
							"elm" + pos, EOBJECT_DESC);
					return;

				} else if (opcode == BeamOpcode.get_tuple_element) {
					push(val, val.type);
					mv.visitFieldInsn(GETFIELD, val.type.getInternalName(),
							"elm" + pos, EOBJECT_DESC);
					pop(out, EOBJECT_TYPE);
					return;
				} else if (opcode == BeamOpcode.set_tuple_element) {
					push(out, out.type);
					push(val, val.type);
					mv.visitFieldInsn(PUTFIELD, val.type.getInternalName(),
							"elm" + pos, EOBJECT_DESC);
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
					mv.visitLdcInsn(new Integer(arg.no));
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "func_info", "("
							+ ETUPLE_DESC + ETUPLE_DESC + "I)" + EOBJECT_DESC);
					mv.visitInsn(ARETURN);
					return;

				case init:
					push_immediate(ENil.NIL, ENIL_TYPE);
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
				case test_arity:

					push(arg, ETUPLE_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, ETUPLE_NAME, "count",
							"()I");
					push_int(arity);
					mv.visitJumpInsn(IF_ICMPNE, getLabel(failLabel));
					push(arg, ETUPLE_TYPE);
					mv.visitTypeInsn(CHECKCAST, tupleType.getInternalName());
					pop(arg, tupleType);
					return;

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

					mv.visitFieldInsn(GETSTATIC, ENIL_NAME, "NIL", ENIL_TYPE
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

				} else if (opcode == BeamOpcode.apply) {

					mv.visitVarInsn(ALOAD, 0);
					for (int i = 0; i < ys.length; i++) {
						push(ys[i], EOBJECT_TYPE);
					}
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "apply", EUtil
							.getSignature(ys.length, true));
					mv.visitVarInsn(ASTORE, xregs[0]);

					return;

				} else if (opcode == BeamOpcode.apply_last) {
					mv.visitVarInsn(ALOAD, 0);
					for (int i = 0; i < ys.length; i++) {
						push(ys[i], EOBJECT_TYPE);
					}
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "apply$last",
							EUtil.getSignature(ys.length, true));

					mv.visitInsn(ARETURN);

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
				int[] hash = new int[values.length];
				Label[] label = new Label[values.length];
				for (int i = 0; i < values.length; i++) {
					hash[i] = values[i].value.hashCode();
					label[i] = new Label();
				}

				push(in, EOBJECT_TYPE);
				mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object",
						"hashCode", "()I");
				mv.visitLookupSwitchInsn(getLabel(failLabel), hash, label);

				for (int i = 0; i < hash.length; i++) {
					int h = hash[i];
					for (int j = i; j < hash.length; j++) {
						if (h == hash[j] && label[j] != null)
							mv.visitLabel(label[j]);
					}

					for (int j = i; j < hash.length; j++) {
						if (h == hash[j] && label[j] != null) {

							if (values[j].type.equals(EATOM_TYPE)) {

								push(in, in.type);
								push(values[j], values[j].type);
								mv.visitJumpInsn(IF_ACMPEQ,
										getLabel(targets[j]));

							} else {

								if (in.type == values[j].type) {
									push(in, in.type);
									push(values[j], values[j].type);

									mv.visitMethodInsn(INVOKESTATIC, ERT_NAME,
											"eq", "(" + in.type.getDescriptor()
													+ in.type.getDescriptor()
													+ ")Z");
								} else {
									push(in, EOBJECT_TYPE);
									push(values[j], EOBJECT_TYPE);
									mv.visitMethodInsn(INVOKEVIRTUAL,
											"java/lang/Object", "equals",
											"(Ljava/lang/Object;)Z");
								}
								mv.visitJumpInsn(IFNE, getLabel(targets[j]));
							}
							label[j] = null;
						}
					}

					mv.visitJumpInsn(GOTO, getLabel(failLabel));
				}
			}

			@Override
			public void visitSelectTuple(Arg in, int failLabel, int[] arities,
					int[] targets) {

				push(in, ETUPLE_TYPE);

				mv.visitMethodInsn(INVOKEVIRTUAL, ETUPLE_NAME, "count", "()I");

				Label[] targetLabels = new Label[targets.length];
				for (int i = 0; i < targets.length; i++) {
					targetLabels[i] = new Label();
				}
				mv.visitLookupSwitchInsn(getLabel(failLabel), arities,
						targetLabels);
				for (int i = 0; i < targetLabels.length; i++) {
					mv.visitLabel(targetLabels[i]);
					push(in, ETUPLE_TYPE);
					mv.visitTypeInsn(CHECKCAST, getTubleType(arities[i])
							.getInternalName());
					pop(in, getTubleType(arities[i]));
					mv.visitJumpInsn(GOTO, getLabel(targets[i]));
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

				test_ex_start();

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
					String field = CompilerVisitor.this
							.getExternalFunction(fun);
					String funTypeName = EFUN_NAME + args.length;
					mv.visitFieldInsn(GETSTATIC, self_type.getInternalName(),
							field, "L" + funTypeName + ";");
					mv.visitVarInsn(ALOAD, 0);
					for (int i = 0; i < args.length; i++) {
						push(args[i], EOBJECT_TYPE);
					}

					mv.visitMethodInsn(INVOKEVIRTUAL, funTypeName,
							is_tail ? "invoke_tail" : "invoke", EUtil
									.getSignature(args.length, true));

					if (is_tail || isExitFunc(fun)) {
						mv.visitInsn(ARETURN);
					} else {
						mv.visitVarInsn(ASTORE, xregs[0]);
					}

				} else {

					mv.visitVarInsn(ALOAD, 0);
					for (int i = 0; i < args.length; i++) {
						push(args[i], EOBJECT_TYPE);
					}

					mv.visitMethodInsn(INVOKESTATIC, self_type
							.getInternalName(), EUtil.getJavaName(fun.fun,
							fun.no)
							+ (is_tail ? "$tail" : ""), EUtil.getSignature(
							args.length, true));

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
	static Method IS_PID_TEST = Method.getMethod("erjang.EPID testPID()");
	static Method IS_PORT_TEST = Method.getMethod("erjang.EPort testPort()");
	static Method IS_REFERENCE_TEST = Method
			.getMethod("erjang.EReference testReference()");
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

	static public byte[] make_invoker(Type self_type, String mname, int arity,
			boolean proc, int freevars) {

		String outer_name = self_type.getInternalName();
		String inner_name = mname;
		String full_inner_name = outer_name + "$" + inner_name;

		ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
		String super_class_name = EFUN_NAME + (arity - freevars);
		cw
				.visit(V1_6, ACC_FINAL, full_inner_name, null,
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
		mv.visitEnd();

		make_invoke_method(cw, outer_name, mname, arity, proc, freevars);
		make_invoketail_method(cw, full_inner_name, arity, freevars);
		make_go_method(cw, outer_name, mname, full_inner_name, arity, proc,
				freevars);

		return cw.toByteArray();

	}

	private static void make_invoke_method(ClassWriter cw, String outer_name,
			String mname, int arity, boolean proc, int freevars) {
		MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "invoke", EUtil
				.getSignature(arity - freevars, true), null, null);
		mv.visitCode();
		if (proc) {
			mv.visitVarInsn(ALOAD, 1);
		}
		for (int i = 0; i < arity - freevars; i++) {
			mv.visitVarInsn(ALOAD, i + 2);
		}
		for (int i = 0; i < freevars; i++) {
			mv.visitVarInsn(ALOAD, 0);
			mv.visitFieldInsn(GETFIELD, outer_name + "$" + mname, "fv" + i,
					EOBJECT_DESC);
		}
		mv.visitMethodInsn(INVOKESTATIC, outer_name, mname, EUtil.getSignature(
				arity, proc));
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
		mv.visitEnd();
	}

	private static void make_invoketail_method(ClassWriter cw,
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
		mv.visitEnd();
	}

	private static void make_go_method(ClassWriter cw, String outer_name,
			String mname, String full_inner, int arity, boolean proc,
			int freevars) {
		MethodVisitor mv;
		mv = cw.visitMethod(ACC_PUBLIC, "go", GO_DESC, null, null);
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
				arity, proc));
		mv.visitInsn(ARETURN);
		mv.visitEnd();

		cw.visitEnd();
	}

}

class EXHandler {
	public boolean is_start_visited;
	Arg reg;
	Label begin, end, target;
}
