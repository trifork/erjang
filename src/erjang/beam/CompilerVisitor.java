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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.Method;

import erjang.EAtom;
import erjang.EBinMatchState;
import erjang.EBinary;
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
import erjang.ERT;
import erjang.ESeq;
import erjang.ETerm;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ErlFun;
import erjang.Module;
import erjang.beam.Arg.Kind;
import erjang.modules.ErlangModule;

/**
 * 
 */
public class CompilerVisitor implements ModuleVisitor, Opcodes {

	ECons atts = EList.EMPTY;
	private Set<String> exported = new HashSet<String>();

	private final ClassVisitor cv;
	private EAtom module_name;
	private Type self_type;

	static final Type ERT_TYPE = Type.getType(ERT.class);
	static final Type EINTEGER_TYPE = Type.getType(EInteger.class);
	static final Type EMODULE_TYPE = Type.getType(EModule.class);
	static final Type ENUMBER_TYPE = Type.getType(ENumber.class);
	static final Type EOBJECT_TYPE = Type.getType(EObject.class);
	static final Type EDOUBLE_TYPE = Type.getType(EDouble.class);
	static final Type ENIL_TYPE = Type.getType(ENil.class);
	static final Type EATOM_TYPE = Type.getType(EAtom.class);
	static final Type ETUPLE_TYPE = Type.getType(ETuple.class);
	static final Type EBINARY_TYPE = Type.getType(EBinary.class);
	static final Type ECONS_TYPE = Type.getType(ECons.class);
	static final Type ESEQ_TYPE = Type.getType(ESeq.class);
	static final Type ELIST_TYPE = Type.getType(EList.class);
	static final Type EFUN_TYPE = Type.getType(EFun.class);
	static final Type EPID_TYPE = Type.getType(EPID.class);
	static final Type EPORT_TYPE = Type.getType(EPort.class);
	static final Type EMATCHSTATE_TYPE = Type.getType(EBinMatchState.class);

	static final ETerm TRUE_ATOM = EAtom.intern("true");
	static final ETerm FALSE_ATOM = EAtom.intern("false");
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
	static final ETerm FIELD_FLAGS_ATOM = EAtom.intern("field_flags");

	private static final Type MODULE_ANN_TYPE = Type.getType(Module.class);
	private static final Type ERLFUN_ANN_TYPE = Type.getType(ErlFun.class);

	/**
	 * 
	 */
	public CompilerVisitor(ClassVisitor cv) {
		this.cv = cv;
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

		cv.visit(V1_5, ACC_PUBLIC, self_type.getInternalName(), null,
				EMODULE_TYPE.getInternalName(), null);

		add_module_annotation(cv);

	}

	private void add_module_annotation(ClassVisitor cv) {

		AnnotationVisitor av = cv.visitAnnotation(MODULE_ANN_TYPE
				.getInternalName(), true);
		av.visitAnnotation("value", getModuleName());

		av.visitEnd();
	}

	private String getInternalClassName() {
		String cn = EUtil.toJavaIdentifier(getModuleName());

		String internalEMname = ErlangModule.class.getName().replace('.', '/');
		int idx = internalEMname.lastIndexOf('/');
		String prefix = internalEMname.substring(0, idx + 1);
		return prefix + cn;
	}

	/**
	 * @return
	 */
	private String getModuleName() {
		return module_name.getName();
	}

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
		cv.visitEnd();
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

	class ASMFunctionAdapter implements FunctionVisitor2 {

		private final EAtom fun_name;
		private final int arity;
		private final int startLabel;

		Map<Integer, Label> labels = new TreeMap<Integer, Label>();
		private boolean isTailRecursive;
		private MethodVisitor mv;
		private int[] xregs;
		private int[] yregs;
		private int[] fpregs;

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
			String signature = EUtil.getSignature(arity);
			mv = cv.visitMethod(ACC_STATIC | ACC_PUBLIC, javaName, signature,
					null, null);

			add_erlfun_annotation(mv);

			allocate_regs_to_locals(x_count, y_count, fp_count);

			mv.visitCode();
			mv.visitJumpInsn(GOTO, getLabel(startLabel));
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see erjang.beam.FunctionVisitor#visitEnd()
		 */
		@Override
		public void visitEnd() {
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

			int local = 0;

			xregs = new int[max_x];
			for (int i = 0; i < max_x; i++) {
				xregs[i] = local;
				local += 1;
			}

			yregs = new int[max_y];
			for (int i = 0; i < max_y; i++) {
				yregs[i] = local;
				local += 1;
			}

			fpregs = new int[max_f];
			for (int i = 0; i < max_f; i++) {
				fpregs[i] = local;
				local += 2;
			}

		}

		private void add_erlfun_annotation(MethodVisitor mv) {
			AnnotationVisitor ann = mv.visitAnnotation(ERLFUN_ANN_TYPE
					.getDescriptor(), true);
			String mod = module_name.getName();
			ann.visitAnnotation("name", fun_name.getName());
			ann.visitAnnotation("arity", String.valueOf(this.arity));
			ann.visitAnnotation("export", String.valueOf(isExported(fun_name,
					arity)));
			ann.visitEnd();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see erjang.beam.FunctionVisitor#visitLabeledBlock(int)
		 */
		@Override
		public BlockVisitor visitLabeledBlock(int label) {
			mv.visitLabel(getLabel(label));

			return new ASMBlockVisitor();
		}

		class ASMBlockVisitor implements BlockVisitor2 {

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInsn(erjang.beam.BeamOpcode,
			 * int, erjang.beam.Arg[], erjang.beam.Arg,
			 * java.lang.reflect.Method)
			 */
			@Override
			public void visitInsn(BeamOpcode opcode, int failLabel, Arg[] in,
					Arg out, BIF bif) {

				switch (opcode) {
				case gc_bif:
				case bif:
				case arithfbif:
					Type[] parameterTypes = bif.getArgumentTypes();
					assert (in.length == parameterTypes.length);
					push(in, parameterTypes);

					mv.visitMethodInsn(INVOKESTATIC, bif.owner
							.getInternalName(), bif.getName(), bif
							.getDescriptor());

					if (failLabel != 0) {
						// guard
						if (bif.getReturnType().getSort() != Type.OBJECT)
							throw new Error("guards must return object type");

						// dup result for test
						if (out != null) {

							pop(out, bif.getReturnType());
							push(out, bif.getReturnType());
						}
						mv.visitJumpInsn(IFNULL, getLabel(failLabel));
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

				if (out.type != null && !out.type.equals(stack_type)) {
					emit_convert(stack_type, out.type);
				}

				if (out.kind == Kind.X || out.kind == Kind.Y) {
					mv.visitVarInsn(ASTORE, var_index(out));
				} else if (out.kind == Kind.F) {
					mv.visitVarInsn(FSTORE, var_index(out));
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
						mv.visitFieldInsn(GETFIELD, EINTEGER_TYPE
								.getInternalName(), "value", "I");
					}
					return;
				}

				if (toType.equals(Type.DOUBLE_TYPE)) {
					if (fromType.equals(EDOUBLE_TYPE)) {
						mv.visitFieldInsn(GETFIELD, EDOUBLE_TYPE
								.getInternalName(), "value", "D");
					}
					return;
				}

				throw new Error();
			}

			/**
			 * @param fromType
			 * @param toType
			 */
			private void emit_box(Type fromType, Type toType) {
				if (fromType.equals(Type.INT_TYPE)
						&& toType.getSort() == Type.OBJECT) {
					mv.visitTypeInsn(NEW, EINTEGER_TYPE.getInternalName());
					mv.visitInsn(DUP);
					mv.visitMethodInsn(INVOKESPECIAL, EINTEGER_TYPE
							.getInternalName(), "<init>", "(I)V");
				} else if (fromType.equals(Type.DOUBLE_TYPE)
						&& toType.getSort() == Type.OBJECT) {
					mv.visitTypeInsn(NEW, EDOUBLE_TYPE.getInternalName());
					mv.visitInsn(DUP);
					mv.visitMethodInsn(INVOKESPECIAL, EDOUBLE_TYPE
							.getInternalName(), "<init>", "(D)V");
				} else if (fromType.equals(Type.BOOLEAN_TYPE)
						&& toType.getSort() == Type.OBJECT) {
					Label true_case = new Label();
					Label end_case = new Label();

					mv.visitJumpInsn(IFEQ, true_case);
					mv.visitFieldInsn(GETSTATIC, ERT_TYPE.getInternalName(),
							"FALSE", EATOM_TYPE.getInternalName());
					mv.visitJumpInsn(GOTO, end_case);
					mv.visitLabel(true_case);
					mv.visitFieldInsn(GETSTATIC, ERT_TYPE.getInternalName(),
							"TRUE", EATOM_TYPE.getInternalName());
					mv.visitLabel(end_case);

				} else {
					throw new Error("cannot box " + fromType + " -> " + toType);
				}

			}

			/**
			 * @param in
			 * @param parameterTypes
			 */
			private void push(Arg[] in, Type[] parameterTypes) {
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
					mv.visitVarInsn(ALOAD, var_index(value));
				} else if (value.kind == Kind.F) {
					mv.visitVarInsn(FLOAD, var_index(value));
				} else if (value.kind == Kind.IMMEDIATE) {
					t = push_immediate((ETerm) value.value, stack_type);
				} else {
					throw new Error();
				}

				if (t != null && !t.equals(stack_type)) {
					emit_convert(t, stack_type);
				}
			}

			Map<ETerm,String> constants = new HashMap<ETerm,String>(); 
			
			/**
			 * @param value
			 * @param stack_type
			 *            TODO
			 */
			private Type push_immediate(ETerm value, Type stack_type) {

				if (stack_type.getSort() != Type.OBJECT) {
					if (value instanceof EInteger) {
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
					throw new Error();
				}

				String known = constants.get(value);
				
				if (known == null) {
					constants.put(value, known = "cp$" + constants.size());
				}
				
				if (known != null) {
					Type type = Type.getType(value.getClass());
					mv.visitFieldInsn(GETSTATIC, self_type.getInternalName(), known, type.getDescriptor());
					return type;
				}
				
				throw new Error("cannot push "+value + " as "+stack_type);

				// return Type.getType(value.getClass());
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInsn(erjang.beam.BeamOpcode)
			 */
			@Override
			public void visitInsn(BeamOpcode insn) {
				switch (insn) {
				case K_return:
					mv.visitInsn(ARETURN);
					return;

				}

				throw new Error();
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
				throw new Error();
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

				push(arg1, test_bif.getArgumentTypes()[0]);

				mv.visitMethodInsn(INVOKEVIRTUAL, EOBJECT_TYPE
						.getInternalName(), test_bif.getName(), test_bif
						.getDescriptor());

				Type returnType = test_bif.getReturnType();
				if (failLabel != 0) {
					// guard
					if (returnType.getSort() != Type.OBJECT)
						throw new Error("guards must return object type");

					// dup result for test
					if (arg1 != null) {
						pop(arg1, returnType);
						push(new Arg(arg1, returnType), returnType);
					}
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
				} else {
					pop(arg1, returnType);
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

				BIF bif = BIFUtil.getMethod(test.name(), args, true);

				for (int i = 0; i < args.length; i++) {
					push(args[i], bif.getArgumentTypes()[i]);
				}

				mv.visitMethodInsn(INVOKESTATIC, bif.owner.getInternalName(),
						bif.getName(), bif.getDescriptor());

				if (failLabel != 0) {
					// guard
					if (bif.getReturnType().getSort() != Type.OBJECT)
						throw new Error("guards must return object type");

					if (out != null) {
						pop(out, bif.getReturnType());
						push(out, bif.getReturnType());
					}

					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
				} else {
					pop(out, bif.getReturnType());
				}

				return;

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

				case fmove:
					push(arg1, arg2.type);
					pop(arg2, arg2.type);
					break;

				case move:

					push(arg1, arg2.type);
					pop(arg2, arg2.type);

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

				if (!EOBJECT_TYPE.equals(type)) {
					throw new Error("expecting EObject");
				}

				switch (test) {
				case is_nonempty_list:
					return IS_NONEMPTY_LIST_TEST;
				case is_nil:
					return IS_NIL_TEST;
				case is_list:
					return IS_LIST_TEST;
				}

				throw new Error("unhandled " + test);
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

				if (isExternal) {
					String field = CompilerVisitor.this
							.getExternalFunction(fun);
					mv.visitFieldInsn(GETSTATIC, self_type.getInternalName(),
							field, EFUN_TYPE.getDescriptor());
					for (int i = 0; i < args.length; i++) {
						push(args[i], EOBJECT_TYPE);
					}

					mv.visitMethodInsn(INVOKEVIRTUAL, EFUN_TYPE
							.getInternalName(), is_tail ? "invoke_tail"
							: "invoke", EUtil.getSignature(args.length));

					if (is_tail) {
						mv.visitInsn(ARETURN);
					}

				} else {

					for (int i = 0; i < args.length; i++) {
						push(args[i], EOBJECT_TYPE);
					}

					mv.visitMethodInsn(INVOKESTATIC, self_type
							.getInternalName(), EUtil.getJavaName(fun.fun,
							fun.no)
							+ (is_tail ? "$tail" : ""), EUtil
							.getSignature(args.length));

					if (is_tail) {
						mv.visitInsn(ARETURN);
					}

				}
			}
		}

	}

	static Method IS_NONEMPTY_LIST_TEST = Method
			.getMethod("erjang.ESeq asSeq()");
	static Method IS_LIST_TEST = Method.getMethod("erjang.ECons asCons()");
	static Method IS_NIL_TEST = Method.getMethod("erjang.ENil asNil()");

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
}
