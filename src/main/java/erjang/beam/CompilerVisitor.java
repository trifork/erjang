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

import static erjang.beam.CodeAtoms.ERLANG_ATOM;
import static erjang.beam.CodeAtoms.FALSE_ATOM;
import static erjang.beam.CodeAtoms.TRUE_ATOM;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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

import com.trifork.clj_ds.IPersistentCollection;

import erjang.EAtom;
import erjang.EBinMatchState;
import erjang.EBinary;
import erjang.EBitString;
import erjang.EBitStringBuilder;
import erjang.ECons;
import erjang.EDouble;
import erjang.EFun;
import erjang.EInteger;
import erjang.EInternalPID;
import erjang.EList;
import erjang.EModuleManager;
import erjang.ENil;
import erjang.ENumber;
import erjang.EObject;
import erjang.EOutputStream;
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
import erjang.ErlangException;
import erjang.Export;
import erjang.FunID;
import erjang.Import;
import erjang.LocalFunID;
import erjang.Module;
import erjang.beam.Arg.Kind;
import erjang.beam.ModuleAnalyzer.FunInfo;
import erjang.beam.repr.ExtFun;
import erjang.beam.repr.Insn;
import erjang.m.erlang.ErlBif;

/**
 * 
 */
public class CompilerVisitor implements ModuleVisitor, Opcodes {
	public static boolean PARANOIA_MODE = false;
	
	// a select ins with up to this many cases that are all
	// atom values is just encoded as an if-then-else-etc.
	public static final int ATOM_SELECT_IF_ELSE_LIMIT = 4;

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
	static final Type ECOMPILEDMODULE_TYPE = Type.getType(ECompiledModule.class);
	/**
	 * 
	 */
	static final String ECOMPILEDMODULE_NAME = ECOMPILEDMODULE_TYPE.getInternalName();
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
				ECOMPILEDMODULE_NAME, null);

		add_module_annotation(cv);

		cv.visitSource(name.getName()+".S", null);
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

		for (Map.Entry<String, ExtFun> ent : imported.entrySet()) {
			String field_name = ent.getKey();
			ExtFun f = ent.getValue();

			FieldVisitor fv = cv.visitField(ACC_STATIC, ent.getKey(), "L"
					+ EFUN_NAME + f.arity + ";", null, null);
			EFun.ensure(f.arity);
			AnnotationVisitor av = fv.visitAnnotation(IMPORT_ANN_TYPE
					.getDescriptor(), true);
			av.visit("module", f.mod.getName());
			av.visit("fun", f.fun.getName());
			av.visit("arity", f.arity);
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
		
		if (this.module_md5 != null) {
			cv.visitField(ACC_STATIC, "module_md5", EBINARY_TYPE.getDescriptor(), null, null);
			module_md5.emit_const(mv);
			mv.visitFieldInsn(PUTSTATIC, self_type.getInternalName(), "module_md5", EBINARY_TYPE.getDescriptor());
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

		// make the method attributes
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
		mv.visitMethodInsn(INVOKESPECIAL, ECOMPILEDMODULE_NAME, "<init>", "()V");		
		mv.visitInsn(RETURN);
		mv.visitMaxs(1, 1);
		mv.visitEnd();

		mv = cv.visitMethod(ACC_PUBLIC, "registerImportsAndExports", "()V", null, null);
		mv.visitCode();
		mv.visitVarInsn(ALOAD, 0);
		mv.visitMethodInsn(INVOKESPECIAL, ECOMPILEDMODULE_NAME,  "registerImportsAndExports", "()V");

		for (Lambda l : lambdas_xx.values() ) {
			
			mv.visitTypeInsn(NEW, Type.getInternalName(LocalFunID.class));
			mv.visitInsn(DUP);
			
			module_name.emit_const(mv);
			l.fun.emit_const(mv);
			push_int(mv, l.arity);
			push_int(mv, l.old_index);
			push_int(mv, l.index);
			push_int(mv, l.old_uniq);
			mv.visitFieldInsn(GETSTATIC, self_type.getInternalName(), "module_md5", EBINARY_TYPE.getDescriptor());

			mv.visitMethodInsn(INVOKESPECIAL, Type.getInternalName(LocalFunID.class), "<init>", 
							"("+EATOM_DESC+EATOM_DESC+"IIII"+EBINARY_TYPE.getDescriptor()+")V"
							  );
			
			mv.visitInsn(DUP);
			cv.visitField(ACC_STATIC, anon_fun_name(l), Type.getDescriptor(LocalFunID.class), null, null).visitEnd();
			mv.visitFieldInsn(PUTSTATIC, self_type.getInternalName(), anon_fun_name(l), Type.getDescriptor(LocalFunID.class));
			
			String mname = EUtil.getJavaName(l.fun, l.arity-l.freevars);
			String outer_name = self_type.getInternalName();
			String inner_name = "FN_" + mname;
			String full_inner_name = outer_name + "$" + inner_name;

			mv.visitLdcInsn(full_inner_name.replace('/', '.'));
			
			mv.visitMethodInsn(INVOKESTATIC, Type.getInternalName(Class.class), "forName", 
								"(Ljava/lang/String;)Ljava/lang/Class;");
			
			mv.visitMethodInsn(INVOKESTATIC, Type.getInternalName(EModuleManager.class), "register_lambda",
											 "(" + Type.getDescriptor(LocalFunID.class) 
											     + Type.getDescriptor(Class.class) + ")V");
		}
		
		mv.visitInsn(RETURN);
		mv.visitMaxs(1, 1);
		mv.visitEnd();
		
	}

	public static String anon_fun_name(Lambda l) {
		return "lambda_"+l.index+"_"+l.old_index+"_"+l.old_uniq;
	}
	
	private void push_int(MethodVisitor mv, int val) {
		if (val == -1) {
			mv.visitInsn(ICONST_M1);
		} else if (val >= 0 && val <= 5) {
			mv.visitInsn(ICONST_0+val);
		} else {
			mv.visitLdcInsn(new Integer(val));
		}
		
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

	@Override
	public void declareFunction(EAtom fun, int arity, int label) {
		/* ignore */
	}

	@Override
	public FunctionVisitor visitFunction(EAtom name, int arity, int startLabel) {
		return new ASMFunctionAdapter(name, arity, startLabel);
	}

	Map<FunID, Lambda> lambdas_xx = new TreeMap<FunID, Lambda>();
	Map<String, String> funs = new HashMap<String, String>();
	Map<String, String> funt = new HashMap<String, String>();
	Set<String> non_pausable_methods = new HashSet<String>();

	public EBinary module_md5;

	private static final String SEQ_CONS_SIG = "(" + EOBJECT_DESC + ")"
		+ ESEQ_DESC;
	private static final String FUNC_INFO_SIG = "(" + EATOM_DESC + EATOM_DESC + ESEQ_DESC +")"
		+ EOBJECT_DESC;
	private static final String ERT_CONS_SIG = "(" + EOBJECT_DESC + EOBJECT_DESC + ")"
		+ ECONS_TYPE.getDescriptor();
	private static final String TEST_FUN_SIG ="(" + EOBJECT_DESC + EFUN_DESCRIPTOR + ")V";


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
		private FunInfo funInfo;

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
		public void visitMaxs(Collection<Integer> x_regs, int y_count, int fp_count,
				boolean isTailRecursive) {

			FunID me = new FunID(module_name, fun_name, arity);
			this.funInfo = funInfos.get(me);
			this.isTailRecursive = isTailRecursive;
			
			Lambda lambda = get_lambda_freevars(fun_name, arity);
			final int freevars = 
					lambda == null 
					? 0
					: lambda.freevars;
			
			int real_arity = arity-freevars;

			String javaName = EUtil.getJavaName(fun_name, real_arity);
			String signature = EUtil.getSignature(arity, true);
			mv = cv.visitMethod(ACC_STATIC | ACC_PUBLIC, javaName, signature,
					null, funInfo.is_pausable ? PAUSABLE_EX : null);

			if (!funInfo.is_pausable) {
				non_pausable_methods.add(javaName);
			}
			
			this.start = new Label();
			this.end = new Label();

			mv.visitCode();
			allocate_regs_to_locals(x_regs, y_count, fp_count);

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

			int arity_plus = arity;
			Lambda lambda = get_lambda_freevars(fun_name, arity_plus);
			final int freevars = 
					lambda == null 
					? 0
					: lambda.freevars;
			
			int real_arity = arity_plus-freevars;
			
			String mname = EUtil.getJavaName(fun_name, real_arity);
			String outer_name = self_type.getInternalName();
			String inner_name = "FN_" + mname;
			String full_inner_name = outer_name + "$" + inner_name;

			boolean make_fun = false;
			boolean is_exported = isExported(fun_name, arity);
			if (lambda != null) {
				CompilerVisitor.this.module_md5 = lambda.uniq;
				make_fun = true;
			} else {

				if (funInfo.is_called_locally_in_nontail_position)
					generate_invoke_call_self();
				
				if (funInfo.is_called_locally_in_tail_position)
					generate_tail_call_self(full_inner_name);

				if (funInfo.mustHaveFun()) {
					FieldVisitor fv = cv.visitField(ACC_STATIC | ACC_FINAL, mname,
							"L" + full_inner_name + ";", null, null);
					EFun.ensure(arity);
	
					if (is_exported) {
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
					funt.put(mname, full_inner_name);
					EFun.ensure(arity);
					make_fun = true;
				}
			}

			if (make_fun) {
			
			cv.visitInnerClass(full_inner_name, outer_name, inner_name,
					ACC_STATIC);

			byte[] data = CompilerVisitor.make_invoker(module_name.getName(), fun_name.getName(), self_type, mname, mname,
					arity, true, is_exported, lambda, EOBJECT_TYPE, funInfo.may_return_tail_marker, funInfo.is_pausable|funInfo.call_is_pausable);

			ClassWeaver w = new ClassWeaver(data, new Compiler.ErjangDetector(
					self_type.getInternalName(), non_pausable_methods));
			if (w.getClassInfos().size() == 0) { // Class did not need weaving
				try {
					classRepo.store(full_inner_name, data);
				} catch (IOException e) {
					e.printStackTrace();
				}
			} else {
				for (ClassInfo ci : w.getClassInfos()) {
					try {
					// System.out.println("> storing "+ci.className);
						classRepo.store(ci.className, ci.bytes);
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
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

			boolean pausable = funInfo.is_pausable || funInfo.call_is_pausable;
			String javaName = EUtil.getJavaName(fun_name, arity);
			String signature = EUtil.getSignature(arity, true);
			mv = cv.visitMethod(ACC_STATIC, javaName + "$call", signature,
					null, pausable ? PAUSABLE_EX : null);
			mv.visitCode();
			
			if (!pausable) {
				non_pausable_methods.add(javaName + "$call");
			}

			// if (isTailRecursive) {

			mv.visitVarInsn(ALOAD, 0);
			for (int i = 0; i < arity; i++) {
				mv.visitVarInsn(ALOAD, i + 1);
			}
			mv.visitMethodInsn(INVOKESTATIC, self_type.getInternalName(),
					javaName, EUtil.getSignature(arity, true));
			
			if (funInfo.may_return_tail_marker) {
			
			mv.visitVarInsn(ASTORE, arity + 1);

			Label done = new Label();
			Label loop = new Label();
			mv.visitLabel(loop);
			mv.visitVarInsn(ALOAD, arity + 1);
			if (EProc.TAIL_MARKER == null) {
				mv.visitJumpInsn(IFNONNULL, done);
			} else {
				mv.visitFieldInsn(GETSTATIC, EPROC_NAME, "TAIL_MARKER",
						EOBJECT_DESC);
				mv.visitJumpInsn(IF_ACMPNE, done);
			}
			// load proc
			mv.visitVarInsn(ALOAD, 0);
			mv.visitFieldInsn(GETFIELD, EPROC_NAME, "tail", EFUN_DESCRIPTOR);
			mv.visitVarInsn(ALOAD, 0);

			mv.visitMethodInsn(INVOKEVIRTUAL, EFUN_NAME, (pausable ? "go" : "go2"), GO_DESC);
			mv.visitVarInsn(ASTORE, arity + 1);

			mv.visitJumpInsn(GOTO, loop);

			mv.visitLabel(done);
			mv.visitVarInsn(ALOAD, arity + 1);

			}
			

			mv.visitInsn(ARETURN);
			mv.visitMaxs(arity + 2, arity + 2);
			mv.visitEnd();

		}

		/**
		 * @param full_inner_name TODO
		 * 
		 */
		private void generate_tail_call_self(String full_inner_name) {

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
					"L" + full_inner_name + ";");
			mv.visitFieldInsn(PUTFIELD, EPROC_NAME, "tail", EFUN_DESCRIPTOR);
			if (EProc.TAIL_MARKER == null) {
				mv.visitInsn(ACONST_NULL);
			} else {
				mv.visitFieldInsn(GETSTATIC, EPROC_NAME, "TAIL_MARKER",
						EOBJECT_DESC);
			}
			
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
		private void allocate_regs_to_locals(Collection<Integer> xRegs, int yCount, int fpCount) {

			int max_y = yCount;
			int max_f = fpCount;

			int local = 1;

			Integer[] xxregs = xRegs.toArray(new Integer[xRegs.size()]);
			
			if (xxregs.length > 0) {
				Arrays.sort(xxregs);
				
				Integer biggest_used = xxregs[ xxregs.length - 1 ];
				xregs = new int[biggest_used+1];
				for (int i = 0; i < xxregs.length; i++) {
					xregs[xxregs[i]] = i+local;	
				}
	
				local += xxregs.length;
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
			mv.visitLineNumber(label & 0x7fff, blockLabel);
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
			public void visitBSAdd(Arg in1, Arg in2, int scale, Arg out) {
				push(in1, Type.INT_TYPE);
				push_scaled(in2, scale);
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
							.getInternalName(), "bs_context_to_binary", "(" +EOBJECT_DESC + ")" + EOBJECT_DESC);
					pop(arg, EOBJECT_TYPE);
					return;

				case bs_utf8_size:
					push(arg, EOBJECT_TYPE);
					mv.visitMethodInsn(INVOKESTATIC, EBINSTRINGBUILDER_TYPE.getInternalName(), "bs_utf8_size", 
							"(" + EOBJECT_DESC +")" + ESMALL_TYPE.getDescriptor());
					pop(imm, ESMALL_TYPE);
					return;
					
				case bs_utf16_size:
					push(arg, EOBJECT_TYPE);
					mv.visitMethodInsn(INVOKESTATIC, EBINSTRINGBUILDER_TYPE.getInternalName(), "bs_utf16_size", 
							"(" + EOBJECT_DESC +")" + ESMALL_TYPE.getDescriptor());
					pop(imm, ESMALL_TYPE);
					return;
					
				}
				

				throw new Error("unhandled: " + opcode);
			}

			@Override
			public void visitInitWritable(Arg size, Arg out) {

				push(size, EOBJECT_TYPE);
				mv.visitMethodInsn(INVOKESTATIC, 
						EBITSTRINGBUILDER_TYPE.getInternalName(), "bs_init_writable", 
						"("+EOBJECT_DESC+")"+EBITSTRINGBUILDER_TYPE.getDescriptor());

				mv.visitInsn(DUP);
				mv.visitVarInsn(ASTORE, bit_string_builder);

				mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
						.getInternalName(), "bitstring", "()"
						+ EBITSTRING_TYPE.getDescriptor());

				pop(out, EBITSTRING_TYPE);
				
			}
			
			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitInitBitString(erjang.EObject,
			 * erjang.beam.Arg, erjang.beam.Arg)
			 */
			@Override
			public void visitInitBitString(Arg size, int flags, Arg out, boolean unit_is_bits) {
				push(size, Type.INT_TYPE);
				mv.visitLdcInsn(new Integer(flags));
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
			public void visitBitStringAppend(BeamOpcode opcode, int label, Arg extra_size, Arg src, int unit, int flags, Arg dst) {
				push(src, EOBJECT_TYPE);
				push(extra_size, Type.INT_TYPE);
				push_int(unit);
				push_int(flags);
				mv.visitMethodInsn(INVOKESTATIC, EBITSTRINGBUILDER_TYPE.getInternalName(),
							opcode.name(), "("+ EOBJECT_DESC +"III)"+EBITSTRINGBUILDER_TYPE.getDescriptor());

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
					push(arg, EDOUBLE_TYPE);
					push_scaled(size, unit);
					push_int(flags);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
									   .getInternalName(), "put_float", "("+EOBJECT_DESC+"II)V");
					return;

				case bs_put_utf8:
					mv.visitVarInsn(ALOAD, bit_string_builder);
					push(arg, EINTEGER_TYPE);
					push_int(flags);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
									   .getInternalName(), "put_utf8", "("+EOBJECT_DESC+"I)V");
					return;

				case bs_put_utf16:
					mv.visitVarInsn(ALOAD, bit_string_builder);
					push(arg, EINTEGER_TYPE);
					push_int(flags);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
									   .getInternalName(), "put_utf16", "("+EOBJECT_DESC+"I)V");
					return;

				case bs_put_utf32:
					mv.visitVarInsn(ALOAD, bit_string_builder);
					push(arg, EINTEGER_TYPE);
					push_int(flags);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
									   .getInternalName(), "put_utf32", "("+EOBJECT_DESC+"I)V");
					return;

				case bs_put_binary:
					mv.visitVarInsn(ALOAD, bit_string_builder);
					push(arg, EBITSTRING_TYPE);

					if (size.kind == Kind.IMMEDIATE &&
						size.value.equals(EAtom.intern("all")))
						push_int(-1);
					else
						push_scaled(size, unit);

					push_int(flags);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBITSTRINGBUILDER_TYPE
							.getInternalName(), "put_bitstring", "("
							+ EOBJECT_TYPE.getDescriptor() + "II)V");
					return;

				}

				throw new Error("unhandled: " + opcode);
			}

			@Override
			public void visitBitStringTest(BeamOpcode test, int failLabel, Arg in, int intg, Arg dst) {
				switch (test) {
				case bs_start_match2: {
					push(in, EOBJECT_TYPE);
					push_int(intg); // Slots
					mv.visitMethodInsn(INVOKESTATIC, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(" + EOBJECT_DESC
							+ "I)" + EMATCHSTATE_TYPE.getDescriptor());

					mv.visitInsn(DUP);
					mv.visitVarInsn(ASTORE, bit_string_matcher);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
					mv.visitVarInsn(ALOAD, bit_string_matcher);

					pop(dst, EBINMATCHSTATE_TYPE);
					return;
				}

				/* {test,bs_get_utf8,{f,6},[{x,0},1,
				   {field_flags,[...,unsigned,big]},{x,1}]}. */
				case bs_get_utf8:
				case bs_get_utf16:
				case bs_get_utf32: {
					push(in, EBINMATCHSTATE_TYPE);
					push_int(intg); // Flags
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(I)I");
					mv.visitInsn(DUP);
					mv.visitVarInsn(ISTORE, scratch_reg);

					mv.visitJumpInsn(IFLT, getLabel(failLabel));
					mv.visitVarInsn(ILOAD, scratch_reg);
					
					emit_box(Type.INT_TYPE, ESMALL_TYPE);

					pop(dst, ESMALL_TYPE);
					return;
				}
				default:
					throw new Error("unhandled bit string test: " + test);
				}
			}

			@Override
			public void visitBitStringTest(BeamOpcode test, int failLabel, Arg in, EBitString bin) {
				switch (test) {
				case bs_match_string: {
					push(in, EBINMATCHSTATE_TYPE);
					push_immediate(bin, EBITSTRING_TYPE);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "("
							+ EBITSTRING_TYPE.getDescriptor() + ")"
							+ EBITSTRING_TYPE);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
					return;
				}
				default:
					throw new Error("unhandled bit string test: " + test);
				}
			}

			@Override
			public void visitBitStringTest(BeamOpcode test, int failLabel, Arg in, Arg bits, int unit, int flags) {
				switch (test) {
				case bs_skip_bits2: {
					// {test,bs_skip_bits2, {f,39},
					// [{x,1},{x,0},8,{field_flags,0}]}
					push(in, EBINMATCHSTATE_TYPE);
					push(bits, EINTEGER_TYPE);
					push_int(unit); // TODO: Scale here instead?
					push_int(flags);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(" + EOBJECT_DESC
							+ "II)" + EOBJECT_DESC);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
					return;
				}
				default:
					throw new Error("unhandled bit string test: " + test);
				}
			}

			@Override
			public void visitBitStringTest(BeamOpcode test, int failLabel, Arg in, Arg bits, int unit, int flags, Arg dst) {
				switch (test) {
				case bs_get_binary2: {
					push(in, EBINMATCHSTATE_TYPE);
					push(bits, EOBJECT_TYPE); //TODO: scale by unit, handling 'all'
					push_int(flags);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(" + EOBJECT_DESC
							+ "I)" + EBITSTRING_TYPE);

					mv.visitInsn(DUP);
					mv.visitVarInsn(ASTORE, scratch_reg);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
					mv.visitVarInsn(ALOAD, scratch_reg);

					pop(dst, EBITSTRING_TYPE);
					return;
				}
					// {test,bs_get_integer2,{f,348},[{x,3},4,{integer,32},1,{field_flags,0},{x,4}]}
				case bs_get_integer2: {
					push(in, EBINMATCHSTATE_TYPE);
					push(bits, Type.INT_TYPE);
					push_int(unit);
					push_int(flags);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(III)"
							+ EINTEGER_TYPE.getDescriptor());

					mv.visitInsn(DUP);
					mv.visitVarInsn(ASTORE, scratch_reg);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
					mv.visitVarInsn(ALOAD, scratch_reg);

					pop(dst, EOBJECT_TYPE);
					return;
				}
				case bs_get_float2: {
					push(in, EBINMATCHSTATE_TYPE);
					push(bits, Type.INT_TYPE);
					push_int(unit);
					push_int(flags);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(III)"
							+ EDOUBLE_TYPE.getDescriptor());

					mv.visitInsn(DUP);
					mv.visitVarInsn(ASTORE, scratch_reg);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));
					mv.visitVarInsn(ALOAD, scratch_reg);

					pop(dst, EOBJECT_TYPE);
					return;
				}
				default:
					throw new Error("unhandled bit string test: " + test);
				}
			}

			@Override
			public void visitBitStringTest(BeamOpcode test, int failLabel, Arg in, int intg) {
				// case bs_test_tail2: // intg == expected bits left
				// case bs_test_unit:  // intg == unit
				// case bs_skip_utfXX: // intg == flags
					push(in, EBINMATCHSTATE_TYPE);
					push_int(intg);
					mv.visitMethodInsn(INVOKEVIRTUAL, EBINMATCHSTATE_TYPE
							.getInternalName(), test.name(), "(I)Z");
					mv.visitJumpInsn(IFEQ, getLabel(failLabel));
					return;
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
					push(in[0], EOBJECT_TYPE);
					push(in[1], EOBJECT_TYPE);

					// raise will actually throw (if successful)
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "raise", "("
							+ EOBJECT_DESC + EOBJECT_DESC + ")"
							+ EOBJECT_DESC);
					mv.visitInsn(ARETURN);

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
			
			public void visitDecrement(Arg src, Arg out) {
				
				push(src, src.type);
				mv.visitMethodInsn(INVOKEVIRTUAL, EOBJECT_NAME, "dec", "()"+ENUMBER_TYPE.getDescriptor());
				pop(out, EOBJECT_TYPE);
				
			};
			
			@Override
			public void visitIncrement(Arg src, Arg out) {

				push(src, src.type);
				mv.visitMethodInsn(INVOKEVIRTUAL, EOBJECT_NAME, "inc", "()"+ENUMBER_TYPE.getDescriptor());
				pop(out, EOBJECT_TYPE);
				return;
				
			}
			
			@Override
			public void visitInsn(BeamOpcode opcode, int failLabel, Arg[] in,
					Arg out, BuiltInFunction bif) {

				ensure_exception_handler_in_place();

				switch (opcode) {
				case bif0:
				case bif1:
				case bif2:
				case gc_bif1:
				case gc_bif2:

				case fadd:
				case fsub:
				case fmul:
				case fdiv:
					Type[] parameterTypes = bif.getArgumentTypes();
					push(in, parameterTypes, bif.isVirtual());
					
					mv.visitMethodInsn(bif.isVirtual() ? INVOKEVIRTUAL : INVOKESTATIC, bif.owner
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
						if (PARANOIA_MODE && bif.getReturnType().getSort() == Type.OBJECT) {
							// Expect non-guards to return non-null.
							mv.visitInsn(DUP);
							mv.visitLdcInsn(bif.toString());
							mv.visitMethodInsn(INVOKESTATIC, ERT_NAME,
											   "paranoiaCheck", "(Lerjang/EObject;Ljava/lang/String;)V");
						}

						pop(out, bif.getReturnType());
					}

					return;

				}

				throw new Error();
			}

			public void visitUnreachablePoint() {
				mv.visitLdcInsn("Reached unreachable point.");
				mv.visitInsn(DUP);
				mv.visitMethodInsn(INVOKESPECIAL, "java/lang/RuntimeException", "<init>", "(Ljava/lang/String;)V");
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
			private void push(Arg[] in, Type[] parameterTypes, boolean isVirtual) {
				
				int off = 0;
				if (isVirtual) {
					push(in[0], EOBJECT_TYPE);
					off = 1;
				}
				
				if (in.length == parameterTypes.length - 1
						&& EPROC_TYPE.equals(parameterTypes[0])) {
					mv.visitVarInsn(ALOAD, 0);
				}

				for (int i = 0; i < in.length-off; i++) {
					Arg arg = in[i+off];
					Type pt = parameterTypes[i];
					push(arg, pt);
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
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "loop_rec",
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
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "cons", ERT_CONS_SIG);
					pop(out, ECONS_TYPE);
					return;
				case call_fun: {
					ensure_exception_handler_in_place();
					
					int nargs = in.length - 1;
					push(in[nargs], EOBJECT_TYPE);
					mv.visitInsn(DUP);
					
					String funtype = EFUN_NAME + nargs;

					mv.visitMethodInsn(INVOKESTATIC, funtype, "cast", "("
							+ EOBJECT_DESC + ")L" + funtype + ";");
					
					mv.visitInsn(DUP_X1);
					
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "test_fun", TEST_FUN_SIG);

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
					mv.visitVarInsn(ALOAD, 0);
					mv
							.visitMethodInsn(INVOKESTATIC, ERT_NAME, "timeout",
									"("+EPROC_DESC+")V");
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
					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "wait",
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
					
					int known_arity = get_known_arity(val.type);
					if (known_arity >= pos+1) {
						push(val, val.type);
						mv.visitFieldInsn(GETFIELD, val.type.getInternalName(),
								"elem" + (pos + 1), EOBJECT_DESC);
					} else {
						push(val, val.type);
						mv.visitTypeInsn(CHECKCAST, ETUPLE_NAME);
						push_int(pos + 1);
						mv.visitMethodInsn(INVOKEVIRTUAL, ETUPLE_NAME, "elm", 
								"(I)" + EOBJECT_DESC);
					}
					
					pop(out, EOBJECT_TYPE);
					return;
				} else if (opcode == BeamOpcode.set_tuple_element) {

					push(out, out.type);
					if (get_known_arity(out.type) < 0)
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
			 * @param type
			 * @return -1 if non-ETuple; arity [#elements] otherwise
			 */
			private int get_known_arity(Type type) {
				String in = type.getInternalName();
				if (in.startsWith(ETUPLE_NAME)) {
					int pfx_len = ETUPLE_NAME.length();

					if (in.length() == pfx_len) {
						return 0;
					}
					
					try {
						String arity = in.substring(pfx_len);
						return Integer.parseInt(arity);
					} catch (NumberFormatException e) {
						return 0;
					}
					
					
				}
					
				// not a tuple
				return -1;
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
			public void visitInsn(Insn insn) {
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

			public void visitInsn(BeamOpcode opcode, ExtFun f) {
				switch (opcode) {
				case func_info:
					push_immediate(f.mod, EATOM_TYPE);
					push_immediate(f.fun, EATOM_TYPE);

					push_immediate(ERT.NIL, ENIL_TYPE);
					for (int i=f.arity-1; i>=0; i--) {
						push(new Arg(Kind.X, i), EOBJECT_TYPE);
						mv.visitMethodInsn(INVOKEVIRTUAL, ESEQ_NAME, "cons", SEQ_CONS_SIG);
					}

					mv.visitMethodInsn(INVOKESTATIC, ERT_NAME, "func_info", FUNC_INFO_SIG);
					mv.visitInsn(ARETURN);
					return;
				}//switch
				
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
					if (tt.equals(arg.type)) {
						// do nothing //
					} else {
						push(arg, EOBJECT_TYPE);
						mv.visitMethodInsn(INVOKESTATIC, tt.getInternalName(),
								"cast", "(" + arg.type.getDescriptor() + ")"
										+ tt.getDescriptor());
						mv.visitInsn(DUP);
	
						mv.visitVarInsn(ASTORE, scratch_reg);
						mv.visitJumpInsn(IFNULL, getLabel(failLabel));
	
						mv.visitVarInsn(ALOAD, scratch_reg);
						pop(arg, getTubleType(arity));
					}
					return;
				}
				}//switch
				throw new Error("unhandled " + test);
			}

			@Override
			public void visitTest(BeamOpcode test, int failLabel, Arg arg,
					Arg arity, Type funType) {

				switch (test) {
				case is_function2:

					// push object to test
					push(arg, EOBJECT_TYPE);
					// push arity
					push(arity, Type.INT_TYPE);
					// call object.testFunction2(nargs)
					mv.visitMethodInsn(INVOKEVIRTUAL, EOBJECT_NAME,
							"testFunction2", "(I)" + EFUN_DESCRIPTOR);

					mv.visitInsn(DUP);

					mv.visitVarInsn(ASTORE, scratch_reg);
					mv.visitJumpInsn(IFNULL, getLabel(failLabel));

					mv.visitVarInsn(ALOAD, scratch_reg);
					pop(arg, funType);
					return;
				}
				
				throw new Error("unhandled " + test);

			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see erjang.beam.BlockVisitor2#visitTest(erjang.beam.BeamOpcode,
			 * int, erjang.beam.Arg[], erjang.beam.Arg, org.objectweb.asm.Type)
			 */
			@Override
			public void visitTest(BeamOpcode test, int failLabel, Arg[] args,
					Type outType) {

				switch (test) {
				case is_eq_exact:
				{
					if (args[0].kind==Kind.IMMEDIATE && args[0].value.equalsExactly(ESmall.ZERO)) {
						push(args[1], EOBJECT_TYPE);
						mv.visitMethodInsn(INVOKEVIRTUAL, EOBJECT_NAME, "is_zero", 
								"()Z");
						mv.visitJumpInsn(IFEQ, getLabel(failLabel));
						return;
					}

					if (args[1].kind==Kind.IMMEDIATE && args[1].value.equalsExactly(ESmall.ZERO)) {
						push(args[0], EOBJECT_TYPE);
						mv.visitMethodInsn(INVOKEVIRTUAL, EOBJECT_NAME, "is_zero", 
								"()Z");
						mv.visitJumpInsn(IFEQ, getLabel(failLabel));
						return;
					}
				}
				
				case is_ne_exact:
				case is_ne:
				case is_eq: 
				{
					// if arg[0] is object type, and arg[1] is not, then swap args.
					if (args[0].type.equals(EOBJECT_TYPE) && !args[1].type.equals(EOBJECT_TYPE)) {
						Arg t = args[0];
						args[0] = args[1];
						args[1] = t;
					}
				}
				
				case is_lt:
				case is_ge:
				{
				
					// this particular case can be coded as a java instruction instruction
					if ((test == BeamOpcode.is_eq_exact || test == BeamOpcode.is_eq)
							&& (args[0].type.equals(EATOM_TYPE) || args[1].type
									.equals(EATOM_TYPE))) {
						push(args[0], EOBJECT_TYPE);
						push(args[1], EOBJECT_TYPE);
						mv.visitJumpInsn(IF_ACMPNE, getLabel(failLabel));
						return;
					}
	
					for (int i = 0; i < args.length; i++) {
						push(args[i], EOBJECT_TYPE);
					}
	
					mv.visitMethodInsn(INVOKEVIRTUAL, EOBJECT_NAME,
							test.name(), "(" + EOBJECT_DESC + ")Z");
	
					if (failLabel != 0) {
						mv.visitJumpInsn(IFEQ, getLabel(failLabel));
					} else {
						throw new Error("test with no fail label?");
					}
					
					//
					
					if (test == BeamOpcode.is_eq_exact && !Type.VOID_TYPE.equals(outType)) {
						
						if (args[0].type.equals(EOBJECT_TYPE) && args[0].kind.isReg()) {
							push(args[1], outType);
							args[0].type = outType;
							pop(args[0], outType);
						} else if (args[1].type.equals(EOBJECT_TYPE) && args[1].kind.isReg()) {
							push(args[0], outType);
							args[1].type = outType;
							pop(args[1], outType);
						}
						
						
					}
					
	
					return;
					}
				
				}
				
				throw new Error("unhandled " + test);

			}

			/**
			 * @param test
			 * @return
			 */
			private String test2name(BeamOpcode test) {
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
			public void visitInsn(BeamOpcode opcode, ExtFun efun,
					Arg[] freevars, int index, int old_index, EBinary uniq, int old_uniq) {
				ensure_exception_handler_in_place();

				if (opcode == BeamOpcode.make_fun2) {

					CompilerVisitor.this.register_lambda(efun.fun, efun.arity,
							freevars.length, index, old_index, uniq, old_uniq);

					String inner = EUtil.getFunClassName(self_type, efun, freevars.length);

					mv.visitTypeInsn(NEW, inner);
					mv.visitInsn(DUP);

					// load proc
					mv.visitVarInsn(ALOAD, 0);
					mv.visitMethodInsn(INVOKEVIRTUAL, EPROC_NAME, "self_handle", "()" + Type.getDescriptor(EInternalPID.class));

					// String funtype = EFUN_NAME + efun.no;
					for (int i = 0; i < freevars.length; i++) {
						push(freevars[i], EOBJECT_TYPE);
					}

					StringBuilder sb = new StringBuilder("(");
					sb.append(EPID_TYPE.getDescriptor());
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

				if (values.length < ATOM_SELECT_IF_ELSE_LIMIT) {
					boolean all_atoms = true;
					for (int i = 0; i < values.length; i++) {
						if (!(values[i].value instanceof EAtom)) {
							all_atoms = false;
							break;
						}
					}
				
					if (all_atoms) {
						
						for (int i = 0; i < values.length; i++) {

							push(in, in.type);
							push(values[i], values[i].type);
							mv.visitJumpInsn(IF_ACMPEQ, getLabel(targets[i]));

						}
						
						mv.visitJumpInsn(GOTO, getLabel(failLabel));
						
						return;
					}
				

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
						return value().erlangCompareTo(o.value());
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

				//if (in.type == null) {
				mv.visitTypeInsn(CHECKCAST, ETUPLE_NAME);
				//}

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
			public void visitCall(ExtFun fun, Arg[] args, boolean is_tail,
					boolean isExternal) {

				ensure_exception_handler_in_place();

				BuiltInFunction bif = null;

				bif = BIFUtil.getMethod(fun.mod.getName(),
						fun.fun.getName(), args, false, false);

				if (bif != null || isExternal) {

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
								(is_tail && !isExitFunc(fun)) ? "invoke_tail" : "invoke", EUtil
										.getSignature(args.length, true));

					} else if (bif.isVirtual()) {
						// System.err.println("DIRECT "+bif);

						push(args[0], bif.owner);

						int off = 0;
						if (bif.getArgumentTypes().length > 0
								&& bif.getArgumentTypes()[0].equals(EPROC_TYPE)) {

							mv.visitVarInsn(ALOAD, 0);
							off = 1;
						}
						for (int i = 1; i < args.length; i++) {
							push(args[i], bif.getArgumentTypes()[off-1]);
						}

						mv.visitMethodInsn(INVOKEVIRTUAL, bif.owner
								.getInternalName(), bif.getName(), bif
								.getDescriptor());
						
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
					if (is_tail && fun.arity == ASMFunctionAdapter.this.arity
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

					FunInfo target = funInfos.get(new FunID(fun.mod, fun.name(), fun.arity));
					
					mv.visitMethodInsn(INVOKESTATIC, self_type
							.getInternalName(), EUtil.getJavaName(fun.fun,
							fun.arity)
							+ (is_tail ? "$tail" : (target.may_return_tail_marker ? "$call" : "")), EUtil
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
			private boolean isExitFunc(ExtFun fun) {
				if (fun.mod == ERLANG_ATOM) {
					if (fun.fun == CodeAtoms.EXIT_ATOM && fun.arity==1)
						return true;
					if (fun.fun == CodeAtoms.ERROR_ATOM &&  (fun.arity==1 || fun.arity==2))
						return true;
					if (fun.fun == CodeAtoms.THROW_ATOM && fun.arity==1)
						return true;
				}

				return false;
			}
		}

	}

	final static Method IS_NONEMPTY_LIST_TEST = Method
			.getMethod("erjang.ECons testNonEmptyList()");
	final static Method IS_LIST_TEST = Method.getMethod("erjang.ECons testCons()");
	final static Method IS_TUPLE_TEST = Method.getMethod("erjang.ETuple testTuple()");
	final static Method IS_INTEGER_TEST = Method
			.getMethod("erjang.EInteger testInteger()");
	final static Method IS_ATOM_TEST = Method.getMethod("erjang.EAtom testAtom()");
	final static Method IS_FLOAT_TEST = Method
			.getMethod("erjang.EDouble testFloat()");
	final static Method IS_NIL_TEST = Method.getMethod("erjang.ENil testNil()");
	final static Method IS_BOOLEAN_TEST = Method
			.getMethod("erjang.EAtom testBoolean()");
	final static Method IS_NUMBER_TEST = Method
			.getMethod("erjang.ENumber testNumber()");
	final static Method IS_BINARY_TEST = Method
	.getMethod("erjang.EBinary testBinary()");
	final static Method IS_BITSTRING_TEST = Method
	.getMethod("erjang.EBitString testBitString()");
	final static Method IS_PID_TEST = Method.getMethod("erjang.EPID testPID()");
	final static Method IS_PORT_TEST = Method.getMethod("erjang.EPort testPort()");
	final static Method IS_REFERENCE_TEST = Method.getMethod(ERef.class.getName()
			+ " testReference()");
	final static Method IS_FUNCTION_TEST = Method
			.getMethod("erjang.EFun testFunction()");
	final static Method IS_FUNCTION2_TEST = Method
			.getMethod("erjang.EFun testFunction(int nargs)");

	Map<String, ExtFun> imported = new HashMap<String, ExtFun>();

	private Map<FunID, FunInfo> funInfos;

	/**
	 * @param fun
	 * @return
	 */
	public String getExternalFunction(ExtFun fun) {

		String name = EUtil.getJavaName(fun);
		if (!imported.containsKey(name)) {
			imported.put(name, fun);
		}

		return name;
	}

	static class Lambda {

		private final EAtom fun;
		private final int arity;
		private final int freevars;
		private final int index;
		private final int old_index;
		private final int old_uniq;
		private final EBinary uniq;

		public Lambda(EAtom fun, int arity, int freevars, int index, int old_index, EBinary uniq, int old_uniq) {
			this.fun = fun;
			this.arity = arity;
			this.freevars = freevars;
			this.index = index;
			this.old_index = old_index;
			this.uniq = uniq;
			this.old_uniq = old_uniq;
		}
		
	}
	
	/**
	 * @param fun
	 * @param arity_plus
	 * @param length
	 * @param index TODO
	 * @param old_index TODO
	 * @param uniq TODO
	 * @param old_uniq 
	 */
	public void register_lambda(EAtom fun, int arity_plus, int freevars, int index, int old_index, EBinary uniq, int old_uniq) {
		
		lambdas_xx.put(new FunID(module_name, fun, arity_plus), 
				       new Lambda(fun, arity_plus, freevars, index, old_index, uniq, old_uniq));
	}

	public Lambda get_lambda_freevars(EAtom fun, int arity_plus) {
		return lambdas_xx.get(new FunID(module_name, fun, arity_plus));
	}

	static public byte[] make_invoker(String module, String function,
			Type self_type, String mname,
			String fname, int arity, boolean proc, boolean exported, Lambda lambda,
			Type return_type, boolean is_tail_call, final boolean is_pausable) {

		int freevars = lambda==null ? 0 : lambda.freevars;
		
		String outer_name = self_type.getInternalName();
		String inner_name = "FN_" + mname;
		String full_inner_name = outer_name + "$" + inner_name;

		ClassWriter cw = new ClassWriter(true);
		int residual_arity = arity - freevars;
		String super_class_name = EFUN_NAME + residual_arity +
			(exported ? "Exported" : "");
		if (exported) EFun.ensure_exported(residual_arity);
		else EFun.ensure(residual_arity);

		cw.visit(V1_6, ACC_FINAL | ACC_PUBLIC, full_inner_name, null,
				super_class_name, null);

		if (lambda != null) {
			cw.visitField(ACC_STATIC|ACC_PUBLIC|ACC_FINAL, "index", "I", null, new Integer(lambda.index));
			cw.visitField(ACC_STATIC|ACC_PUBLIC|ACC_FINAL, "old_index", "I", null, new Integer(lambda.old_index));
			cw.visitField(ACC_STATIC|ACC_PUBLIC|ACC_FINAL, "old_uniq", "I", null, new Integer(lambda.old_uniq));
			
			/** */
			MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "encode", "("+ Type.getDescriptor(EOutputStream.class) +")V", null, null);
			mv.visitCode();
			
			mv.visitVarInsn(ALOAD, 1);

			mv.visitVarInsn(ALOAD, 0);
			mv.visitFieldInsn(GETFIELD, full_inner_name, "pid", EPID_TYPE.getDescriptor());
			mv.visitLdcInsn(module);
			
			mv.visitFieldInsn(GETSTATIC, full_inner_name, "old_index", "I");
			mv.visitInsn(I2L);
			mv.visitLdcInsn(new Integer(arity));
			mv.visitFieldInsn(GETSTATIC, outer_name, "module_md5", EBINARY_TYPE.getDescriptor());
			
			mv.visitFieldInsn(GETSTATIC, full_inner_name, "index", "I");
			mv.visitInsn(I2L);
			mv.visitFieldInsn(GETSTATIC, full_inner_name, "old_uniq", "I");
			mv.visitInsn(I2L);
			
			mv.visitLdcInsn(new Integer(freevars));
			mv.visitTypeInsn(ANEWARRAY, EOBJECT_NAME);
			
			for (int i = 0; i < freevars; i++) {
				mv.visitInsn(DUP);
				
				if (i <= 5) {
					mv.visitInsn(ICONST_0+i);
				} else {
					mv.visitLdcInsn(new Integer(i));
				}
				
				mv.visitVarInsn(ALOAD, 0); // load self
				mv.visitFieldInsn(GETFIELD, full_inner_name, "fv"+i, EOBJECT_DESC);
				
				mv.visitInsn(AASTORE);
			}

			
			mv.visitMethodInsn(INVOKEVIRTUAL, Type.getInternalName(EOutputStream.class), "write_fun", 
					"("+EPID_TYPE.getDescriptor()+"Ljava/lang/String;"
					 +"JI"+EBINARY_TYPE.getDescriptor()
					 +"JJ"+Type.getDescriptor(EObject[].class)+")V");
			
			mv.visitInsn(RETURN);
			mv.visitMaxs(10, 3);
			mv.visitEnd();
		}
		
		make_constructor(cw, module, function,
						 full_inner_name, super_class_name, lambda, exported);

		make_go_method(cw, outer_name, fname, full_inner_name, arity, proc,
				freevars, return_type, is_tail_call, is_pausable);

		make_go2_method(cw, outer_name, fname, full_inner_name, arity, proc,
				freevars, return_type, is_tail_call, is_pausable);

		return cw.toByteArray();

	}

	private static void make_constructor(ClassWriter cw,
			String module_name, String function_name,
			String full_inner_name, String super_class_name, Lambda lambda, boolean exported) {
		StringBuilder sb = new StringBuilder("(");
		int freevars = lambda==null?0:lambda.freevars;
		if (lambda != null) {
			sb.append(EPID_TYPE.getDescriptor());
			cw.visitField(ACC_PUBLIC|ACC_FINAL, "pid", EPID_TYPE.getDescriptor(), null, null);
		}
		// create the free vars
		for (int i = 0; i < freevars; i++) {
			cw.visitField(ACC_PUBLIC | ACC_FINAL, "fv" + i, EOBJECT_DESC,
					null, null);
			sb.append(EOBJECT_DESC);
		}
		sb.append(")V");

		MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "<init>", sb.toString(),
				null, null);
		mv.visitCode();

		mv.visitVarInsn(ALOAD, 0);
		if (exported) {
			mv.visitLdcInsn(module_name);
			mv.visitLdcInsn(function_name);
			mv.visitMethodInsn(INVOKESPECIAL, super_class_name, "<init>", "(Ljava/lang/String;Ljava/lang/String;)V");
		} else {
			mv.visitMethodInsn(INVOKESPECIAL, super_class_name, "<init>", "()V");
		}

		if (lambda != null) {
			mv.visitVarInsn(ALOAD, 0);
			mv.visitVarInsn(ALOAD, 1);
			mv.visitFieldInsn(PUTFIELD, full_inner_name, "pid",
							EPID_TYPE.getDescriptor());

		}
		
		for (int i = 0; i < freevars; i++) {
			mv.visitVarInsn(ALOAD, 0);
			mv.visitVarInsn(ALOAD, i + 2);
			mv
					.visitFieldInsn(PUTFIELD, full_inner_name, "fv" + i,
							EOBJECT_DESC);
		}

		mv.visitInsn(RETURN);
		mv.visitMaxs(3, 3);
		mv.visitEnd();
		
		if (lambda != null) {
			mv = cw.visitMethod(ACC_PROTECTED, "get_env", "()"+ESEQ_DESC, null, null);
			mv.visitCode();
			mv.visitFieldInsn(GETSTATIC, ERT_NAME, "NIL", ENIL_TYPE.getDescriptor());
			for (int i = freevars-1; i >= 0; i--) {
				mv.visitVarInsn(ALOAD, 0);
				mv.visitFieldInsn(GETFIELD, full_inner_name, "fv"+i, EOBJECT_DESC);
				mv.visitMethodInsn(INVOKEVIRTUAL, ESEQ_NAME, "cons", "("+EOBJECT_DESC+")"+ESEQ_DESC);
			}
			mv.visitInsn(ARETURN);
			mv.visitMaxs(3, 3);
			mv.visitEnd();

			mv = cw.visitMethod(ACC_PROTECTED, "get_pid", "()"+EOBJECT_DESC, null, null);
			mv.visitCode();
			mv.visitVarInsn(ALOAD, 0);
			mv.visitFieldInsn(GETFIELD, full_inner_name, "pid", EPID_TYPE.getDescriptor());
			mv.visitInsn(ARETURN);
			mv.visitMaxs(3, 3);
			mv.visitEnd();
			
			mv = cw.visitMethod(ACC_PROTECTED, "get_id", "()"+Type.getDescriptor(FunID.class), null, null);
			mv.visitCode();
			mv.visitFieldInsn(GETSTATIC, full_inner_name.substring(0, full_inner_name.indexOf('$')), 
								anon_fun_name(lambda), Type.getDescriptor(LocalFunID.class));
			mv.visitInsn(ARETURN);
			mv.visitMaxs(3, 3);
			mv.visitEnd();
			
			
		}
	}

	private static void make_invoke_method(ClassWriter cw, String outer_name,
			String mname, int arity, boolean proc, int freevars, Type returnType, boolean isTailCall) {
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
		
		if (isTailCall) {
		mv.visitVarInsn(ASTORE, arity + 2);

		Label done = new Label();
		Label loop = new Label();
		mv.visitLabel(loop);
		mv.visitVarInsn(ALOAD, arity + 2);
		if (EProc.TAIL_MARKER == null) {
			mv.visitJumpInsn(IFNONNULL, done);
		} else {
			mv.visitFieldInsn(GETSTATIC, EPROC_NAME, "TAIL_MARKER", EOBJECT_DESC);
			mv.visitJumpInsn(IF_ACMPNE, done);
		}
			
		// load proc
		mv.visitVarInsn(ALOAD, 1);
		mv.visitFieldInsn(GETFIELD, EPROC_NAME, "tail", EFUN_DESCRIPTOR);
		mv.visitVarInsn(ALOAD, 1);

		mv.visitMethodInsn(INVOKEVIRTUAL, EFUN_NAME, "go", GO_DESC);
		mv.visitVarInsn(ASTORE, arity + 2);

		mv.visitJumpInsn(GOTO, loop);

		mv.visitLabel(done);
		mv.visitVarInsn(ALOAD, arity + 2);
		}
		
		mv.visitInsn(ARETURN);
		mv.visitMaxs(arity + 2, arity + 2);
		mv.visitEnd();
	}

	public static void make_invoketail_method(ClassWriter cw,
			String full_inner, int arity, int freevars) {
		MethodVisitor mv;
		mv = cw.visitMethod(ACC_PUBLIC|ACC_FINAL, "invoke_tail", EUtil.getSignature(arity
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
		if (EProc.TAIL_MARKER == null) {
			mv.visitInsn(ACONST_NULL);
		} else {
			mv.visitFieldInsn(GETSTATIC, EPROC_NAME, "TAIL_MARKER", EOBJECT_DESC);
		}
		mv.visitInsn(ARETURN);
		mv.visitMaxs(arity + 2, arity + 2);
		mv.visitEnd();
	}

	private static void make_go_method(ClassWriter cw, String outer_name,
			String mname, String full_inner, int arity, boolean proc,
			int freevars, Type returnType, boolean isTailCall, boolean isPausable) {
		if (!isPausable) return;
		
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

	private static void make_go2_method(ClassWriter cw, String outer_name,
			String mname, String full_inner, int arity, boolean proc,
			int freevars, Type returnType, boolean isTailCall, boolean isPausable) {
		
		if (isPausable) {
			if (ModuleAnalyzer.DEBUG_ANALYZE) {
				System.err.println
					("not generating go2 (pausable) for "+full_inner);
			}
			return;
		}
		
		MethodVisitor mv;
		mv = cw.visitMethod(ACC_PUBLIC, "go2", GO_DESC, null, null);
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

	public void setFunInfos(Map<FunID, FunInfo> funInfos) {
		this.funInfos = funInfos;
	}

}

/** Active exception handler */
class EXHandler {
	int handler_beam_label;
	Label begin, end, target;
	BeamExceptionHandler beam_exh;
}
