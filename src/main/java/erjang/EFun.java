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

package erjang;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;

import kilim.Pausable;
import kilim.analysis.ClassInfo;
import kilim.analysis.ClassWeaver;
import kilim.tools.Weaver;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import erjang.beam.Compiler;
import erjang.beam.CompilerVisitor;
import erjang.beam.EUtil;

public abstract class EFun extends EObject implements Opcodes {

	public abstract int arity();

	public EFun testFunction2(int nargs) {
		if (this.arity() == nargs)
			return this;
		return null;
	}

	public EFun testFunction() {
		return this;
	}

	@Override
	int cmp_order() {
		return 3;
	}

	/** used for translation of tail recursive methods */
	public abstract EObject go(EProc eproc) throws Pausable;

	/** generic invoke, used only for apply */
	public abstract EObject invoke(EProc proc, EObject[] args) throws Pausable;

	private static final Type EFUN_TYPE = Type.getType(EFun.class);
	private static final String EFUN_NAME = EFUN_TYPE.getInternalName();
	private static final Type EFUNHANDLER_TYPE = Type
			.getType(EFunHandler.class);
	private static final Type EOBJECT_TYPE = Type.getType(EObject.class);
	private static final Type EOBJECT_ARR_TYPE = Type.getType(EObject[].class);
	private static final Type EPROC_TYPE = Type.getType(EProc.class);
	static final String GO_DESC = "(" + EPROC_TYPE.getDescriptor() + ")"
			+ EOBJECT_TYPE.getDescriptor();
	private static final String EPROC_NAME = EPROC_TYPE.getInternalName();
	private static final String EOBJECT_DESC = EOBJECT_TYPE.getDescriptor();
	static final String[] PAUSABLE_EX = new String[] { Type.getType(Pausable.class).getInternalName() };

	/**
	 * @param method
	 * @param arity
	 * @param name
	 * @param module
	 * @return
	 */
	static EFun make(Method method) {

		assert (Modifier.isStatic(method.getModifiers()));
		assert (!Modifier.isPrivate(method.getModifiers()));

		Class<?>[] parameterTypes = method.getParameterTypes();
		int ary = parameterTypes.length;
		boolean proc = (ary > 0 && parameterTypes[0].equals(EProc.class));
		if (proc)
			ary -= 1;
		String mname = EUtil.getJavaName(EAtom.intern(method.getName()), ary);

		Class<?> declaringClass = method.getDeclaringClass();
		Type type = Type.getType(declaringClass);
		byte[] data = CompilerVisitor.make_invoker(type, mname, method
				.getName(), ary, proc, 0, Type.getType(method.getReturnType()));

		String clname = type.getClassName() + "$FN_" + mname;

		ClassLoader cl = declaringClass.getClassLoader();

		// make sure we have it's superclass loaded
		get_fun_class(ary);

		Class<? extends EFun> res_class = ERT.defineClass(cl, clname.replace(
				'/', '.'), data, 0, data.length);

		try {
			return res_class.newInstance();
		} catch (Exception e) {
			throw new Error(e);
		}
	}

	@SuppressWarnings("unchecked")
	static Class<? extends EFun> get_fun_class(int arity) {

		String self_type = EFUN_TYPE.getInternalName() + arity;

		try {
			return (Class<? extends EFun>) Class.forName(EFUN_TYPE
					.getClassName()
					+ arity, true, EFun.class.getClassLoader());
		} catch (ClassNotFoundException ex) {
			// that's what we'll do here...
		}

		byte[] data = gen_fun_class_data(arity);
		
		ClassWeaver w = new ClassWeaver(data, new Compiler.ErjangDetector("/xx/"));
		for (ClassInfo ci : w.getClassInfos()) {
			ETuple.dump(ci.className, ci.bytes);
			
			if (ci.className.equals(EFUN_NAME)) {
				data = ci.bytes;
			}
		}


		return ERT.defineClass(EFun.class.getClassLoader(), self_type.replace(
				'/', '.'), data, 0, data.length);
	}

	static byte[] gen_fun_class_data(int arity) {

		String self_type = EFUN_TYPE.getInternalName() + arity;

		ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
		cw.visit(Opcodes.V1_5, Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT,
				self_type, null, EFUN_TYPE.getInternalName(), null);

		MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC
				| Opcodes.ACC_ABSTRACT, "invoke", EUtil.getSignature(arity,
				true), null, PAUSABLE_EX);
		mv.visitEnd();

		mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT,
				"invoke_tail", EUtil.getSignature(arity, true), null, null);
		mv.visitEnd();

		mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "arity", "()I", null, null);
		mv.visitCode();
		mv.visitLdcInsn(new Integer(arity));
		mv.visitInsn(Opcodes.IRETURN);
		mv.visitMaxs(2, 2);
		mv.visitEnd();

		mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "invoke", "("
				+ EPROC_TYPE.getDescriptor() + EOBJECT_ARR_TYPE.getDescriptor()
				+ ")" + EOBJECT_TYPE.getDescriptor(), null, PAUSABLE_EX);
		mv.visitCode();
		mv.visitVarInsn(Opcodes.ALOAD, 0); // load this
		mv.visitVarInsn(Opcodes.ALOAD, 1); // load proc
		for (int i = 0; i < arity; i++) {
			mv.visitVarInsn(Opcodes.ALOAD, 2);
			push_int(mv, i);
			mv.visitInsn(Opcodes.AALOAD);
		}

		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, self_type, "invoke", EUtil
				.getSignature(arity, true));

		mv.visitInsn(Opcodes.ARETURN);
		mv.visitMaxs(arity + 2, arity + 2);
		mv.visitEnd();

		mv = cw.visitMethod(Opcodes.ACC_PROTECTED, "<init>", "()V", null, null);
		mv.visitVarInsn(Opcodes.ALOAD, 0);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, EFUN_TYPE.getInternalName(),
				"<init>", "()V");
		mv.visitInsn(Opcodes.RETURN);
		mv.visitMaxs(arity + 2, arity + 2);
		mv.visitEnd();

		create_cast(cw, arity);
		
		cw.visitEnd();

		byte[] data = cw.toByteArray();
		return data;
	}

	static Map<Integer, Constructor<? extends EFun>> handlers = new HashMap<Integer, Constructor<? extends EFun>>();

	static EFun get_fun_with_handler(int arity, EFunHandler handler) {
		Constructor<? extends EFun> h = handlers.get(arity);

		if (h == null) {

			String self_type = EFUN_TYPE.getInternalName() + "Handler" + arity;

			ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
			String super_class_name = EFUN_TYPE.getInternalName() + arity;
			cw.visit(Opcodes.V1_4, ACC_PUBLIC, self_type, null,
					super_class_name, null);

			// create handler field
			FieldVisitor fv = cw.visitField(ACC_PRIVATE, "handler",
					EFUNHANDLER_TYPE.getDescriptor(), null, null);
			fv.visitEnd();

			// make constructor
			MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "<init>", "("
					+ EFUNHANDLER_TYPE.getDescriptor() + ")V", null, null);
			mv.visitCode();

			mv.visitVarInsn(ALOAD, 0);
			mv
					.visitMethodInsn(INVOKESPECIAL, super_class_name, "<init>",
							"()V");

			mv.visitVarInsn(ALOAD, 0);
			mv.visitVarInsn(ALOAD, 1);
			mv.visitFieldInsn(PUTFIELD, self_type, "handler", EFUNHANDLER_TYPE
					.getDescriptor());

			mv.visitInsn(RETURN);
			mv.visitMaxs(3, 3);
			mv.visitEnd();

			// make invoke_tail method
			CompilerVisitor.make_invoketail_method(cw, self_type, arity, 0);
			make_invoke_method(cw, self_type, arity);
			make_go_method(cw, self_type, arity);

			cw.visitEnd();
			byte[] data = cw.toByteArray();

			ETuple.dump(self_type, data);

			Class<? extends EFun> clazz = ERT.defineClass(EFun.class
					.getClassLoader(), self_type.replace('/', '.'), data, 0,
					data.length);

			try {
				h = clazz.getConstructor(EFunHandler.class);
			} catch (Exception e) {
				throw new Error(e);
			}

			handlers.put(arity, h);
		}

		try {
			return h.newInstance(handler);
		} catch (Exception e) {
			throw new Error(e);
		}
	}
	
	private static void create_cast(ClassWriter cw, int n) {
		MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC|Opcodes.ACC_STATIC, "cast", 
				"(" + EOBJECT_DESC + ")L" + EFUN_NAME + n + ";",
				null, null);
		mv.visitCode();

		mv.visitVarInsn(Opcodes.ALOAD, 0);
		mv.visitTypeInsn(INSTANCEOF, EFUN_NAME+n);
		
		Label fail = new Label();
		
		mv.visitJumpInsn(Opcodes.IFEQ, fail);
		mv.visitVarInsn(Opcodes.ALOAD, 0);
		mv.visitTypeInsn(Opcodes.CHECKCAST, EFUN_NAME+n);
		mv.visitInsn(Opcodes.ARETURN);
		
		mv.visitLabel(fail);
		mv.visitInsn(Opcodes.ACONST_NULL);
		mv.visitInsn(Opcodes.ARETURN);
		
		mv.visitMaxs(2, 2);
		mv.visitEnd();
	}


	private static void make_go_method(ClassWriter cw, String self_type,
			int arity) {
		MethodVisitor mv;
		mv = cw.visitMethod(ACC_PUBLIC, "go", GO_DESC, null, null);
		mv.visitCode();

		for (int i = 0; i < arity; i++) {
			mv.visitVarInsn(ALOAD, 1);
			mv.visitFieldInsn(GETFIELD, EPROC_NAME, "arg" + i, EOBJECT_DESC);
			mv.visitVarInsn(ASTORE, i + 2);
		}
		for (int i = 0; i < arity; i++) {
			mv.visitVarInsn(ALOAD, 1);
			mv.visitInsn(ACONST_NULL);
			mv.visitFieldInsn(PUTFIELD, EPROC_NAME, "arg" + i, EOBJECT_DESC);
		}

		// load handler
		mv.visitVarInsn(ALOAD, 0);
		mv.visitFieldInsn(GETFIELD, self_type, "handler", EFUNHANDLER_TYPE
				.getDescriptor());

		// load proc
		mv.visitVarInsn(ALOAD, 1);

		// create array
		mv.visitLdcInsn(new Integer(arity));
		mv.visitTypeInsn(ANEWARRAY, EOBJECT_TYPE.getInternalName());

		for (int i = 0; i < arity; i++) {
			mv.visitInsn(DUP);
			mv.visitLdcInsn(new Integer(i));
			mv.visitVarInsn(ALOAD, i + 2);
			mv.visitInsn(AASTORE);
		}

		mv.visitMethodInsn(INVOKEINTERFACE, EFUNHANDLER_TYPE.getInternalName(), "invoke", 
				"(" + EPROC_TYPE.getDescriptor() + "[" + EOBJECT_DESC + ")" + EOBJECT_DESC);
		mv.visitInsn(ARETURN);
		mv.visitMaxs(arity + 2, arity + 2);
		mv.visitEnd();

		cw.visitEnd();
	}

	private static void make_invoke_method(ClassWriter cw, String self_type,
			int arity) {
		MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "invoke", EUtil
				.getSignature(arity, true), null, null);
		mv.visitCode();
		mv.visitVarInsn(ALOAD, 0);
		mv.visitVarInsn(ALOAD, 1);
		for (int i = 0; i < arity; i++) {
			mv.visitVarInsn(ALOAD, i + 2);
		}
		mv.visitMethodInsn(INVOKEVIRTUAL, self_type, "invoke_tail", EUtil
				.getSignature(arity, true));
		mv.visitVarInsn(ASTORE, arity + 2);

		Label done = new Label();
		Label loop = new Label();
		mv.visitLabel(loop);
		mv.visitVarInsn(ALOAD, arity + 2);
		mv.visitFieldInsn(GETSTATIC, EPROC_TYPE.getInternalName(),
				"TAIL_MARKER", EOBJECT_TYPE.getDescriptor());
		mv.visitJumpInsn(IF_ACMPNE, done);

		// load proc
		mv.visitVarInsn(ALOAD, 1);
		mv.visitFieldInsn(GETFIELD, EPROC_TYPE.getInternalName(), "tail",
				EFUN_TYPE.getDescriptor());
		mv.visitVarInsn(ALOAD, 1);

		mv.visitMethodInsn(INVOKEVIRTUAL, EFUN_TYPE.getInternalName(), "go",
				GO_DESC);
		mv.visitVarInsn(ASTORE, arity + 2);

		mv.visitJumpInsn(GOTO, loop);

		mv.visitLabel(done);
		mv.visitVarInsn(ALOAD, arity + 2);
		mv.visitInsn(ARETURN);
		mv.visitMaxs(arity + 2, arity + 2);
		mv.visitEnd();
	}

	/**
	 * @param mv
	 * @param i
	 */
	private static void push_int(MethodVisitor mv, int i) {
		if (i >= -1 && i <= 5) {
			mv.visitInsn(Opcodes.ICONST_0 + i);
		} else {
			mv.visitLdcInsn(new Integer(i));
		}
	}

	/**
	 * @param a
	 * @return
	 */
	public EObject apply(EProc proc, ESeq a) throws Pausable {
		// TODO: this should be implemented for all EFunX
		return invoke(proc, a.toArray());
	}
	
	public static void main(String[] args) {
		for (int i = 0; i < 10; i++) {
			get_fun_class(i);
		}
	}

}
