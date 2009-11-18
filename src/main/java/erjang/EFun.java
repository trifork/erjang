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

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import erjang.beam.CompilerVisitor;
import erjang.beam.EUtil;

public abstract class EFun {

	/** used for translation of tail recursive methods */
	protected abstract EObject go(EProc eproc);

	/** generic invoke, used only for apply */
	public abstract EObject invoke(EProc proc, EObject[] args);

	private static final Type EFUN_TYPE = Type.getType(EFun.class);
	private static final Type EOBJECT_TYPE = Type.getType(EObject.class);
	private static final Type EOBJECT_ARR_TYPE = Type.getType(EObject[].class);
	private static final Type EPROC_TYPE = Type.getType(EProc.class);

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

		Class<?> declaringClass = method.getDeclaringClass();
		Type type = Type.getType(declaringClass);
		byte[] data = CompilerVisitor.make_invoker(type, method.getName(), ary,
				proc, 0);

		String clname = type.getClassName() + "$" + method.getName();

		ClassLoader cl = declaringClass.getClassLoader();

		// make sure we have it's superclass loaded
		get_fun_class(ary);
		
		Class<? extends EFun> res_class = ERT.defineClass(cl, clname, data, 0,
				data.length);

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
			return (Class<? extends EFun>) Class.forName(EFUN_TYPE.getClassName() + arity, true,
					EFun.class.getClassLoader());
		} catch (ClassNotFoundException ex) {
			// that's what we'll do here...
		}

		ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
		cw.visit(Opcodes.V1_5, Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT,
				self_type, null, EFUN_TYPE.getInternalName(), null);

		MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC
				| Opcodes.ACC_ABSTRACT, "invoke", EUtil.getSignature(arity,
				true), null, null);
		mv.visitEnd();

		mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT,
				"invoke_tail", EUtil.getSignature(arity, true), null, null);
		mv.visitEnd();

		mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "invoke", "("
				+ EPROC_TYPE.getDescriptor() + EOBJECT_ARR_TYPE.getDescriptor()
				+ ")" + EOBJECT_TYPE.getDescriptor(), null, null);
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

		cw.visitEnd();

		byte[] data = cw.toByteArray();
		return ERT.defineClass(EFun.class.getClassLoader(), self_type, data, 0,
				data.length);
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

}
