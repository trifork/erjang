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

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import kilim.Pausable;
import kilim.analysis.ClassInfo;
import kilim.analysis.ClassWeaver;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import erjang.beam.Compiler;
import erjang.beam.CompilerVisitor;
import erjang.beam.EUtil;
import erjang.m.ets.EMatchContext;
import erjang.m.ets.ETermPattern;

public abstract class EFun extends EObject implements Opcodes {

	public abstract int arity();

	@BIF
	public EAtom is_function(EObject arity) {
		ESmall ary;
		if ((ary=arity.testSmall()) != null) {
			return ERT.box( arity() == ary.value );
		}
		throw ERT.badarg(this, arity);
	}
	

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
		return CMP_ORDER_FUN;
	}
	
	/* (non-Javadoc)
	 * @see erjang.EObject#compare_same(erjang.EObject)
	 */
	@Override
	int compare_same(EObject rhs) {
		if (rhs == this) return 0;
		return (System.identityHashCode(this)&0xffff)
			 - (System.identityHashCode(rhs)&0xffff);
	}

	/** used for translation of tail recursive methods */
	public EObject go(EProc eproc) throws Pausable { return go2(eproc); }

	/** used for translation of tail recursive methods */
	public EObject go2(EProc eproc) {
		throw new java.lang.AbstractMethodError(this.getClass().getName() + "#go2(EProc)");
	}

	/** generic invoke, used only for apply */
	public abstract EObject invoke(EProc proc, EObject[] args) throws Pausable;

	/** pass-thru handler for interpreter */
	public EObject invoke(EProc proc, EObject[] args, int off, int len) 
		throws Pausable 
	{
		EObject[] new_args = new EObject[len];
		System.arraycopy(args, off, new_args, 0, len);
		return invoke(proc, new_args);
	}

	public boolean match(ETermPattern matcher, EMatchContext r) {
		return matcher.match(this, r);
	}


	/*==================== Code generation: ==================*/

	private static final Type EFUN_TYPE = Type.getType(EFun.class);
	private static final String EFUN_NAME = EFUN_TYPE.getInternalName();
	private static final Type EATOM_TYPE = Type.getType(EAtom.class);
	private static final Type EFUNHANDLER_TYPE = Type
			.getType(EFunHandler.class);
	private static final Type EOBJECT_TYPE = Type.getType(EObject.class);
	private static final Type STRING_TYPE = Type.getType(String.class);
	private static final Type EOBJECT_ARR_TYPE = Type.getType(EObject[].class);
	private static final Type EPROC_TYPE = Type.getType(EProc.class);
	static final String GO_DESC = "(" + EPROC_TYPE.getDescriptor() + ")"
			+ EOBJECT_TYPE.getDescriptor();
	private static final String EPROC_NAME = EPROC_TYPE.getInternalName();
	private static final String EOBJECT_DESC = EOBJECT_TYPE.getDescriptor();
	private static final String EATOM_DESC = EATOM_TYPE.getDescriptor();
	static final String[] PAUSABLE_EX = new String[] { Type.getType(Pausable.class).getInternalName() };


	private static final HashMap<Method, EFun> method_fun_map = new HashMap<Method, EFun>();
	private static final String ERJANG_MODULES_DOT = "erjang.m.";

	/* TODO: Using a central database like this to avoid duplicate
	 * definitions is a hack, perpetrated for the sake of moving
	 * interpreter progress along, and may interfere with module
	 * reloading.
	 *
	 * Treating native functions differently in EModule loading might
	 * be a better solution.
	 */
	public static synchronized EFun make(Method method, String module) {
		EFun fun = method_fun_map.get(method);
		if (fun==null) {
			method_fun_map.put(method, fun = do_make(method, module));
		}
		return fun;
	}

	private static EFun do_make(Method method, String module) {

		assert (Modifier.isStatic(method.getModifiers()));
		assert (!Modifier.isPrivate(method.getModifiers()));

		Class<?>[] parameterTypes = method.getParameterTypes();
		int ary = parameterTypes.length;
		boolean proc = (ary > 0 && parameterTypes[0].equals(EProc.class));
		if (proc)
			ary -= 1;
		String fname = erlangNameOfMethod(method);
		String mname = EUtil.getJavaName(EAtom.intern(fname), ary);

		Class<?> declaringClass = method.getDeclaringClass();
		Type type = Type.getType(declaringClass);
		byte[] data = CompilerVisitor.make_invoker(module, fname, type, mname, method
				.getName(), ary, proc, true, null, Type.getType(method.getReturnType()), true, true);

		ClassLoader cl = declaringClass.getClassLoader();

		// make sure we have its superclass loaded
		get_fun_class(ary);

		data = weave(data);

		String clname = type.getClassName() + "$FN_" + mname;
		clname = clname.replace('/', '.');
		Class<? extends EFun> res_class = ERT.defineClass(cl, clname, data);

		try {
			return res_class.newInstance();
		} catch (Exception e) {
			throw new Error(e);
		}
	}

	private static String erlangNameOfMethod(Method method) {
		BIF bif_ann = method.getAnnotation(BIF.class);
		if (bif_ann != null) {
			String bif_name = bif_ann.name().equals("__SELFNAME__") ? method.getName() : bif_ann.name();
			if (bif_ann.type() == BIF.Type.GUARD) return bif_name + "\1";
			else return bif_name;
		} else {
			return method.getName();
		}
	}


	/*==================== Code generation of EFun{arity}: ==================*/
	@SuppressWarnings("unchecked")
	static Class<? extends EFun> get_fun_class(int arity) {

		try {
			String className = EFUN_TYPE.getClassName() + arity;
			return (Class<? extends EFun>) Class.forName(className, true, EFun.class.getClassLoader());
		} catch (ClassNotFoundException ex) {
			// that's what we'll do here...
		}

		byte[] data = gen_fun_class_data(arity);
		
		data = weave(data);

		String self_type = EFUN_TYPE.getInternalName() + arity;
		self_type = self_type.replace('/', '.');
		return ERT.defineClass(EFun.class.getClassLoader(), self_type, data);
	}

	static byte[] gen_fun_class_data(int arity) {

		String self_type = EFUN_TYPE.getInternalName() + arity;

		ClassWriter cw = new ClassWriter(true);
		cw.visit(Opcodes.V1_5, Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT,
				self_type, null, EFUN_TYPE.getInternalName(), null);

		make_invoke_method(cw, self_type, arity);

		CompilerVisitor.make_invoketail_method(cw, self_type, arity, 0);

		MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "arity", "()I", null, null);
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
		mv.visitMaxs(1, 1);
		mv.visitEnd();

		make_cast_method(cw, arity);
		
		cw.visitEnd();

		byte[] data = cw.toByteArray();
		return data;
	}

	static Map<Integer, Constructor<? extends EFun>> handlers = new HashMap<Integer, Constructor<? extends EFun>>();

    public static EFun get_fun_with_handler(int arity, EFunHandler handler, ClassLoader loader) {
		Constructor<? extends EFun> h = handlers.get(arity);

		if (h == null) {

			get_fun_class(arity);
			
			String self_type = EFUN_TYPE.getInternalName() + "Handler" + arity;

			ClassWriter cw = new ClassWriter(true);
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

			/** forward toString to handler */
			mv = cw.visitMethod(ACC_PUBLIC, "toString", "()Ljava/lang/String;", null, null);
			mv.visitCode();
			mv.visitVarInsn(ALOAD, 0); // load self
			mv.visitFieldInsn(GETFIELD, self_type, "handler", EFUNHANDLER_TYPE.getDescriptor());
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "toString", "()Ljava/lang/String;");
			mv.visitInsn(ARETURN);
			mv.visitMaxs(3, 3);
			mv.visitEnd();
			
			// make invoke_tail method
			//CompilerVisitor.make_invoketail_method(cw, self_type, arity, 0);
			make_invoke_method(cw, self_type, arity);
			make_go_method(cw, self_type, arity);

			cw.visitEnd();
			byte[] data = cw.toByteArray();

			data = weave(data);

			Class<? extends EFun> clazz = ERT.defineClass(loader, self_type.replace('/', '.'), data);

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
	
	private static void make_cast_method(ClassWriter cw, int n) {
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
		mv = cw.visitMethod(ACC_PUBLIC, "go", GO_DESC, null, PAUSABLE_EX);
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
				.getSignature(arity, true), null, PAUSABLE_EX);
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
		if (EProc.TAIL_MARKER == null) {
			mv.visitJumpInsn(IFNONNULL, done);
		} else {
			mv.visitFieldInsn(GETSTATIC, EPROC_TYPE.getInternalName(),
					"TAIL_MARKER", EOBJECT_TYPE.getDescriptor());
			mv.visitJumpInsn(IF_ACMPNE, done);
		}
		
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
	/*^^^^^^^^^^^^^^^^^^^^ Code generation of EFun{arity}  ^^^^^^^^^^^^^^^^^^*/

	/*==================== Code generation of EFun{arity}Exported: ==========*/
	@SuppressWarnings("unchecked")
	static Class<? extends EFun> get_exported_fun_class(int arity) {

		String className = EFUN_TYPE.getClassName() + arity + "Exported";
		try {
			return (Class<? extends EFun>) Class.forName(className, true, EFun.class.getClassLoader());
		} catch (ClassNotFoundException ex) {
			// that's what we'll do here...
		}

		byte[] data = get_exported_fun_class_data(arity);

		data = weave(data);

		return ERT.defineClass(EFun.class.getClassLoader(), className, data);
	}

	static byte[] get_exported_fun_class_data(int arity) {
		/* Code template:
		 * public abstract class EFun{arity}Exported extends EFun{arity} {
		 *   protected final EAtom module_name, function_name;
		 *   protected EFun{arity}Exported(String m, String f) {
		 *     module_name   = EAtom.intern(m);
		 *     function_name = EAtom.intern(f);
		 *   }
		 * }
		 */

		ensure(arity); // Ensure presence of superclass.
		String super_type = EFUN_TYPE.getInternalName() + arity;
		String self_type = super_type + "Exported";

		ClassWriter cw = new ClassWriter(true);
		cw.visit(Opcodes.V1_5, Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT,
				self_type, null, super_type, null);

		cw.visitField(ACC_PROTECTED | ACC_FINAL, "module_name",
					  EATOM_TYPE.getDescriptor(), null, null)
			.visitEnd();

		cw.visitField(ACC_PROTECTED | ACC_FINAL, "function_name",
					  EATOM_TYPE.getDescriptor(), null, null)
			.visitEnd();

		MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PROTECTED, "<init>", "(Ljava/lang/String;Ljava/lang/String;)V", null, null);

		mv.visitVarInsn(Opcodes.ALOAD, 0);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, super_type, "<init>", "()V");

		mv.visitVarInsn(Opcodes.ALOAD, 0);
		mv.visitVarInsn(Opcodes.ALOAD, 1);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, EATOM_TYPE.getInternalName(),
						   "intern", "(Ljava/lang/String;)Lerjang/EAtom;");
		mv.visitFieldInsn(Opcodes.PUTFIELD, self_type, "module_name", EATOM_DESC);

		mv.visitVarInsn(Opcodes.ALOAD, 0);
		mv.visitVarInsn(Opcodes.ALOAD, 2);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, EATOM_TYPE.getInternalName(),
						   "intern", "(Ljava/lang/String;)Lerjang/EAtom;");
		mv.visitFieldInsn(Opcodes.PUTFIELD, self_type, "function_name", EATOM_DESC);

		mv.visitInsn(Opcodes.RETURN);
		mv.visitMaxs(3, 3);
		mv.visitEnd();

		make_encode_method_for_exported(cw, self_type, arity);

		cw.visitEnd();

		byte[] data = cw.toByteArray();
		return data;
	}


	static void make_encode_method_for_exported(ClassWriter cw, String className, int arity) {
		MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "encode", "("+ Type.getDescriptor(EOutputStream.class) +")V", null, null);
		mv.visitCode();

		mv.visitVarInsn(ALOAD, 1);
		mv.visitVarInsn(ALOAD, 0);
		mv.visitFieldInsn(GETFIELD, className, "module_name", EATOM_DESC);
		mv.visitVarInsn(ALOAD, 0);
		mv.visitFieldInsn(GETFIELD, className, "function_name", EATOM_DESC);
		mv.visitLdcInsn(new Integer(arity));

		mv.visitMethodInsn(INVOKEVIRTUAL, Type.getInternalName(EOutputStream.class), "write_external_fun",
						   "("+EATOM_DESC+EATOM_DESC+"I)V");

		mv.visitInsn(RETURN);
		mv.visitMaxs(4, 1);
		mv.visitEnd();
	}

	/*^^^^^^^^^^^^^^^^^^^^ Code generation of EFun{arity}Exported ^^^^^^^^^^*/

	/*==================== Code generation utilities:  ==================*/
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

	public static byte[] weave(byte[] data) {
		ClassWeaver w = new ClassWeaver(data, new Compiler.ErjangDetector("/xx/", (Set<String>)Collections.EMPTY_SET));
		for (ClassInfo ci : w.getClassInfos()) {
			// ETuple.dump(ci.className, ci.bytes);
			
			if (!ci.className.startsWith("kilim"))
				data = ci.bytes;
		}
		return data;
	}
	/*^^^^^^^^^^^^^^^^^^^^ Code generation utilities:  ^^^^^^^^^^^^^^^^^^*/

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
			get_exported_fun_class(i);
		}
	}

	/**
	 * @param arity
	 */
	public static void ensure(int arity) {
		get_fun_class(arity);
	}
	public static void ensure_exported(int arity) {
		get_exported_fun_class(arity);
	}

	/**
	 * @param eInputStream
	 * @return
	 * @throws IOException 
	 */
	public static EFun read(EInputStream eis) throws IOException {
		return eis.read_fun();
	}

	/**
	 * @param spec
	 * @return
	 */
	public EObject info(EAtom spec) {
		
		FunID id = get_id();
		
		if (spec == ERT.am_arity) {
			return new ETuple2(spec, ERT.box(id.arity - get_env().length()));
		} else if (spec == ERT.am_module) {
			return new ETuple2(spec, id.module);
		} else if (spec == ERT.am_name) {
			return new ETuple2(spec, id.function);
		} else if (spec == ERT.am_env) {
			return new ETuple2(spec, this.get_env());
		} else if (spec == ERT.am_type) {
			return new ETuple2(ERT.am_type, (id instanceof LocalFunID) ? ERT.am_local : ERT.am_external);
		}

		if (id instanceof LocalFunID) {			
			LocalFunID lid = (LocalFunID) id;
			if (spec == ERT.am_index) {
				return new ETuple2(spec, ERT.box(lid.index));
			} else if (spec == ERT.am_new_index) {
				return new ETuple2(spec, ERT.box(lid.new_index));
			} else if (spec == ERT.am_uniq) {
				return new ETuple2(spec, ERT.box(lid.uniq));
			} else if (spec == ERT.am_new_uniq) {
				return new ETuple2(spec, lid.new_uniq);
			} else if (spec == ERT.am_pid) {
				return new ETuple2(spec, this.get_pid());
			}			
		} else {
			if (spec == ERT.am_type) {
				return new ETuple2(ERT.am_type, ERT.am_external);			
			}
		}
			
		return ERT.am_undefined;
	}
	
	public boolean is_local() {
		return this.get_id() instanceof LocalFunID;
	}
	
	protected EObject get_pid() {
		return ERT.NIL;
	}

	protected ESeq get_env() {
		return ERT.NIL;
	}

	@Override
	public String toString() {
		FunID id = get_id();
		if (id instanceof LocalFunID) {
			LocalFunID lid = (LocalFunID) id;
			return "#Fun<" + id.module + "." + lid.index + "." + lid.uniq + ">";
		} else {
			return "#Fun<" + id.module + ":" + id.function + "/" + id.arity + ">";
		}
	}
	
	protected FunID get_id() {
		
		String cname = getClass().getName();
		EAtom module = null;
		if (cname.startsWith(ERJANG_MODULES_DOT)) {

			int last = cname.lastIndexOf('.');
			module = EAtom.intern(cname.substring(ERJANG_MODULES_DOT.length(),
					last));
		}
		
		EAtom fun = null;
		int end = cname.lastIndexOf("__");
		int start = cname.indexOf("$FN_");
		if (start != 1 && end != -1) {
			String method_name = cname.substring(start+4, end);
			fun = EAtom.intern(EUtil.decodeJavaName(method_name));
		}

		return new FunID(module, fun, arity());		
	}
	
}
