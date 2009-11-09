package org.erlang.jbeam;

import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.erlang.EModule;
import org.erlang.jbeam.ops.AllocateZero;
import org.erlang.jbeam.ops.AttributesDecl;
import org.erlang.jbeam.ops.BasicBlock;
import org.erlang.jbeam.ops.Call;
import org.erlang.jbeam.ops.CallExtOnly;
import org.erlang.jbeam.ops.CodeAdapter;
import org.erlang.jbeam.ops.CompilationInfo;
import org.erlang.jbeam.ops.Deallocate;
import org.erlang.jbeam.ops.ExportsDecl;
import org.erlang.jbeam.ops.ExternalFunction;
import org.erlang.jbeam.ops.FileDecl;
import org.erlang.jbeam.ops.FuncInfo;
import org.erlang.jbeam.ops.FunctionAdapter;
import org.erlang.jbeam.ops.FunctionDecl;
import org.erlang.jbeam.ops.GCBif;
import org.erlang.jbeam.ops.Insn;
import org.erlang.jbeam.ops.ModuleDecl;
import org.erlang.jbeam.ops.Move;
import org.erlang.jbeam.ops.PutInsn;
import org.erlang.jbeam.ops.PutToupleInsn;
import org.erlang.jbeam.ops.Register;
import org.erlang.jbeam.ops.RegisterX;
import org.erlang.jbeam.ops.RegisterY;
import org.erlang.jbeam.ops.Return;
import org.erlang.jbeam.ops.Test;
import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Type;

import com.sun.xml.internal.ws.org.objectweb.asm.Opcodes;

import org.erlang.EAtom;
import org.erlang.ECons;
import org.erlang.EFun;
import org.erlang.EFunRef;
import org.erlang.EInteger;
import org.erlang.EList;
import org.erlang.EObject;
import org.erlang.ESeq;
import org.erlang.EString;
import org.erlang.ETerm;
import org.erlang.ETuple;

public class BEAMFile {

	private static final Type MODULE_TYPE = Type.getType(EModule.class);

	private static final Type ETERM_TYPE = Type.getType(ETerm.class);

	private static final Type EFUN_TYPE = Type.getType(EFun.class);
	private static final Type EFUNREF_TYPE = Type.getType(EFunRef.class);

	private static final Type ETUPLE_TYPE = Type.getType(ETuple.class);

	private static final Type EMODULE_TYPE = Type.getType(EModule.class);;

	EAtom RETURN = EAtom.intern("return");

	private FileDecl file;
	private ModuleDecl module;
	private ExportsDecl exports;
	private AttributesDecl attributes;
	private CompilationInfo comp_info;
	private List<FunctionDecl> funs = new ArrayList<FunctionDecl>();

	private boolean trans_is_tail = false;
	
	void init(List<ETerm> vec) throws Exception {

		FunctionDecl curr_fun = null;
		BasicBlock curr_bb = null;

		
		for (int i = 0; i < vec.size(); i++) {
			switch (lookahead(vec, i)) {
			case file:
				file = (FileDecl) decode(vec, i);
				continue;

			case module:
				module = (ModuleDecl) decode(vec, i);
				continue;

			case exports:
				exports = (ExportsDecl) decode(vec, i);
				continue;

			case attributes:
				attributes = (AttributesDecl) decode(vec, i);
				continue;

			case comp_info:
				comp_info = (CompilationInfo) decode(vec, i);
				continue; 
				

			case function:
				if (curr_fun != null) {
					curr_fun.set_is_tail_recursive(trans_is_tail);
					curr_fun.finish();
				}
				curr_fun = (FunctionDecl) decode(vec, i);
				funs.add(curr_fun);
				trans_is_tail = false;
				continue;

			case label:

				int num = integer((ETuple) vec.get(i), 2);
				curr_bb = new BasicBlock(curr_fun, num);
				continue;

			case call_only:
			case call_ext_only:
				trans_is_tail = true;
				
			default:

				curr_bb.append((Insn) decode(vec, i));

			}
		}

		curr_fun.set_is_tail_recursive(trans_is_tail);
		curr_fun.finish();

		// BEAM LOADED

		ClassWriter cfw = new ClassWriter(ClassWriter.COMPUTE_MAXS
				| ClassWriter.COMPUTE_FRAMES);

		registerConst(attributes.list);
		
		emit(cfw);

		byte[] b = cfw.toByteArray();

		FileOutputStream fo = new FileOutputStream(module.getName() + ".class");
		fo.write(b);
		fo.close();
	}
	
	boolean constpool_fronzen = false;

	void emit(ClassVisitor cv) {
		ClassAdapter ca = new ClassAdapter(cv);

		ca.visit(Opcodes.V1_4, Opcodes.ACC_PUBLIC, getInternalClassName(),
				null, MODULE_TYPE.getInternalName(), null);

		ca.visitSource(this.file.getFileString(), null);
		
		for (FunctionDecl fd : funs) {
			fd.emit(this, ca);
		}

		MethodVisitor xx = ca.visitMethod(Opcodes.ACC_STATIC
				| Opcodes.ACC_PRIVATE, "<clinit>", "()V", null, null);
		CodeAdapter init = new CodeAdapter(this, xx);
		init.visitCode();

		
		for (Map.Entry<ETuple,String> imp : imports.entrySet())
		{
			registerConst(imp.getKey());
		}
		
		constpool_fronzen = true;
		
		for (Map.Entry<ETerm, String> e : consts.entrySet()) {

			String name = e.getValue();
			ETerm term = e.getKey();

			Type type = term.emit_const(init);
			
			if (type == null) throw new Error(""+term.toString());
			
			init.visitFieldInsn(Opcodes.PUTSTATIC, getInternalClassName(),
					name, 
					type.getDescriptor());

			ca.visitField(Opcodes.ACC_STATIC, name, type.getDescriptor(), null,
					null);

		}

		for (Map.Entry<ETuple,String> imp : imports.entrySet())
		{
			String name = imp.getValue();
			ETuple fun = imp.getKey();
			
			ca.visitField(Opcodes.ACC_STATIC, name, EFUNREF_TYPE.getDescriptor(), null, null);
			
			Type t = emit_const(init, fun);
			init.visitMethodInsn(Opcodes.INVOKESTATIC, EMODULE_TYPE.getInternalName(), "import", 
					"("+ETUPLE_TYPE.getDescriptor()+")"+EFUNREF_TYPE.getDescriptor());
			init.visitFieldInsn(Opcodes.PUTSTATIC, getInternalClassName(), name, EFUNREF_TYPE.getDescriptor());
			
		}
		
		init.visitInsn(Opcodes.RETURN);
		init.visitEnd();

		ca.visitEnd();
	}

	public Type emit_const(CodeAdapter ca, EObject head) {

		if (!constpool_fronzen && !consts.containsKey(head)) {
			registerConst(head);
		}

		
		if (consts.containsKey(head)) {
			Type t = Type.getType(head.getClass());
			
			ca.visitFieldInsn(Opcodes.GETSTATIC, getInternalClassName(), consts
					.get(head), t.getDescriptor());
			return t;
		} else {
			
			return head.emit_const(ca);
		}
	}

	public String getInternalClassName() {
		return getModuleClassName(module.getName());
	}

	private BeamFileTag lookahead(List<ETerm> vec, int i) {
		Object nth = vec.get(i);
		if (nth instanceof EAtom) {
			return sym2enum((EAtom) nth);
		} else {
			ETuple stmt = (ETuple) nth;
			return sym2enum((EAtom) stmt.nth(1));
		}
	}

	private Object decode(List<ETerm> vec, int i) throws Exception {
		Object nth = vec.get(i);
		return decode_one(nth);
	}

	private Object decode(ETuple vec, int i) throws Exception {
		Object nth = vec.nth(i);
		return decode_one(nth);
	}

	private Object decode_one(Object nth) throws Exception {
		if (nth instanceof ETuple) {
			ETuple rec = (ETuple) nth;
			return decode_record(rec);
		} else if (nth.equals(RETURN)) {
			return new Return();
		} else {
			System.err.println("not vector: " + nth);
			return null;
		}
	}

	int tuple_idx;

	private Object decode_record(ETuple stmt) throws Exception {

		BeamFileTag opcode = sym2enum((EAtom) stmt.nth(1));
		switch (opcode) {

		case file:
			return new FileDecl(string(stmt, 2));

		case module:
			return new ModuleDecl(atom(stmt, 2));

		case exports:
			return new ExportsDecl(list(stmt, 2));

		case attributes:
			return new AttributesDecl(list(stmt, 2));

		case comp_info:
			return new CompilationInfo(list(stmt, 2));

		case function:
			return new FunctionDecl(this, atom(stmt, 2), integer(stmt, 3),
					integer(stmt, 4));

		case func_info:
			return new FuncInfo((EAtom) decode(stmt, 2),
					(EAtom) decode(stmt, 3), integer(stmt, 4));

		case move:
			return new Move(decode(stmt, 2), reg(stmt, 3));

		case x:
			return (Register) new RegisterX(integer(stmt, 2));

		case y:
			return (Register) new RegisterY(integer(stmt, 2));

		case integer:
			return (EInteger) stmt.nth(2);

		case atom:
			return (EAtom) stmt.nth(2);

		case test:
			return (Insn) new Test(atom(stmt, 2), (Label) decode(stmt, 3),
					(Object[]) decode_list(stmt, 4));

		case f:
			return get_label(integer(stmt, 2));

		case allocate_zero:
			return new AllocateZero(integer(stmt, 2), integer(stmt, 3));

		case gc_bif: {
			EAtom name = atom(stmt, 2);
			Label fail = (Label) decode(stmt, 3);
			int hmm = integer(stmt, 4);
			Object[] args = decode_list(stmt, 5);
			Register dest = (Register) decode(stmt, 6);

			return new GCBif(name, fail, hmm, args, dest);
		}

		case call: {
			int nargs = integer(stmt, 2);
			ETuple vec = (ETuple) stmt.nth(3);
			EAtom mod = (EAtom) vec.nth(1);
			EAtom fun = (EAtom) vec.nth(2);
			EInteger ari = (EInteger) vec.nth(3);
			return new Call(nargs, mod, fun, ari.intValue(), false);
		}

		case call_only: {
			int nargs = integer(stmt, 2);
			ETuple vec = (ETuple) stmt.nth(3);
			EAtom mod = (EAtom) vec.nth(1);
			EAtom fun = (EAtom) vec.nth(2);
			EInteger ari = (EInteger) vec.nth(3);
			return new Call(nargs, mod, fun, ari.intValue(), true);
		}

		case deallocate:
			return new Deallocate(integer(stmt, 2));

		case call_ext_only:
			return new CallExtOnly(integer(stmt, 2), (ExternalFunction) decode(
					stmt, 3), true);

		case call_ext:
			return new CallExtOnly(integer(stmt, 2), (ExternalFunction) decode(
					stmt, 3), false);

		case extfunc:
			return new ExternalFunction(atom(stmt, 2), atom(stmt, 3), integer(
					stmt, 4));

		case put_tuple:
			tuple_idx = 0;
			return new PutToupleInsn(integer(stmt, 2), reg(stmt, 4));

		case put:
			return new PutInsn(tuple_idx++, decode(stmt, 2));

		default:
			throw new Exception("unhandled: " + opcode);
		}

	}

	private Object[] decode_list(ETuple stmt, int i) throws Exception {
		ESeq v = (ESeq) stmt.nth(i);

		Object[] result = new Object[v.count()];
		int pos = 0;
		while (v != ECons.EMPTY) {
			result[pos++] = decode_record((ETuple) v.head());
			v = v.tail();
		}
		return result;
	}

	private EList list(ETuple stmt, int i) {
		return (EList) stmt.nth(i);
	}

	private EString string(ETuple stmt, int i) {
		return (EString) stmt.nth(i);
	}

	private EAtom atom(ETuple stmt, int i) {
		return (EAtom) stmt.nth(i);
	}

	private Register reg(ETuple stmt, int i) throws Exception {
		return (Register) decode(stmt, i);
	}

	private int integer(ETuple stmt, int i) {
		return ((EInteger) stmt.nth(i)).intValue();
	}

	BeamFileTag sym2enum(EAtom sym) {
		String name = sym.getName();
		if ("return".equals(name)) {
			name = "K_return";
		}
		if ("case".equals(name)) {
			name = "K_case";
		}
		return BeamFileTag.valueOf(name);
	}

	Map<Integer, Label> labels = new HashMap<Integer, Label>();

	public Label get_label(int key) {
		if (key == 0)
			return null;
		Label res = labels.get(key);
		if (res == null) {
			labels.put(key, res = new Label());
		}
		return res;
	}

	public String getModuleClassName(String module) {
		return "org/erlang/modules/" + module + "/" + module;
	}

	Map<ETerm, String> consts = new HashMap<ETerm, String>();

	public String registerConst(ETerm value) {
		String str = consts.get(value);
		if (str == null) {
			str = "cp$" + consts.size();
			consts.put(value, str);
		}
		
		if (value != ECons.EMPTY && value instanceof ECons) {
			registerConst(((ECons)value).head());
			registerConst(((ECons)value).tail());
		} else if (value instanceof ETuple) {
			ETuple t = (ETuple) value;
			for (int i = 1; i <= t.arity(); i++) {
				registerConst(t.nth(i));
			}
		}
		
		return str;
	}

	Map<ETuple,String> imports = new HashMap<ETuple, String>();
	static String DEREF_DESCRIPTOR = "()" + EFUN_TYPE.getDescriptor();
	
	public String deref_external_fun(CodeAdapter ca, ExternalFunction fun) {
		
		ETuple id = fun.getID();
		String var = imports.get(id);
		if (var == null) {
			imports.put(id, var = "import$"+fun.mod.getName()+"$"+fun.name.getName()+"$"+fun.arity);
		}

		ca.visitFieldInsn(Opcodes.GETSTATIC, getInternalClassName(), var, EFUNREF_TYPE.getDescriptor());
		ca.visitMethodInsn(Opcodes.INVOKEVIRTUAL, EFUNREF_TYPE.getInternalName(), "deref", DEREF_DESCRIPTOR);
		
		StringBuffer sig = new StringBuffer("(");
		
		for (int i = 0; i < fun.arity; i++) {
			sig.append(ETERM_TYPE.getDescriptor());
		}
		
		sig.append(")");
		sig.append(ETERM_TYPE.getDescriptor());
		String desc = sig.toString();

		return desc;
	}

}
