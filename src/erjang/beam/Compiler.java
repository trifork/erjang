package erjang.beam;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Pattern;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodAdapter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import erjang.EModule;
import erjang.EObject;
import erjang.ErlFun;
import erjang.Module;
import erjang.beam.analysis.BeamTypeAnalysis;

public class Compiler implements Opcodes {

	private static final Type EMODULE_TYPE = Type.getType(EModule.class);
	private static final Type EOBJECT_TYPE = Type.getType(EObject.class);
	private static final String EOBJECT_DESC = EOBJECT_TYPE.getDescriptor();

	private static final Type MODULE_ANN_TYPE = Type.getType(Module.class);
	private static final Type ERLFUN_ANN_TYPE = Type.getType(ErlFun.class);

	public static void main(String[] args) throws Exception {

		Compiler cc = new Compiler(new File("erl/test1.beam"));
		cc.writeTo(new File("erl/test1.jbeam"));
		
	}

	private BeamTypeAnalysis analysis;
	private BeamFile loaded;
	private Type self_type;
	private byte[] class_data;

	public Compiler(File input) throws Exception {
		ErlangBeamDisLoader foo = new ErlangBeamDisLoader();
		loaded = foo.load(input);
		loaded.accept(analysis = new BeamTypeAnalysis());

		class_data = compile();
	}
	
	void writeTo(File output) throws IOException {

		FileOutputStream fo = new FileOutputStream(output);
		fo.write(class_data);
		fo.close();
		
	}

	private byte[] compile() {

		this.self_type = Type.getObjectType(getInternalClassName());

		ClassWriter cw___ = new ClassWriter(ClassWriter.COMPUTE_MAXS);

		ClassVisitor cv = cw___;

		cv.visit(V1_5, ACC_PUBLIC, self_type.getInternalName(), null,
				EMODULE_TYPE.getInternalName(), null);

		add_module_annotation(cv);

		for (BeamFunction f : analysis.functions()) {
			compile_function(cv, f);
		}

		cw___.visitEnd();
		return cw___.toByteArray();
	}

	private void compile_function(ClassVisitor cv, BeamFunction f) {

		String name = getJavaName(f.getName(), f.getArity());
		EMethodAdapter ma = new EMethodAdapter(cv.visitMethod(ACC_STATIC
				| ACC_PUBLIC, name, getSignature(f.getArity()), null,
				getExceptions(f)), f);

		add_erlfun_annotation(ma, f);

		ma.compile_code();

		ma.visitEnd();
	}

	private void add_erlfun_annotation(EMethodAdapter ma, BeamFunction f) {

		AnnotationVisitor ann = ma.visitAnnotation(ERLFUN_ANN_TYPE
				.getDescriptor(), true);
		ann.visitAnnotation("module", f.getModuleName());
		ann.visitAnnotation("name", f.getName());
		ann.visitAnnotation("arity", String.valueOf(f.getArity()));
		ann.visitAnnotation("export", String.valueOf(f.isExported()));
		ann.visitEnd();

	}

	/** for now, erlang functions have no exceptions */
	private String[] getExceptions(BeamFunction f) {
		return null;
	}

	Map<Integer, String> signatures = new HashMap<Integer, String>();

	private String getSignature(int arity) {
		String res = signatures.get(arity);
		if (res == null) {
			StringBuffer sb = new StringBuffer("(");

			for (int i = 0; i < arity; i++) {
				sb.append(EOBJECT_DESC);
			}

			sb.append(")");
			sb.append(EOBJECT_DESC);

			signatures.put(arity, res = sb.toString());
		}

		return res;
	}

	private String getJavaName(String fun, int arity) {
		return toJavaIdentifier(fun + "/" + arity);
	}

	private void add_module_annotation(ClassVisitor cv) {

		AnnotationVisitor av = cv.visitAnnotation(MODULE_ANN_TYPE
				.getInternalName(), true);
		av.visitAnnotation("value", getModuleName());

		av.visitEnd();
	}

	private String getModuleName() {
		return analysis.getModuleName();
	}

	private String getInternalClassName() {
		String cn = toJavaIdentifier(getModuleName());
		return "erjang/modules/" + cn;
	}

	static final Pattern SIMPLE_ID = Pattern
			.compile("^([a-z]|[A-Z])\\p{Alnum}*$");

	/** encode any char sequence into a valid java identifier */
	private static String toJavaIdentifier(String name) {
		if (SIMPLE_ID.matcher(name).matches())
			return name;

		StringBuilder sb = new StringBuilder();
		for (char c : name.toCharArray()) {

			if (c == '$') {
				sb.append("$$");
				continue;
			} else if (sb.length() == 0) {
				if (Character.isJavaIdentifierStart(c)) {
					sb.append(c);
					continue;
				}
			} else if (Character.isJavaIdentifierPart(c)) {
				sb.append(c);
				continue;
			} 

			try {
				sb.append('$');

				ByteArrayOutputStream baro = new ByteArrayOutputStream(5);
				DataOutputStream dao = new DataOutputStream(baro);
				dao.writeUTF(new String(new char[] { c }));
				dao.close();

				byte[] data = baro.toByteArray();

				writeHexByte(sb, 0xff & data[2]);

				if (data.length > 3)
					writeHexByte(sb, 0xff & data[3]);

				if (data.length > 4)
					writeHexByte(sb, 0xff & data[4]);

			} catch (IOException ex) {
				throw new Error();
			}

		}

		return sb.toString();
	}

	private static void writeHexByte(StringBuilder sb, int b) {
		if (b < 0x10) {
			sb.append('0');
		}

		sb.append(Integer.toHexString(b).toUpperCase());
	}

	class EMethodAdapter extends MethodAdapter {

		int[] xregs, yregs, fregs;
		private BeamFunction fun;
		private BeamCodeBlock[] blocks;
		private Map<Integer, Label> labels = new TreeMap<Integer, Label>();

		public EMethodAdapter(MethodVisitor mv, BeamFunction f) {
			super(mv);
			this.fun = f;
		}

		/** */
		public void compile_code() {
			visitCode();

			allocate_regs_to_locals();

			blocks = fun.getCodeBlocks();

			// make some labels
			for (BeamCodeBlock block : blocks) {
				labels.put(block.getLabel(), new Label());
			}

			// goto first instruction
			int start = fun.getEntryLabel();
			visitJumpInsn(GOTO, labels.get(start));

			for (BeamCodeBlock block : blocks) {

				if (block.isDeadCode())
					continue;

				// now we are here, trada!
				Label block_label = labels.get(block.getLabel());
				visitLabel(block_label);

				BeamInstruction[] insns = block.getInstructions();
				for (int i = 0; i < insns.length; i++) {
					compile_insn(insns[i]);
				}
				
			}

		}

		private void compile_insn(BeamInstruction insn) {
			// TODO Auto-generated method stub
				
			// .. fill in here and we are done!
			
			switch (insn.opcode()) {
			case gc_bif:
				
				
			default: 
				throw new Error("unhandled: "+insn);
			}
		}

		private void allocate_regs_to_locals() {

			int max_x = fun.getXregCount();
			int max_y = fun.getYregCount();
			int max_f = fun.getFregCount();

			int local = 0;

			xregs = new int[max_x];
			for (int i = 0; i < max_x; i++) {
				xregs[i] = local;
				local += 1;
			}

			yregs = new int[max_y];
			for (int i = 0; i < max_y; i++) {
				xregs[i] = local;
				local += 1;
			}

			fregs = new int[max_f];
			for (int i = 0; i < max_x; i++) {
				xregs[i] = local;
				local += 2;
			}

		}

	}

}
