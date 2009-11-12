package erjang.jbeam.ops;

import org.objectweb.asm.MethodAdapter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Type;

import erjang.EObject;
import erjang.ETerm;
import erjang.jbeam.BEAMFile;

public class CodeAdapter extends MethodAdapter {

	private final BEAMFile beam;


	public CodeAdapter(BEAMFile beam, MethodVisitor mv) {
		super(mv);
		this.beam = beam;
	}


	public Type emit_const(EObject head) {
		return beam.emit_const(this, head);
	}


}
