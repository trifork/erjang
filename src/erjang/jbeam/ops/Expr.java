package erjang.jbeam.ops;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public abstract class Expr implements Opcodes {
	abstract Type emit_push(FunctionAdapter ma);
}
