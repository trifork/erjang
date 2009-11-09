package org.erlang;

import org.erlang.jbeam.ops.CodeAdapter;
import org.objectweb.asm.Type;

public abstract class ETerm extends EObject {

	public abstract Type emit_const(CodeAdapter fa);

	public int asInt() {
		return ((EInteger)this).intValue();
	}

}
