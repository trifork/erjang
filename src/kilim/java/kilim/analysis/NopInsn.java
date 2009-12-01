/* Copyright (c) 2006, Sriram Srinivasan
 *
 * You may distribute this software under the terms of the license 
 * specified in the file "License"
 */

package kilim.analysis;

import static org.objectweb.asm.Opcodes.NOP;

import java.util.Map;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.tree.AbstractInsnNode;

class NopInsn extends AbstractInsnNode {
    public NopInsn() {
        super(NOP);
    }

    @Override
	public int getType() {
        return 0;
    }

    @Override
    public void accept(MethodVisitor mv) {
        // Do nothing
    }

	/* (non-Javadoc)
	 * @see org.objectweb.asm.tree.AbstractInsnNode#clone(java.util.Map)
	 */
	@Override
	public AbstractInsnNode clone(Map labels) {
		// TODO Auto-generated method stub
		return this;
	}
}
