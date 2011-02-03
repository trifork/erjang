package erjang.m.rpc;

import kilim.Mailbox;
import kilim.Pausable;
import erjang.EObject;
import erjang.EPseudoTerm;

public class MBox extends EPseudoTerm {

	Mailbox<EObject> mbox;
	
	public MBox() {
		this.mbox = new Mailbox<EObject>();
	}
	
	public void put(EObject value) throws Pausable {
		mbox.put(value);
	}

	public EObject get_b() {
		return mbox.getb();
	}

	public EObject get_b(long timeout) {
		return mbox.getb(timeout);
	}

	@Override
	public int hashCode() {
		return System.identityHashCode(this);
	}
}
