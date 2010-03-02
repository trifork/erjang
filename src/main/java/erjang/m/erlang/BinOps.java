package erjang.m.erlang;

import kilim.Pausable;
import erjang.BIF;
import erjang.EAtom;
import erjang.EInteger;
import erjang.ENumber;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.ESmall;
import erjang.BIF.Type;

public class BinOps {

	@BIF(name="band",type=Type.GUARD)
	static public ENumber band_guard(EObject o1, EObject o2)
	{
		EInteger n1;
		EInteger n2;
		if ((n1=o1.testInteger()) != null) {
			if ((n2=o2.testInteger()) != null) {
				return n1.band(n2);
			}
		}
		
		return null;
	}

	
	@BIF
	static public EInteger band(EObject o1, EObject o2)
	{
		return o1.band(o2);
	}
	
	
	
	@BIF
	static public EInteger bor(EObject o1, EObject o2)
	{
		return o1.bor(o2);
	}
	

	@BIF
	static public EInteger bsr(EObject o1, EObject o2)
	{
		return o1.bsr(o2);
	}
	
	@BIF
	static public EInteger bsl(EObject o1, EObject o2)
	{
		return o1.bsl(o2);
	}
	
	@BIF
	static public EInteger bsl(EInteger o1, ESmall o2)
	{	
		return o1.bsl(o2);
	}
	
	@BIF(name=">=")
	static public EAtom ge(EObject o1, EObject o2) {
		return o1.ge(o2);
	}
	
	@BIF
	static public EObject send(EProc proc, EObject pid, EObject msg) throws Pausable {
		return ERT.send(proc, pid, msg);
	}
	
}
