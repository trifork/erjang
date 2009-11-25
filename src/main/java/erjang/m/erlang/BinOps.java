package erjang.m.erlang;

import erjang.BIF;
import erjang.EInteger;
import erjang.ENumber;
import erjang.EObject;
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
	static public ENumber band(EObject o1, EObject o2)
	{
		return o1.band(o2);
	}
	
	
	
	@BIF
	static public ENumber bor(EObject o1, EObject o2)
	{
		return o1.bor(o2);
	}
	

	@BIF
	static public ENumber bsr(EObject o1, EObject o2)
	{
		return o1.bsr(o2);
	}
	
	@BIF
	static public ENumber bsl(EObject o1, EObject o2)
	{
		return o1.bsl(o2);
	}
	
	@BIF(name=">=")
	static public boolean ge(EObject o1, EObject o2) {
		return o1.ge(o2);
	}
	
}
