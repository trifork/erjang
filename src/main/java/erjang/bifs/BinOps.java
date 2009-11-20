package erjang.bifs;

import erjang.BIF;
import erjang.EInt32;
import erjang.ENumber;
import erjang.EObject;
import erjang.ERT;
import erjang.BIF.Type;

public class BinOps {

	@BIF(name="band",type=Type.GUARD)
	static public ENumber band_guard(EObject o1, EObject o2)
	{
		ENumber n1;
		ENumber n2;
		if ((n1=o1.testNumber()) != null) {
			if ((n2=o2.testNumber()) != null) {
				return n1.bitAnd(n2);
			}
		}
		
		return null;
	}

	
	@BIF
	static public ENumber band(EObject o1, EObject o2)
	{
		EInt32 i1, i2;
		if ((i1=o1.testInteger()) != null && (i2=o2.testInteger()) != null)
		{
			return new EInt32(i1.value & i2.value);
		}
		
		ENumber n1;
		ENumber n2;
		if ((n1=o1.testNumber()) != null) {
			if ((n2=o2.testNumber()) != null) {
				return n1.bitAnd(n2);
			}
		}
		
		throw ERT.badarg();
	}
	
	
	@BIF
	static public int bor(int n1, int n2)
	{
		return n1|n2;
	}
	

	
	@BIF
	static public ENumber bor(EObject o1, EObject o2)
	{
		EInt32 i1, i2;
		if ((i1=o1.testInteger()) != null && (i2=o2.testInteger()) != null)
		{
			return new EInt32(i1.value | i2.value);
		}
		
		ENumber n1;
		ENumber n2;
		if ((n1=o1.testNumber()) != null) {
			if ((n2=o2.testNumber()) != null) {
				return n1.bitOr(n2);
			}
		}
		
		throw ERT.badarg();
	}
	
	
	@BIF
	static public int band(int n1, int n2)
	{
		return n1&n2;
	}
	
	
	
	

	@BIF
	static public ENumber bsr(EObject o1, EObject o2)
	{
		EInt32 i1, i2;
		if ((i1=o1.testInteger()) != null && (i2=o2.testInteger()) != null)
		{
			return new EInt32(i1.value >> i2.value);
		}
		
		ENumber n1;
		ENumber n2;
		if ((n1=o1.testNumber()) != null) {
			if ((n2=o2.testNumber()) != null) {
				return n1.bitShiftRight(n2);
			}
		}
		
		throw ERT.badarg();
	}
	
	@BIF
	static public ENumber bsl(EObject o1, EObject o2)
	{
		EInt32 i1, i2;
		if ((i1=o1.testInteger()) != null && (i2=o2.testInteger()) != null)
		{
			return new EInt32(i1.value << i2.value);
		}
		
		ENumber n1;
		ENumber n2;
		if ((n1=o1.testNumber()) != null) {
			if ((n2=o2.testNumber()) != null) {
				return n1.bitShiftLeft(n2);
			}
		}
		
		throw ERT.badarg();
	}
	
	
	
}
