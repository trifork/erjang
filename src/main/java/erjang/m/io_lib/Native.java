package erjang.m.io_lib;

import erjang.BIF;
import erjang.EBitString;
import erjang.EInteger;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;
import erjang.EString;

public class Native extends ENative {

	@BIF
	static public EObject write_binary(EObject arg1, EObject arg2)
	{
		EBitString bin = arg1.testBitString();
		EInteger d = arg2.testInteger();
		
		if (bin == null || d == null) 
			throw ERT.badarg(arg1, arg2);
		
		int depth = d.intValue();
		
	    StringBuilder sb = new StringBuilder("<<");
	    long remainingBits = bin.bitSize();
	    long bitPos = 0L;
	    while(remainingBits != 0) {
	        
	        if (depth == 1) {
	        	sb.append("..."); 
	        	break; 
	        }

	        if (remainingBits > 8) {
		           int b = bin.intBitsAt( bitPos, 8 );
		           sb.append( b ).append(',');
		           bitPos += 8;
		           remainingBits -= 8;
		           depth = depth-1;
		           continue;
	        }

	        if (remainingBits == 8) {
	           int b = bin.intBitsAt(bitPos, 8);
	           sb.append( b );
	           break;
	        }

	        int b = bin.intBitsAt(bitPos, (int) remainingBits);
	        sb.append( b ).append(':').append(remainingBits);
	        break;
	     }
	    
	    sb.append(">>");
	    
	    return new EString ( sb.toString() );
	}
}
