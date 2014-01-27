package erjang.m.io_lib;

import erjang.BIF;
import erjang.EBitString;
import erjang.EInteger;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;
import erjang.EString;
import erjang.ESeq;
import erjang.ESmall;

public class Native extends ENative {

    /** Emulate io_lib:write_binary_body(), but without using deep stack. */
    @BIF
    static public EObject write_binary_body(EObject arg1, EObject arg2)
    {
        EBitString bin = arg1.testBitString();
        EInteger d = arg2.testInteger();
        
        if (bin == null || d == null) 
            throw ERT.badarg(arg1, arg2);
        
        int depth = d.intValue();
        if (depth < 0) depth=Integer.MAX_VALUE;
        
        ESeq builder = ERT.NIL;
        long bitCount = bin.bitSize();
        if (bitCount > 8*(depth-1)) {
            // Replace tail with ellipsis:
            builder = builder.cons(EString.fromString("..."));
        } else if (bitCount % 8 > 0) {
            // Handle tail bits
            int tailBitCount = (int)(bitCount & 7);
            int tailBits = bin.intBitsAt(bitCount & (~7), tailBitCount);
            builder = builder.cons(EString.fromString(String.valueOf(tailBitCount)));
            builder = builder.cons(ESmall.make(':'));
            builder = builder.cons(EString.fromString(String.valueOf(tailBits)));
        }
        // Handle whole bytes:
        for (long bitPos=8*(Math.min(bitCount/8, depth-1)-1);
             bitPos>=0;
             bitPos-=8)
        {
            if (bitPos < bitCount-8) {
                builder = builder.cons(ESmall.make(','));
            }
            String byteAsIntString = String.valueOf(bin.intBitsAt(bitPos, 8));
            builder = builder.cons(EString.fromString(byteAsIntString));
        }
        return builder;
    }
}
