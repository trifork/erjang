package erjang.m.prim_file;

import java.util.ArrayList;
import java.util.List;

import erjang.BIF;
import erjang.EBinary;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;
import erjang.EString;

public class Native extends ENative
{
	@BIF
	public static EObject internal_name2native(EObject arg) {
		if (arg.testAtom() != null) {
			throw ERT.badarg(arg);
		}
		else if (arg.testBinary() != null) {
			EBinary bin = arg.testBinary();
			byte[] binbytes = bin.getByteArray();
			byte[] outbytes = new byte[binbytes.length + 1];
			
			System.arraycopy(binbytes, 0, outbytes, 0, binbytes.length);
			outbytes[outbytes.length - 1] = 0x0;
			
			return new EBinary(outbytes);
		}
		else {
			ESeq input = arg.testSeq();
			List<Byte> bytes = new ArrayList<Byte>();
			byte[] out;
			
			while (!input.isNil()) {
				Integer el = input.head().testSmall().intValue();
				bytes.add(el.byteValue());
				input = input.tail();
			}
			out = new byte[bytes.size() + 1];
			for (int i = 0; i < bytes.size(); ++i) {
				out[i] = bytes.get(i);
			}
			out[out.length - 1] = 0x0;
			
			return new EBinary(out);
		}
	}
	
	@BIF
	public static EObject internal_native2name(EObject arg) {
		EBinary bin = arg.testBinary();
		byte[] binbytes;
		byte[] outbytes;
		
		if (bin == null) {
			throw ERT.badarg(arg);
		}
		binbytes = bin.getByteArray();
		outbytes = new byte[binbytes.length - 1];
		System.arraycopy(binbytes, 0, outbytes, 0, binbytes.length - 1);
		String out = new String(outbytes);
		
		return new EString(out);
	}
}
