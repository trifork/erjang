package erjang.m.prim_file;

import java.io.UnsupportedEncodingException;

import erjang.BIF;
import erjang.EBinary;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;

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
			try
			{
				String in = arg.toString();
				byte[] bytes;
				byte[] outbytes;
				
				bytes = in.substring(1, in.length() - 1).getBytes(System.getProperty("file.encoding"));
				outbytes = new byte[bytes.length + 1];
				System.arraycopy(bytes, 0, outbytes, 0, bytes.length);
				outbytes[outbytes.length - 1] = 0x0;
				
				return new EBinary(outbytes);
			}
			catch (UnsupportedEncodingException e)
			{
				throw ERT.notsup();
			}
		}
	}
	
	@BIF
	public static EObject internal_native2name(EObject arg) {
		if (arg.testBinary() == null) {
			throw ERT.badarg(arg);
		}
		System.err.println("native2name " + arg.getClass());
		
		return null;
	}
}
