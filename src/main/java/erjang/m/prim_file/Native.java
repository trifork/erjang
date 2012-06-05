package erjang.m.prim_file;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import erjang.BIF;
import erjang.CharCollector;
import erjang.EBinary;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.EString;
import erjang.driver.IO;

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
			/*
			try {
				StringBuilder output = new StringBuilder();
				CharCollector out = new CharCollector(IO.ISO_LATIN_1, output);
				arg.collectCharList(out);
				out.addInteger(0);
				out.end();
				return new EBinary(output.toString().getBytes(IO.ISO_LATIN_1));
			} catch (Exception e) {
				throw ERT.badarg();
			}
			*/
			ESeq input = arg.testSeq();
			List<Byte> bytes = new ArrayList<Byte>();
			byte[] out;
			
			ESeq all = input;
			
			while (!input.isNil()) {
				EObject first = input.head();
				ESmall small = first.testSmall();
				if (small == null) {
					EBinary bin = first.testBinary();
					if (bin == null) {
						// it doesn't look like an iolist to me...
						throw ERT.badarg();
					} else {
						for (int i = 0; i < bin.byteSize(); i++) {
							bytes.add(bin.byteAt(i));
						}
						input = input.tail();
						continue;
					}
				}
				Integer el = small.intValue();
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
		
		if (bin == null) {
			throw ERT.badarg(arg);
		}
		binbytes = bin.getByteArray();
		String out = new String(binbytes);
		
		return new EString(out);
	}
}
