package erjang;

import java.io.File;

import erjang.m.erlang.ErlConvert;
import junit.framework.TestCase;

/**
 * Abstract class for Erjang JUnit test cases
 * 
 * @author Pavlo Baron <pb@pbit.org>
 *
 */
public abstract class AbstractErjangTestCase extends TestCase {

	protected File file;
	
	public AbstractErjangTestCase(File file) {
		super(file.getName());
		this.file = file;
	}
	
	public AbstractErjangTestCase(String name) {
		super(name);
        file = new File(name);
	}
	
	public void setFile(File file) {
		this.file = file;
        this.setName(file.getName());
	}

    protected EObject processOutput(byte[] bin) {
        /* Parse out the data part - drop the stdout noise.
		 * Search for the substring "DATA::"
		 * This would be easier if we could could use some
		 * indexOf()-like function; unfortunately, converting to String
		 * and back again via getBytes() is lossy in some encodings.
		 */
		int offset = 0;
		int len=bin.length;
		for (int i=0; i < len-6; i++) {
			if (bin[i+0]=='D' &&
				bin[i+1]=='A' &&
				bin[i+2]=='T' &&
				bin[i+3]=='A' &&
				bin[i+4]==':' &&
				bin[i+5]==':')
			{
				offset = i+6;
				break;
			}
		}

		EBinary binOutput = new EBinary(bin, offset, len-offset);
		try {
			return ErlConvert.binary_to_term(binOutput);
		} catch (Throwable e) {
			System.err.println("Undecodable output: "+e);
			System.err.println("Output is: "+binOutput);
			System.err.println("Full output is: "+new EBinary(bin)+"; data offset="+offset);
			return null;
		}
    }
}
