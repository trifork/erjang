/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2009 by Trifork
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *  
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/


package erjang;


/**
 * 
 */
public class EBitStringBuilder {

	EBitString bs;
	int bpos;
	int extraBits;
	byte[] data;
	
	/**
	 * @param size
	 * @param flags
	 */
	public EBitStringBuilder(int size, int flags) {
		if (flags != 0) throw new NotImplemented("flags="+flags);
		data = new byte[size];
		bs = new EBitString(data, 0, size, 0);
	}

	public EBitStringBuilder(int size, int extra, int flags) {
		if (flags != 0) throw new NotImplemented("flags="+flags);
		data = new byte[size+1];
		bs = new EBitString(data, 0, size, extra);
	}

	/** return bitstring under construction */
	public EBitString bitstring() {
		return bs;
	}
	
	public void put_integer(EObject value, int flags) {
		throw new NotImplemented("val="+value+";flags="+flags);
	}

	public void put_integer(EObject value, int size, int flags) {
		
		if (extraBits != 0)
			throw new NotImplemented();
		
		if (size==8) {
			ESmall sm = value.testSmall();
			if (sm==null) { throw ERT.badarg(value); }
			int val = sm.value;
			
			if ((flags & 0x01) == 0x01) // unsigned
				val &= 0xff;
			
			data[bpos++] = (byte)val;
			return;
		}
		
		throw new NotImplemented("val="+value+";size="+size+";flags="+flags);
	}

	public void put_string(EString str) {
		if (extraBits != 0)
			throw new NotImplemented();
		System.arraycopy(str.data, str.off, data, bpos, str.length());
		bpos += str.length();
	}
	
	public static EBitStringBuilder bs_append(EObject str_or_builder, int extra_size, int flags)
	{
		EBitString ebs = str_or_builder.testBitString();
		if (ebs == null) throw new NotImplemented();
		
		long bitSize = ebs.bitSize() + extra_size;
		int size = (int) (bitSize/8);
		int extra = (int) (bitSize % 8);
		
		EBitStringBuilder result = new EBitStringBuilder(size, extra, flags);
		System.arraycopy(ebs.data, ebs.byteOffset(), result.data, 0, ebs.dataByteSize());
		result.bpos = (int) ebs.bitSize();
		return result;
	}

	public void put_bitstring(EObject str, EAtom how, int flags) {
		EBitString ebs = str.testBitString();
		if (ebs == null) throw new InternalError("bad code gen, arg is "+str.getClass());
		
		throw new NotImplemented();
	}
}
