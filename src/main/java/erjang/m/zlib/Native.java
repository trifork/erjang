/** -*- tab-width: 4 -*-
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
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

package erjang.m.zlib;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.zip.Deflater;
import java.util.zip.DeflaterOutputStream;

import erjang.BIF;
import erjang.EBinary;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;

public class Native extends ENative {

	@BIF
	public static EBinary compress(EObject bin) {
		
		EBinary b = bin.testBinary();
		if (b == null) {
			throw ERT.badarg(bin);
		}
		
		Deflater defl = new Deflater();
		BARR bos = new BARR();
		DeflaterOutputStream dos = new DeflaterOutputStream(bos, defl);
		
		try {
			b.writeTo(dos);
			dos.close();
		} catch (IOException e) {
			throw new InternalError("should not happen");
		}
		
		return bos.asBinary();		
	}
	
	static private class BARR extends ByteArrayOutputStream {
		EBinary asBinary() {
			return new EBinary(super.buf, 0, super.count);
		}
	}


}
