/**
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
package erjang.m.unicode;

import erjang.ENative;
import erjang.BIF;
import erjang.EObject;
import erjang.EAtom;
import erjang.ETuple;
import erjang.EBinary;
import erjang.ERT;
import erjang.NotImplemented;
import erjang.CharCollector;

import java.io.CharArrayWriter;
import java.io.IOException;
import java.nio.charset.Charset;

public class Native extends ENative {
	public static EAtom LATIN1_ATOM  = EAtom.intern("latin1");
	public static EAtom UNICODE_ATOM = EAtom.intern("unicode");
	public static EAtom UTF8_ATOM    = EAtom.intern("utf8");
	public static EAtom UTF16_ATOM   = EAtom.intern("utf16");
	public static EAtom UTF32_ATOM   = EAtom.intern("utf32");

	public static EAtom LITTLE_ATOM  = EAtom.intern("little");
	public static EAtom BIG_ATOM     = EAtom.intern("big");

	public static EAtom ERROR_ATOM   = EAtom.intern("error");
	public static EAtom INCOMPLETE_ATOM = EAtom.intern("incomplete");

	@BIF
	public static EObject characters_to_binary(EObject charlist, EObject encodingSpec) {
		Charset encoding = encodingSpecToCharset(encodingSpec);
		if (encoding == null)
			throw ERT.badarg(charlist, encodingSpec);

		CharArrayWriter out = new CharArrayWriter();
		CharCollector collector = new CharCollector(encoding, out);

		try {
			charlist.collectCharList(collector);
		} catch (CharCollector.InvalidElementException e) {
			throw ERT.badarg(charlist, encodingSpec);
		} catch (CharCollector.CollectingException e) {
			EBinary bin = charArrayToUnicodeBinary(out);
			return ETuple.make(ERROR_ATOM, bin, e.restOfInput);
		} catch (IOException e) {
			throw new Error(e); // Not supposed to happen.
		}

		try {
			collector.end();
		} catch (CharCollector.PartialDecodingException e) {
			EBinary bin = charArrayToUnicodeBinary(out);
			return ETuple.make(INCOMPLETE_ATOM, bin);
		} catch (IOException e) {
			throw new Error(e); // Not supposed to happen.
		}

		EBinary bin = charArrayToUnicodeBinary(out);
		return bin;
	}

	protected static EBinary charArrayToUnicodeBinary(CharArrayWriter caw) {
		return charArrayToUnicodeBinary(caw.toCharArray());
	}

	protected static EBinary charArrayToUnicodeBinary(char[] chars) {
		String s = new String(chars);
		try {
			return new EBinary(s.getBytes("UTF-8"));
		} catch (java.io.UnsupportedEncodingException e) {
			throw new Error(e); // Not supposed to happen.
		}
	}

	public static Charset encodingSpecToCharset(EObject encoding) {
		EAtom ea;
		ETuple et;
		if ((ea = encoding.testAtom()) != null) {
			if (ea.equals(LATIN1_ATOM))
				return Charset.forName("ISO-8859-1");
			else if (ea.equals(UNICODE_ATOM) ||
					 ea.equals(UTF8_ATOM))
				return Charset.forName("UTF-8");
			else if (ea.equals(UTF16_ATOM))
				return Charset.forName("UTF-16BE");
		} else if ((et = encoding.testTuple()) != null) {
			EAtom ea2;
			if ((ea  = et.elm(1).testAtom()) != null &&
				(ea2 = et.elm(21).testAtom()) != null) {
				if (ea.equals(UTF16_ATOM)){
					if (ea2.equals(LITTLE_ATOM))
						return Charset.forName("UTF-16LE");
					if (ea2.equals(BIG_ATOM))
						return Charset.forName("UTF-16BE");
				} else if (ea.equals(UTF32_ATOM)) {
					throw new NotImplemented();
				}
			}
		}

		return null;
	}
}
