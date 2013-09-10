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

package erjang.m.re;

import java.math.BigInteger;
import java.util.regex.Pattern;

import erjang.EAtom;
import erjang.EBinary;
import erjang.EBitString;
import erjang.ECons;
import erjang.EPseudoTerm;
import erjang.ERT;
import erjang.ESeq;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ETuple4;
import erjang.driver.IO;
import erjang.m.re.Native.Options;

/**
 * Representation of a compiled regular expression
 * 
 * @see http://www.erlang.org/doc/man/re.html mp().
 */
public class ECompiledRE extends ETuple4 {
	final Pattern patt;
	final Options options;
	
	static final EAtom am_re_pattern = EAtom.intern("re_pattern");

	ECompiledRE(Options options, Pattern patt) {
		this.options = options;
		this.patt = patt;
		
		this.elem1 = am_re_pattern;
		this.elem2 = ERT.box(countGroups(patt.pattern()));
		this.elem3 = options.isUnicode() ? ERT.box(1) : ERT.box(0);
		String p = "/" + patt.pattern() + "/" + encode_options();
		this.elem4 = EBinary.make(p.getBytes(IO.UTF8));
	}

	String encode_options() {
		StringBuilder sb = new StringBuilder();
		if (options.global) 
			sb.append('g');
		if (options.unicode) 
			sb.append('u');
		if ((options.flags & Pattern.CASE_INSENSITIVE) == Pattern.CASE_INSENSITIVE)
			sb.append('i');
		if ((options.flags & Pattern.MULTILINE) == Pattern.MULTILINE)
			sb.append('m');
		if ((options.flags & Pattern.DOTALL) == Pattern.DOTALL)
			sb.append('s');
		return sb.toString();
	}
	
	public static ESeq decode_options(byte[] ops) {
		ESeq l = ERT.NIL;
		for (int i = 0; i < ops.length; i++) {
			switch(ops[i]) {
			case 'g': l = l.cons(EAtom.intern("global")); break;
			case 'u': l = l.cons(EAtom.intern("unicode")); break;
			case 'i': l = l.cons(EAtom.intern("caseless")); break;
			case 'f': l = l.cons(EAtom.intern("firstline")); break;
			case 'm': l = l.cons(EAtom.intern("multiline")); break;
			case 's': l = l.cons(EAtom.intern("dotall")); break;
			default: throw new RuntimeException("unsupported regex option '"+((char)ops[i])+"'");
			}
		}
		return l;
	}
	
	private int countGroups(String pattern) {
		int idx = 0;
		int res = 0;
		while ((idx = pattern.indexOf('(', idx)) != -1) {
			if (idx > 0 && pattern.charAt(idx-1) != '\\') {
				res += 1;
			}
			idx += 1;
		}
		return res;
	}
	
}
