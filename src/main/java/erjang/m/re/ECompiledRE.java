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
import erjang.EPseudoTerm;
import erjang.ERT;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ETuple4;
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
		this.elem4 = EBinary.EMPTY;
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
