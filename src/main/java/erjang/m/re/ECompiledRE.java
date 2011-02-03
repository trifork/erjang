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

import java.util.regex.Pattern;

import erjang.EAtom;
import erjang.EBinary;
import erjang.EBitString;
import erjang.EPseudoTerm;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.m.re.Native.Options;

/**
 * Representation of a compiled regular expression
 * 
 * @see http://www.erlang.org/doc/man/re.html mp().
 */
public class ECompiledRE extends EPseudoTerm {
	final Pattern patt;
	final Options options;

	ECompiledRE(Options options, Pattern patt) {
		this.options = options;
		this.patt = patt;
	}
	
	@Override
	public ETuple testTuple() {
		return new ETuple2(EAtom.intern("re_pattern"), EBinary.EMPTY);
	}
	
	@Override
	public EBinary testBinary() {
		return null;
	}
	
	@Override
	public EBitString testBitString() {
		return null;
	}

	@Override
	public int hashCode() {
		return patt.hashCode(); //TODO: Or be compatible with testTuple()?
	}
	//TODO: equality operators
}
