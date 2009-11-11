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

public class EObject {

	public ECons cons(EObject h)
	{
		return new EPair(h, this);
	}
	

	public EAtom asAtom() {
		return null;
	}

	public ETuple asTuple() {
		return null;
	}

	public ESeq asSeq() {
		return null;
	}

	public EPID asPID() {
		return null;
	}

	public int asInt() {
		throw new RuntimeException("cannot convert " + this + " to int");
	}

	public ENumber asNumber() {
		return null;
	}


	public ECons asCons() {
		return null;
	}


	public EInteger asInteger() {
		return null;
	}

}
