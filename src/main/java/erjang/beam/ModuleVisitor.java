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

package erjang.beam;

import erjang.EAtom;
import erjang.EObject;

/**
 * 
 */
public interface ModuleVisitor {

	void visitModule(EAtom name);

	/** list of {Fun,Arity,Entry} */
	void visitExport(EAtom fun, int arity, int entry);

	/** list of {Atom,Value} */
	void visitAttribute(EAtom att, EObject value);

	/** declare all local functions (happens before any visitFunction) */
	void declareFunction(EAtom fun, int arity, int label);

	/** Visit function */
	FunctionVisitor visitFunction(EAtom name, int arity, int startLabel);

	void visitEnd();

}