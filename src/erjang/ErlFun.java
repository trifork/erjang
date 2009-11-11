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

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface ErlFun {

	/** Name of enclosing module
	 * Value of "__SELFNAME__" means "infer from context" */
	String module() default "__SELFNAME__";
	
	/** Name of function
	 * Value of "__SELFNAME__" means "infer from context" */
	String name() default "__SELFNAME__";

	/** Arity of function
	 * Value of -1 means "infer from arguments" */
	int arity() default -1;
	
	boolean export() default false;
}
