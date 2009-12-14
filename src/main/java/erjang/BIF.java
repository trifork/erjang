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

/**
 * Annotation used to declare an Erlang built-in-function (a.k.a. a BIF).
 * Usually, the BIF must be a public static method.
 * 
 * You can overload the static method, and the compiler will choose the most
 * specific version based on what it can infer about types at compile time. If a
 * statement appears within a match-clause for instance, then the compiler may
 * know something about the structure of your values.
 * 
 * Set ERT.DEBUG2 to true to have the compiler print out "missed opportunity"
 * warnings that can give you hints to which BIFs it may make sense to overload
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface BIF {

	/** name */
	String name() default "__SELFNAME__";

	/** */
	Type type() default Type.DEFAULT;

	public enum Type {
		DEFAULT {
			public boolean export() {
				return true;
			}
		},
		GUARD {
			public boolean export() {
				return false;
			}
		},
		ARITHBIF {
			public boolean export() {
				return false;
			}
		};

		abstract public boolean export();
	}

}
