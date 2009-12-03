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

import org.objectweb.asm.Type;
import org.objectweb.asm.commons.Method;

/**
 * 
 */
public class BuiltInFunction {
	public final Type owner;
	public final Method method;
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return method.toString();
	}
	
	public BuiltInFunction(Type owner, Method method) {
		this.owner = owner;
		this.method = method;
	}

	/**
	 * @param m
	 */
	public BuiltInFunction(java.lang.reflect.Method m) {
		this.owner = Type.getType(m.getDeclaringClass());
		this.method = new Method(m.getName(), 
					Type.getType(m.getReturnType()),
					Type.getArgumentTypes(m));
	}

	/**
	 * @return
	 */
	public Type[] getArgumentTypes() {
		return method.getArgumentTypes();
	}

	/**
	 * @return
	 */
	public String getName() {
		return method.getName();
	}

	/**
	 * @return
	 */
	public String getDescriptor() {
		return method.getDescriptor();
	}

	/**
	 * @return
	 */
	public Type getReturnType() {
		return method.getReturnType();
	}

	
}
