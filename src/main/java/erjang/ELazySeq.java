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

/**
 * 
 */
public abstract class ELazySeq extends ESeq {

	ESeq value = null;
	
	@Override
	public ESeq cons(EObject h) {
		return value().cons(h);
	}

	/**
	 * @return
	 */
	private ESeq value() {
		if (value == null) {
			value = initialValue();
			
			// Sanity check: nil not allowed
			assert value.testNil()==null;
		}
		return value;
	}

	protected abstract ESeq initialValue();

	@Override
	public ESeq tail() {
		return value().tail();
	}

	@Override
	public EObject head() {
		return value().head();
	}
	
	@Override
	public boolean isNil() {
		return value().isNil();
	}
	
	@Override
	public ESeq testSeq() {
		return value().testSeq();
	}

}
