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

import java.util.ArrayList;
import java.util.List;


public abstract class ESeq extends ECons {

	public ESeq asSeq() {
		return this;
	}

	@Override
	public abstract ESeq tail();

	public abstract ESeq cons(EObject h);

	public EObject[] toArray() {
		List<EObject> out = new ArrayList<EObject>();
		ESeq curr = this;
		while (curr != EMPTY) {
			out.add(curr.head());
			curr = curr.tail();
		}
		return out.toArray(new EObject[out.size()]);
	}

	public int length() {
		int count = 0;
		ESeq curr = this;
		while (curr != EMPTY) {
			count += 1;
			curr = curr.tail();
		}
		return count;
	}

}
