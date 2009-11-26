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

import java.util.HashMap;
import java.util.Map;



/**
 * An erlang process
 */
public class EProc {
	public static final EObject TAIL_MARKER = new ETailMarker();
	
	public EFun tail;
	public EObject arg0, arg1, arg2, arg3, arg4, arg5, arg6;
	
	/**
	 * @return
	 */
	public EPID self() {
		throw new NotImplemented();
	}
	/**
	 * @param key
	 * @param value
	 * @return
	 */
	
	Map<EObject, EObject> pdict = new HashMap<EObject, EObject>();
	
	public EObject put(EObject key, EObject value) {
		EObject res = pdict.put(key, value);
		if (res == null) return ERT.NIL;
		return res;
	}
	
	public EObject get(EObject key) {
		EObject res = pdict.get(key);
		return (res==null) ? ERT.NIL : res;
	}

	/**
	 * @return
	 */
	public ECons get() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @param key
	 * @return
	 */
	public EObject erase(EObject key) {
		EObject res = pdict.remove(key);
		if (res == null) res = ERT.NIL;
		return res;
	}
	
}

class ETailMarker extends EObject {

	@Override
	int cmp_order() {
		return -1;
	}

	/* (non-Javadoc)
	 * @see erjang.EObject#compare_same(erjang.EObject)
	 */
	@Override
	int compare_same(EObject rhs) {
		if (rhs == EProc.TAIL_MARKER) return 0;
		return -1;
	}
	
}