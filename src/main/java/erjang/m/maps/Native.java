/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2015 by Trifork
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

 package erjang.m.maps;

import com.trifork.clj_ds.PersistentHashMap;

import erjang.BIF;
import erjang.EAtom;
import erjang.EList;
import erjang.EMap;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;

public class Native extends ENative {

	@BIF public static
	EObject get(EObject key, EObject map) {
		EMap self = map.testMap();
		if (self == null) throw ERT.badmap(map);
		return self.get(key);		
	}
	
	@BIF public static
	EObject get(EObject key, EObject map, EObject defaultValue) {
		EMap self = map.testMap();
		if (self == null) throw ERT.badmap(map);
		return self.get(key, defaultValue);		
	}
	
	@BIF public static
	EMap put(EObject key, EObject value, EObject map) {
		EMap self = map.testMap();
		if (self == null) throw ERT.badmap(map);
		return self.put(key, value);
	}
	
	@BIF public static
	EMap update(EObject key, EObject value, EObject map) {
		EMap self = map.testMap();
		if (self == null) throw ERT.badmap(map);
		if (!self.containsKey(key)) throw ERT.badkey(key);
		return self.put(key, value);
	}
	
	@BIF public static
	EMap remove(EObject key, EObject map) {
		EMap self = map.testMap();
		if (self == null) throw ERT.badmap(map);
		return self.remove(key);
	}
	
	@BIF public static
	EAtom is_key(EObject key, EObject map) {
		EMap self = map.testMap();
		if (self == null) throw ERT.badmap(map);
		return self.is_key(key);		
	}
	
	@BIF public static
	ESeq keys(EObject map) {
		EMap self = map.testMap();
		if (self == null) throw ERT.badmap(map);
		return self.keys();
	}
	
	@BIF public static
	ESeq values(EObject map) {
		EMap self = map.testMap();
		if (self == null) throw ERT.badmap(map);
		return self.values();
	}
	
	@BIF public static
	ESeq to_list(EObject map) {
		EMap self = map.testMap();
		if (self == null) throw ERT.badmap(map);
		return self.to_list();
	}
	
	@BIF public static
	EMap merge(EObject map1, EObject map2) {

		EMap self1 = map1.testMap();
		if (self1 == null) throw ERT.badmap(map1);

		EMap self2 = map2.testMap();
		if (self2 == null) throw ERT.badmap(map2);

		return self1.merge(self2);
	}
	
	@SuppressWarnings("unchecked")
	@BIF(name="new") public static
	EMap new_map() {
		return EMap.EMPTY;
	}
	
	
	
	
}