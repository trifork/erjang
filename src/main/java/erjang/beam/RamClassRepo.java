/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2011 by Trifork
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

import java.util.Map;
import java.util.HashMap;
import java.util.Collection;

/**
 * 
 */
public class RamClassRepo implements ClassRepo {
    private final HashMap<String, byte[]> map = new HashMap();


    public RamClassRepo() {
    }

    /* (non-Javadoc)
     * @see erjang.beam.ClassRepo#store(java.lang.String, byte[])
     */
    @Override
    public void store(String internalName, byte[] data) {
// 	System.err.println("RCR| store(): "+internalName+" => "+data);
	map.put(internalName, data);
    }

    /* (non-Javadoc)
     * @see erjang.beam.ClassRepo#close()
     */
    @Override
    public void close() {
    }

    public byte[] get(String className) {
	String pathName = className.replace('.', '/');
// 	System.err.println("RCR| get(): "+pathName+" => "+map.get(pathName));
	return map.get(pathName);
    }

    public Collection<Map.Entry<String,byte[]> > entrySet() {
	return map.entrySet();
    }

}
