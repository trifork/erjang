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

package erjang.driver;

import java.util.HashMap;

import erjang.ERT;
import erjang.ESeq;
import erjang.EString;

/** The collection of known drivers.
 */
public class Drivers {
    public static final HashMap<String, EDriver> drivers = new HashMap();

    public static synchronized void register(EDriver driver) {
		drivers.put(driver.driverName(), driver);
    }

    public static synchronized EDriver getDriver(String name) {
		EDriver res = drivers.get(name);
		return res;
    }

	public static ESeq getLoaded() {
		ESeq res = ERT.NIL;
		for (String driver : drivers.keySet()) {
			res = res.cons(EString.fromString(driver));
		}
		return res;
	}
}
