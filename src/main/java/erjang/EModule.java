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

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigInteger;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;

import kilim.Pausable;

import sun.tools.java.Imports;

import erjang.beam.ClassRepo;
import erjang.beam.Compiler;
import erjang.beam.DirClassRepo;

public abstract class EModule {


	public EModule() {
		// TODO: handle if there is a module of this name already!
		// modules.put(EAtom.intern(this.module_name()), this);

		EModuleManager.setup_module(this);
	}

	/**
	 * @return
	 */
	public abstract String module_name();

	/**
	 * This method is code-generated in subclasses
	 * 
	 * @return the attributes associated with this module
	 */
	public ESeq attributes() {
		return ERT.NIL;
	}

}
