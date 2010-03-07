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
 * See the License for the specific language goversning permissions and
 * limitations under the License.
 **/

package erjang.beam.loader;

import java.io.File;
import java.io.IOException;

import erjang.ETuple;
import erjang.beam.BeamFileData;
import erjang.beam.loader.BeamLoader;
import erjang.beam.repr.ModuleRepr;

public class ErjangBeamDisLoader extends erjang.beam.BeamLoader {
	public ErjangBeamDisLoader() { }

	@Override
	public BeamFileData load(File file) throws IOException {
		ModuleRepr mod = BeamLoader.read(file.getAbsolutePath());
		return mod;
	}

	@Override
	public BeamFileData load(byte[] data) throws IOException {
		ModuleRepr mod = BeamLoader.parse(data);
		return mod;
	}

}
