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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 * 
 */
public class DirClassRepo implements ClassRepo {

	private File dir;

	public DirClassRepo(File dir) {

		this.dir = dir;
		dir.mkdirs();

	}
	
	/* (non-Javadoc)
	 * @see erjang.beam.ClassRepo#store(java.lang.String, byte[])
	 */
	@Override
	public void store(String internalName, byte[] data) throws IOException {
		
		File out = new File(dir, internalName + ".class");
		File out_dir = out.getParentFile();
		
		out_dir.mkdirs();
		
		FileOutputStream fo = new FileOutputStream(out);
		fo.write(data);
		fo.close();
	}
	
	/* (non-Javadoc)
	 * @see erjang.beam.ClassRepo#close()
	 */
	@Override
	public void close() throws IOException {
	}

}
