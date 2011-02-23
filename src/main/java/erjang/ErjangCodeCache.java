/** -*- tab-width: 4 -*-
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

package erjang;

import java.net.URL;

import java.io.IOException;
import java.io.File;
import java.io.ByteArrayOutputStream;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.FileOutputStream;

import java.util.Map;
import java.util.HashMap;
import java.util.Collections;
import java.util.Queue;
import java.util.LinkedList;

import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;

import erjang.beam.Compiler;
import erjang.beam.BeamLoader;
import erjang.beam.JarClassRepo;
import erjang.beam.RamClassRepo;

public class ErjangCodeCache {
    // Config:
    static final String ERJ_CACHE_DIR;
    static final boolean useAsyncPersisting;
    static final boolean useSyncPersisting;
    static final Persister persister;
    static {
	String cacheDir = System.getenv("ERJ_CACHE_DIR");
	if (cacheDir == null) cacheDir = System.getProperty("user.home");
	ERJ_CACHE_DIR = cacheDir;

	String mode = System.getProperty("erjang.codecache.mode");
	if ("async".equals(mode)) {
	    useAsyncPersisting = true;
	    useSyncPersisting = false;
	} else if ("sync".equals(mode)) {
	    useAsyncPersisting = false;
	    useSyncPersisting = true;
	} else if ("off".equals(mode)) {
	    useAsyncPersisting = false;
	    useSyncPersisting = false;
	} else {
	    // TODO: Warn?
	    // Default to 'async':
	    useAsyncPersisting = true;
	    useSyncPersisting = false;
	} // Other values which might make sense: 'read-only', 'existing-only'

	if (useAsyncPersisting) {
	    persister = new Persister();
	    Thread t = new Thread(persister, "Erjang Code Cache Persister");
	    t.setDaemon(true);
	    t.setPriority(Thread.MIN_PRIORITY);
	    t.start();
	} else persister = null;
    }


    private static Map<String, RamClassRepo> cache = Collections.synchronizedMap(new HashMap<String, RamClassRepo>());


    public static EModuleClassLoader getModuleClassLoader(String moduleName, EBinary beam_data, BeamLoader beam_parser) throws IOException {
	long crc = beam_data.crc();
//	crc ^= BIFUtil.all_bif_hash();

	File jarFile = new File(erjdir(), moduleJarFileName(moduleName, crc));

	if (jarFile.exists()) {
	    return new EModuleClassLoader(jarFile.toURI().toURL());
	}
	RamClassRepo repo = new RamClassRepo();

	try {
	    Compiler.compile(beam_parser.load(beam_data.getByteArray()), repo);

	    repo.close();
	    cache.put(moduleName, repo);
	    if (useAsyncPersisting) persister.enqueue(jarFile, repo);
	    else if (useSyncPersisting) persister.persist(jarFile, repo);
	} finally {
	    try {repo.close();
		// jarFile.delete();
	    } catch (Exception e) {}
	}

	return new EModuleClassLoader(jarFile.toURI().toURL(), repo);
    }

    static File erjdir() throws IOException {
	File home = new File(ERJ_CACHE_DIR);

	File dir = new File(home, ".erjang");
	if (!dir.exists()) {
	    if (!dir.mkdirs())
		throw new IOException("cannot create " + dir);

	} else if (!dir.canWrite()) {
	    throw new IOException("cannot write to " + dir);
	}

	return dir;
    }

    public static String moduleJarFileName(String moduleName, long crc) {
	return moduleFileName(moduleName, crc, "jar");
    }
    /*
    static String moduleJarBackupFileName(String moduleName, long crc) {
	return moduleFileName(moduleName, crc, "ja#");
    }
    */

    static String moduleFileName(String moduleName, long crc, String extension) {
	return mangle(moduleName)
	    + "-" + Long.toHexString(crc)
	    + "." + extension;
    }

    /** Mangle string so that the result contains only [a-z0-9_$]. */
    static String mangle(String s) {
	// TODO: Faster handling of the normal case.
	StringBuffer sb = new StringBuffer();
	for (int i=0; i<s.length(); i++) {
	    char c = s.charAt(i);
	    if (('a' <= c && c <= 'z') ||
		('0' <= c && c <= '9') ||
		c == '_')
		sb.append(c);
	    else
		sb.append('$').append(Integer.toHexString(c)).append('$');
	}
	return sb.toString();
    }

    static class PersistRequest { // Just a Pair<File,RamClassRepo>, really.
	final File file;
	final RamClassRepo repo;
	public PersistRequest(File file, RamClassRepo repo) {
	    this.file = file;
	    this.repo = repo;
	}
    }

    static class Persister implements Runnable {
	final Queue<PersistRequest> queue = new LinkedList<PersistRequest>();

	public void run() {
	    while (true) {
		PersistRequest request;
		synchronized (queue) {
		    while ((request = queue.poll()) == null) {
			try { queue.wait(); }
			catch (InterruptedException ie) {}
		    }
		}

		persist(request.file, request.repo);
	    }
	}

	void enqueue(File file, RamClassRepo repo) {
	    synchronized (queue) {
		queue.add(new PersistRequest(file, repo));
		queue.notify();
	    }
	}

	static void persist(File file, RamClassRepo repo) {
	    try {
		File tmpFile = File.createTempFile(file.getName(), "tmp",
						   file.getParentFile());
		JarOutputStream jo = new JarOutputStream(new FileOutputStream(tmpFile));
		for (Map.Entry<String,byte[]> e : repo.entrySet()) {
		    String classFilename = e.getKey() + ".class";
		    byte[] classContents = e.getValue();
		    jo.putNextEntry(new ZipEntry(classFilename));
		    jo.write(classContents);
		    jo.closeEntry();
		}
		jo.close();
		tmpFile.renameTo(file);
	    } catch (IOException ioe) {
		System.err.println("Warning: Failed to store cached module in "+file);
	    }
	}
    }
}
