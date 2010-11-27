/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
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

import java.util.Properties;
import java.io.FileInputStream;
import java.io.IOException;

/**
 * Reads a configuration file at system startup, and provides access to
 * the settings therein.
 * The configuration file is a Java 'properties' file (as defined by
 * java.util.Properties), and its location is defined by the
 * Java property "erjang.configfile".
 */
public class ErjangConfig {

	static {
		String configFileName = System.getProperty("erjang.configfile");
		if (configFileName != null) {
			try {
				FileInputStream in = new FileInputStream(configFileName);
				try {
					Properties properties = new Properties();
					properties.load(in);
					for (String name : properties.stringPropertyNames()) {
						if (name.startsWith("erjang.") && System.getProperty(name)==null) { // Don't overwrite e.g. properties given on the command line
							System.setProperty(name, properties.getProperty(name));
						}
					}
				} finally {
					in.close();
				}
			} catch (IOException ioe) {
				System.err.println("Failed to load Erjang properties from "+configFileName+": "+ioe);
			}
		}
	}

	public static void init() {}

	/*==================== Getters ========================================
	 * Like System.getProperty() and e.g. Boolean.getBoolean(), but
	 * ensures that initialization of this class is performed first:
	 */
	public static boolean getBoolean(String propname) {
		return Boolean.getBoolean(propname);
	}

	public static int getInteger(String propname) {
		return Integer.getInteger(propname);
	}

	public static String getString(String propname) {
		return System.getProperty(propname);
	}
	public static String getString(String propname, String fallback) {
		return System.getProperty(propname, fallback);
	}
}
