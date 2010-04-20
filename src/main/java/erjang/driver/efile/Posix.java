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

package erjang.driver.efile;

import java.io.File;

import erjang.EObject;

/**
 * 
 */
public class Posix {

	public static final int EINVAL = 0;
	public static final int ENOMEM = 1;
	public static final int EEXIST = 2;
	public static final int ENOENT = 3;
	public static final int EPERM = 4;
	public static final int EISCONN = 5;
	public static final int EUNKNOWN = 6;
	public static final int ENETUNREACH = 7;
	public static final int EADDRNOTAVAIL = 8;
	public static final int EIO = 9;
	public static final int EOPNOTSUPP = 10;
	public static final int ECONNREFUSED = 11;
	public static final int EMSGSIZE = 12;
	public static final int ENOTCONN = 13;
	public static final int EINTR = 14;
	public static final int EAGAIN = 15;
	public static final int EALREADY = 16;

	private static final String[] err_id = { 
			"einval",
			"enomem", 
			"eexist", 
			"enoent",
			"eperm", 
			"eisconn",
			"eunknown",
			"enetunreach",
			"eaddrnotavail",
			"eio",
			"eopnotsupp",
			"econnrefused",
			"emsgsize",
			"enotconn",
			"eintr",
			"eagain",
			"ealready",
	};


	/**
	 * @param posixErrno
	 * @return
	 */
	public static String errno_id(int posixErrno) {
		if (posixErrno >= err_id.length || posixErrno < 0)
			return "unknown_posix_error";
		else
			return err_id[posixErrno];
	}

	/**
	 * @param file
	 * @return
	 */

	static File CWD = new File(".");

	public static boolean isCWD(String name, File file) {
		if (name.equals("."))
			return true;
		if (file.isAbsolute()) {
			if (file.equals(getCWD()))
				return true;
		} else {
			if (file.getAbsoluteFile().equals(getCWD()))
				return true;
		}
		return false;
	}

	public static File getCWD() {
		return new File(System.getProperty("user.dir")).getAbsoluteFile();
	}

}
