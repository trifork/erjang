package org.erlang.jbeam.ops;

import org.erlang.EString;

public class FileDecl extends Stmt {

	private final EString file;

	public FileDecl(EString file) {
		this.file = file;
	}

	public String getFileString() {
		return file.stringValue();
	}

}
