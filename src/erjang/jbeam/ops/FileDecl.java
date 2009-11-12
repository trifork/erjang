package erjang.jbeam.ops;

import erjang.EString;

public class FileDecl extends Stmt {

	private final EString file;

	public FileDecl(EString file) {
		this.file = file;
	}

	public String getFileString() {
		return file.stringValue();
	}

}
